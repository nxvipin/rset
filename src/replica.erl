-module(replica).
-author("Vipin Nair <swvist@gmail.com>").

-include("rset.hrl").
-behaviour(gen_server).

%% API
-export([create/1,
         create/2,
         add/2,
         delete/2,
         contains/2,
         elements/1,
         ivvmap/1,
         ivv/2,
         sync/1,
         sync_all/1]).

% Internal API
-export([start_link/2]).

%% Gen server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% -----------------------------------------------------------------------------


-record(state, {rset   :: rset(),
                dlog   :: map(),
                ackmap :: ackivvmap()}).

-spec create(list(replica() | replica_name())) -> {ok, list(pid())}.
create(AllReplicas0) ->
    AllReplicas = validate_replicas(AllReplicas0),
    rset_sup:create_replica(AllReplicas).

-spec create(list(replica_name()), node()) -> {ok, list(pid())};
            (replica_name(), list(node())) -> {ok, list(pid())}.

create(ReplicaNames, Node) when is_list(ReplicaNames), is_atom(Node) ->
    AllReplicas = validate_replicas([{Replica, Node} || Replica <- ReplicaNames]),
    rset_sup:create_replica(AllReplicas);

create(ReplicaName, NodeList) when is_atom(ReplicaName), is_list(NodeList) ->
    AllReplicas = validate_replicas([{ReplicaName, Node} || Node <- NodeList]),
    rset_sup:create_replica(AllReplicas).

start_link({ThisReplicaName, _ThisReplicaNode}=ThisReplica, AllReplicas) ->
    gen_server:start_link({local, ThisReplicaName}, ?MODULE,
                          [ThisReplica, AllReplicas], []).

add(Replica, Value) ->
    gen_server:call(Replica, {add, Value}).

delete(Replica, Value) ->
    gen_server:call(Replica, {delete, Value}).

send_downstream(Replica, {#{}=_, _,_}=DelElement) ->
    gen_server:cast(Replica, {delete_downstream, DelElement});

send_downstream(Replica, {_,_,_}=AddElement) ->
    gen_server:cast(Replica, {add_downstream, AddElement}).


contains(Replica, Value) ->
    gen_server:call(Replica, {'contains?', Value}).

elements(Replica) ->
    gen_server:call(Replica, elements).

ivvmap(Replica) ->
    gen_server:call(Replica, ivvmap).

ivv(Replica, DownstreamReplica) ->
    gen_server:call(Replica, {ivv, DownstreamReplica}).

sync(Replica) ->
    gen_server:cast(Replica, sync).

sync_all(Replica) ->
    gen_server:cast(Replica, sync_all).

add_ack(SourceReplica, DownstreamReplica, {_,_,_}=Element) ->
    %% Ack a downstream add message received by the
    gen_server:cast(SourceReplica, {ack_add_downstream, SourceReplica,
                                    DownstreamReplica, Element}).

del_ack(SourceReplica, DownstreamReplica, {#{}=_,_,_}=DelElement) ->
    %% Ack a downstream delete message received by the
    gen_server:cast(SourceReplica, {ack_delete_downstream, SourceReplica,
                                    DownstreamReplica, DelElement}).

init([ThisReplica, AllReplicas]) ->
    {ok, #state{rset = rset:init(ThisReplica, AllReplicas),
                dlog = #{},
                ackmap = ivv:minit(AllReplicas)}}.

handle_call({add, Value}, From,
            #state{rset=#rset{repinfo={ThisReplica, OtherReplicas, _}}=Rset0,
                   dlog=Dlog,
                   ackmap=AckMap}=State) ->
    %% Insert the value into the set
    {{Value, Timestamp, ThisReplica}=Element, Rset} = rset:add(Value, Rset0),
    lager:debug("Add operation received from Client: ~p "
                "ThisReplica: ~p Timestamp: ~p Value:~p",
                [From, ThisReplica, Timestamp, Value]),
    %% Send the element to other downstream replicas asynchronously
    [send_downstream(Replica, Element) || Replica <- OtherReplicas],
    {reply, {ok, Element}, State#state{
                             %% Update the set in the state
                             rset=Rset,
                             %% Add the downstream entry to the log
                             dlog=Dlog#{Timestamp => Element},
                             %% Trivially ack the reciept of the downstream
                             %% message at ThisReplica
                             ackmap = ivv:madd(Timestamp, ThisReplica, AckMap)}};

handle_call({delete, Value}, From,
            #state{rset=#rset{repinfo={ThisReplica, OtherReplicas, _}}=Rset0,
                   dlog=Dlog,
                   ackmap=AckMap}=State) ->
    %% Delete the element from the set
    {{_, DelTimestamp, _}=DelElement, Rset} = rset:delete(Value, Rset0),
    lager:debug("Delete operation received from Client: ~p ThisReplica: ~p "
                "DelIVVMap:~p", [From, ThisReplica, DelElement]),
    %% Send the delete element to other downstream replicas asychronously
    [send_downstream(Replica, DelElement) || Replica <- OtherReplicas],
    {reply, {ok, DelElement},
     State#state{
       %% Update the set in the state
       rset=Rset,
       %% Add the downstream entry to the log
       dlog=Dlog#{DelTimestamp => DelElement},
       %% Trivially ack the reciept of the downstream
       %% message at ThisReplica
       ackmap = ivv:madd(DelTimestamp, ThisReplica, AckMap)}};

handle_call({'contains?', Value}, _From, #state{rset=Rset}=State) ->
    {reply, {ok, rset:contains(Value, Rset)}, State};

handle_call(elements, _From, #state{rset=Rset}=State) ->
    Elements = rset:elements(Rset),
    {reply, {ok, Elements}, State};

handle_call(ivvmap, _From, #state{rset=Rset}=State) ->
    {reply, {ok, rset:ivvmap(Rset)}, State};

handle_call({ivv, Replica}, _From, #state{rset=Rset}=State) ->
    {reply, {ok, rset:ivv(Replica, Rset)}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_downstream, {_Value, _Timestamp, SourceReplica}=Element},
            #state{rset=#rset{repinfo={ThisReplica, _, _}}=Rset0}=State) ->
    lager:debug("Add downstream operation received from SourceReplica: ~p "
                "ThisReplica: ~p Element: ~p",
                [SourceReplica, ThisReplica, Element]),
    {Element, Rset} = rset:add(Element, Rset0),
    add_ack(SourceReplica, ThisReplica, Element),
    {noreply, State#state{rset=Rset}};

handle_cast({ack_add_downstream, ThisReplica, DownstreamReplica,
             {_, Timestamp, _}=Element},
            #state{rset=#rset{repinfo={ThisReplica, _, _}},
                   ackmap=AckMap}=State) ->
    lager:debug("Add downstream ACK received at ThisReplica: ~p "
                "from DownstreamReplica: ~p for Element: ~p",
                [ThisReplica, DownstreamReplica, Element]),

    {noreply, State#state{
                %% Record that Downstream Replica has received this downstream
                %% operation
                ackmap=ivv:madd(Timestamp, DownstreamReplica, AckMap)}};

handle_cast({delete_downstream, {#{}=_DelIVVMap, _Timestamp, SourceReplica}=DelElement},
            #state{rset=#rset{repinfo={ThisReplica, _, _}}=Rset0}=State) ->
    lager:debug("Delete downstream operation received from SourceReplica: ~p "
                "ThisReplica: ~p DelElement: ~p",
                [SourceReplica, ThisReplica, DelElement]),
    {DelElement, Rset} = rset:delete(DelElement, Rset0),
    del_ack(SourceReplica, ThisReplica, DelElement),
    {noreply, State#state{rset=Rset}};

handle_cast({ack_delete_downstream, ThisReplica, DownstreamReplica,
             {_, DelTimestamp, _}=DelElement},
            #state{rset=#rset{repinfo={ThisReplica, _, _}},
                   ackmap=AckMap}=State) ->
    lager:debug("Delete downstream ACK received at ThisReplica: ~p "
                "from DownstreamReplica: ~p for DelElement: ~p",
                [ThisReplica, DownstreamReplica, DelElement]),
    {noreply, State#state{
                %% Record that Downstream Replica has received this downstream
                %% operation
                ackmap=ivv:madd(DelTimestamp, DownstreamReplica, AckMap)}};

handle_cast(sync, #state{rset=#rset{repinfo={ThisReplica, OtherReplicas, _}}=Rset,
                         dlog=Dlog,
                         ackmap=AckMap}=State) ->
    %% Send all the elements that have not been acknowledged by downstream
    %% replicas.
    %% [TODO]: This is ugly, fix it.
    ThisReplicaInterval = rset:ivv(ThisReplica, Rset),
    lists:map(
      fun(Rep) ->
              RepAck = maps:get(Rep, AckMap),
              UnackedTimestamps = ivv:diff(ThisReplicaInterval, RepAck),
              lager:debug("Elements: ~p from SourceReplica: ~p have not been "
                          "acknowledged by DownstreamReplica: ~p~n",
                          [UnackedTimestamps, ThisReplica, Rep]),
              lists:map(
                fun(UnackedTimestamp) ->
                        UnackedMsg = maps:get(UnackedTimestamp, Dlog),
                        send_downstream(Rep, UnackedMsg)
                end,
                UnackedTimestamps)
      end,
      OtherReplicas),
    {noreply, State};

handle_cast(sync_all, #state{rset=#rset{repinfo={_, _, AllReplicas}}}=State) ->
    [sync(Replica) || Replica <- AllReplicas],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% -----------------------------------------------------------------------------

-spec validate_replicas(list(replica() | replica_name())) -> list(replica()).
validate_replicas(Input) ->
    lists:map(
      fun({ReplicaName, ReplicaNode}) when is_atom(ReplicaName),
                                           is_atom(ReplicaNode) ->
              {ReplicaName, ReplicaNode};
         (ReplicaName) when is_atom(ReplicaName) ->
              {ReplicaName, node()}
      end, Input).
