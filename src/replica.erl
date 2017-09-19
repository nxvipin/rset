-module(replica).
-author("Vipin Nair <swvist@gmail.com>").

-include("rset.hrl").
-behaviour(gen_server).

%% API
-export([create/1,
         add/2,
         delete/2,
         elements/1]).

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


create(AllReplicas) ->
    rset_sup:create_replica(AllReplicas).


start_link(ThisReplica, AllReplicas) ->
    gen_server:start_link({local, ThisReplica}, ?MODULE,
                          [ThisReplica, AllReplicas], []).


add(Replica, {_,_,_}=Element) ->
    %% Downstream operations are async. This is called by the source replica to
    %% propagate downstream add operation.
    gen_server:cast(Replica, {add_downstream, Element});

add(Replica, Value) ->
    gen_server:call(Replica, {add, Value}).

delete(Replica, {#{}=_, _,_}=DelElement) ->
    %% Downstream operations are async. This is called by the source replica to
    %% propagate downstream delete operation.
    gen_server:cast(Replica, {delete_downstream, DelElement});

delete(Replica, Value) ->
    gen_server:call(Replica, {delete, Value}).

elements(Replica) ->
    gen_server:call(Replica, elements).

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
    [add(Replica, Element) || Replica <- OtherReplicas],
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
    [delete(Replica, DelElement) || Replica <- OtherReplicas],
    {reply, {ok, DelElement},
     State#state{
       %% Update the set in the state
       rset=Rset,
       %% Add the downstream entry to the log
       dlog=Dlog#{DelTimestamp => DelElement},
       %% Trivially ack the reciept of the downstream
       %% message at ThisReplica
       ackmap = ivv:madd(DelTimestamp, ThisReplica, AckMap)}};

handle_call(elements, _From, State) ->
    Elements = rset:elements(State),
    {reply, {ok, Elements}, State};

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

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
