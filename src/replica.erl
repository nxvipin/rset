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
    {ok, rset:init(ThisReplica, AllReplicas)}.

handle_call({add, Value}, From,
            #rset{repinfo = {ThisReplica, OtherReplicas, _}}=State0) ->

    {{Value, Timestamp, ThisReplica}=Element, State} = rset:add(Value, State0),
    lager:debug("Add operation received from Client: ~p "
                "ThisReplica: ~p Timestamp: ~p Value:~p",
                [From, ThisReplica, Timestamp, Value]),
    [add(Replica, Element) || Replica <- OtherReplicas],
    {reply, {ok, Element}, State};

handle_call({delete, Value}, From,
            #rset{repinfo = {ThisReplica, OtherReplicas, _}}=State0) ->
    {DelElement, State} = rset:delete(Value, State0),
    lager:debug("Delete operation received from Client: ~p ThisReplica: ~p "
                "DelIVVMap:~p", [From, ThisReplica, DelElement]),
    [delete(Replica, DelElement) || Replica <- OtherReplicas],
    {reply, {ok, DelElement}, State};

handle_call(elements, _From, State) ->
    Elements = rset:elements(State),
    {reply, {ok, Elements}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_downstream, {_Value, _Timestamp, SourceReplica}=Element},
            #rset{repinfo={ThisReplica, _, _}}=State0) ->
    lager:debug("Add downstream operation received from SourceReplica: ~p "
                "ThisReplica: ~p Element: ~p",
                [SourceReplica, ThisReplica, Element]),
    {Element, State} = rset:add(Element, State0),
    add_ack(SourceReplica, ThisReplica, Element),
    {noreply, State};

handle_cast({ack_add_downstream, ThisReplica, DownstreamReplica, Element},
            #rset{repinfo={ThisReplica, _, _}}=State) ->
    lager:debug("Add downstream ACK received at ThisReplica: ~p "
                "from DownstreamReplica: ~p for Element: ~p",
                [ThisReplica, DownstreamReplica, Element]),
    {noreply, State};

handle_cast({delete_downstream, {#{}=_DelIVVMap, _Timestamp, SourceReplica}=DelElement},
            #rset{repinfo={ThisReplica, _, _}}=State0) ->
    lager:debug("Delete downstream operation received from SourceReplica: ~p "
                "ThisReplica: ~p DelElement: ~p",
                [SourceReplica, ThisReplica, DelElement]),
    {DelElement, State} = rset:delete(DelElement, State0),
    del_ack(SourceReplica, ThisReplica, DelElement),
    {noreply, State};

handle_cast({ack_delete_downstream, ThisReplica, DownstreamReplica, DelIVVMap},
            #rset{repinfo={ThisReplica, _, _}}=State) ->
    lager:debug("Delete downstream ACK received at ThisReplica: ~p "
                "from DownstreamReplica: ~p for DelIVVMap: ~p",
                [ThisReplica, DownstreamReplica, DelIVVMap]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
