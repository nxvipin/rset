-module(replica).
-author("Vipin Nair <swvist@gmail.com>").

-include("rset.hrl").
-behaviour(gen_server).

%% API
-export([start_link/2,
         add/2]).

%% Gen server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% -----------------------------------------------------------------------------


start_link(ThisReplica, AllReplicas) ->
    gen_server:start_link({local, ThisReplica}, ?MODULE,
                          [ThisReplica, AllReplicas], []).

add(Replica, Value) ->
    gen_server:call(Replica, {add, Value}).

init([ThisReplica, AllReplicas]) ->
    {ok, rset:init(ThisReplica, AllReplicas)}.

handle_call({add, {_, _, _}=Element}, _From, State0) ->
    {Element, State} = rset:add(Element, State0),
    {reply, {ok, Element}, State};

handle_call({add, Value}, _From,
            #rset{repinfo = {_, OtherReplicas, _}}=State0) ->
    {Element, State} = rset:add(Value, State0),
    [add(Replica, Element) || Replica <- OtherReplicas],
    {reply, {ok, Element}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
