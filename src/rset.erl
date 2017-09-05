-module(rset).
-author("Vipin Nair <swvist@gmail.com>").

-include("rset.hrl").

%% Public API
-export([init/2,
         add/2,
         delete/2,
         elements/1]).


%% -----------------------------------------------------------------------------


init(ThisReplica, AllReplicas) ->
    OtherReplicas = lists:delete(ThisReplica, AllReplicas),
    init(ThisReplica, OtherReplicas, AllReplicas).

init(ThisReplica, OtherReplicas, AllReplicas) ->
    #rset{elements=[],
          timestamp=0,
          ivvmap=init_ivvmap(AllReplicas),
          repinfo={ThisReplica, OtherReplicas, AllReplicas}}.

add({_MsgVal, MsgTimestamp, MsgSourceReplica}=Element,
    #rset{elements=Elements, ivvmap=IVVMap}=Rset0) ->
    %% Downstream operation of Add. We have not see this element before so we
    %% add it and update our element list and ivv of the source replica.
    #{MsgSourceReplica := MsgSourceReplicaIVV0}=IVVMap,
    Rset = case not ivv:contains(MsgTimestamp, MsgSourceReplicaIVV0) of
               true ->
                   #{MsgSourceReplica := MsgSourceReplicaIVV0}=IVVMap,
                   MsgSourceReplicaIVV = ivv:add(MsgTimestamp,
                                                 MsgSourceReplicaIVV0),
                   Rset0#rset{
                     elements = [Element | Elements],
                     ivvmap = IVVMap#{MsgSourceReplica := MsgSourceReplicaIVV}};
               false ->
                   Rset0
           end,
    {Element, Rset};

add(Val, #rset{repinfo={SourceReplica, _, _}, timestamp=Timestamp}=Rset) ->
    %% A value was added at this replica. We create an element() from this value
    %% and propagate it downstream.
    Element = {Val, Timestamp, SourceReplica},

    %% Downstream operation at this replica, which is the source replica. The
    %% downstream operation is propagated to other replicas by the the `replica`
    %% server using the return value of this function.
    add(Element, Rset#rset{timestamp=Timestamp+1}).

delete(#{}=DelIVVMap, #rset{elements=Elements0,
                            ivvmap=IVVMap0}=Rset) ->
    DelElements = lists:filter(
                    fun({_, Timestamp, Replica}) ->
                            RepIVV = maps:get(Replica, DelIVVMap),
                            ivv:contains(Timestamp, RepIVV)
                    end,
                    Elements0),
    Elements = lists:subtract(Elements0, DelElements),
    IVVMap = union_ivvmap(IVVMap0, DelIVVMap),
    {DelIVVMap, Rset#rset{elements=Elements,
                          ivvmap=IVVMap}};

delete(Value, #rset{elements=Elements}=Rset) ->
    IVVMap = init_ivvmap(Rset),
    DelElements = lists:filter(fun({Val, _, _}) ->
                                       Val == Value
                               end, Elements),
    delete(add_ivvmap(DelElements, IVVMap), Rset).

elements(#rset{elements=Elements}) ->
    [Val || {Val, _, _} <- Elements].


%% -----------------------------------------------------------------------------


init_ivvmap(#rset{repinfo={_,_,AllReplicas}}) ->
    init_ivvmap(AllReplicas);
init_ivvmap(AllReplicas) when is_list(AllReplicas) ->
    maps:from_list([{Rep, []} || Rep <- AllReplicas]).

add_ivvmap([], IVVMap) ->
    IVVMap;
add_ivvmap([{Timestamp, Replica} | Rest], IVVMap) ->
    add_ivvmap(Rest, add_ivvmap({Timestamp, Replica}, IVVMap));
add_ivvmap([{_Value, Timestamp, Replica} | Rest], IVVMap) ->
    add_ivvmap(Rest, add_ivvmap({Timestamp, Replica}, IVVMap));
add_ivvmap({Timestamp, Replica}, IVVMap) ->
    #{Replica := IVV} = IVVMap,
    IVVMap#{Replica := ivv:add(Timestamp, IVV)}.

union_ivvmap(IVVMap1, IVVMap2) ->
    maps:map(fun(K, IVV1) ->
                     IVV2 = maps:get(K, IVVMap2),
                     ivv:union(IVV1, IVV2)
             end, IVVMap1).
