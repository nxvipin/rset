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
    #rset{elements=Elements, ivvmap=IVVMap0}=Rset0) ->
    %% Downstream operation of Add. We have not see this element before so we
    %% add it and update our element list and ivv of the source replica.
    Rset = case not ivvmap_contains({MsgTimestamp, MsgSourceReplica}, IVVMap0) of
               true ->
                   IVVMap = add_ivvmap({MsgTimestamp, MsgSourceReplica}, IVVMap0),
                   Rset0#rset{elements = [Element | Elements],
                              ivvmap = IVVMap};
               false ->
                   Rset0
           end,
    {Element, Rset};

add(Val, #rset{repinfo={SourceReplica, _, _}, timestamp=Timestamp0}=Rset) ->
    %% A value was added at this replica. We create an element() from this value
    %% and propagate it downstream.
    Timestamp = Timestamp0 + 1,
    Element = {Val, Timestamp, SourceReplica},

    %% Downstream operation at this replica, which is the source replica. The
    %% downstream operation is propagated to other replicas by the the `replica`
    %% server using the return value of this function.
    add(Element, Rset#rset{timestamp=Timestamp}).

delete({#{}=DelIVVMap, DelTimestamp, DelSourceReplica}=DelElement,
       #rset{elements=Elements0, ivvmap=IVVMap0}=Rset0) ->
    Rset = case not ivvmap_contains({DelTimestamp, DelSourceReplica}, IVVMap0)
           of true ->
                   %% Record that we have seen this delete operation from the
                   %% SourceReplica
                   IVVMap1 = add_ivvmap({DelTimestamp, DelSourceReplica}, IVVMap0),
                   %% Remove elements based on DelIVVMap
                   RemovedElements = lists:filter(
                                   fun({_, Timestamp, Replica}) ->
                                           RepIVV = maps:get(Replica, DelIVVMap),
                                           ivv:contains(Timestamp, RepIVV)
                                   end,
                                   Elements0),
                   Elements = lists:subtract(Elements0, RemovedElements),
                   %% Record elements that should have been deleted in the IVVMap
                   IVVMap = union_ivvmap(IVVMap1, DelIVVMap),
                   Rset0#rset{elements=Elements, ivvmap=IVVMap};
               false ->
                   Rset0
           end,
    {DelElement, Rset};

delete(Value, #rset{elements=Elements, timestamp=Timestamp0,
                    repinfo={SourceReplica, _, _}}=Rset) ->
    Timestamp = Timestamp0 + 1,
    IVVMap = init_ivvmap(Rset),
    DelElements = lists:filter(fun({Val, _, _}) ->
                                       Val == Value
                               end, Elements),
    DelIVVMap = add_ivvmap(DelElements, IVVMap),
    DelElement = {DelIVVMap, Timestamp, SourceReplica},
    delete(DelElement, Rset#rset{timestamp=Timestamp}).

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

ivvmap_contains({Timestamp, Replica}, IVVMap) ->
    #{Replica := ReplicaIVV}=IVVMap,
    ivv:contains(Timestamp, ReplicaIVV).
