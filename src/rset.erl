-module(rset).
-author("Vipin Nair <swvist@gmail.com>").

-include("rset.hrl").

%% Public API
-export([init/2,
         add/2]).


%% -----------------------------------------------------------------------------


init(ThisReplica, AllReplicas) ->
    OtherReplicas = lists:delete(ThisReplica, AllReplicas),
    init(ThisReplica, OtherReplicas, AllReplicas).

init(ThisReplica, OtherReplicas, AllReplicas) ->
    #rset{elements=[],
          timestamp=0,
          ivvmap=maps:from_list([{Rep, []} || Rep <- AllReplicas]),
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
