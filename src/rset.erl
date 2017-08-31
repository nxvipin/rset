-module(rset).
-author("Vipin Nair <swvist@gmail.com>").

-export([init/2,
         add/2]).

-record(element, {update    :: update(),
                  timestamp :: timestamp(),
                  replica   :: replica()}).

-record(rset, {elements     :: elements(),
               timestamp    :: timestamp(),
               ivvmap       :: ivvmap(),
               repinfo      :: {this_replica(),
                                other_replicas(),
                                all_replicas()}}).

-type update()         :: any().
-type timestamp()      :: non_neg_integer().

-type replica()        :: any().
-type this_replica()   :: replica().       %% Current replica
-type other_replicas() :: list(replica()). %% All replicas except `this` replica
-type all_replicas()   :: list(replica()). %% All participating replicas

-type ivvmap()         :: #{replica() := ivv:ivv()}.
-type element()        :: #element{}.
-type elements()       :: list(element()).


%% -----------------------------------------------------------------------------


init(ThisReplica, AllReplicas) ->
    OtherReplicas = lists:delete(ThisReplica, AllReplicas),
    init(ThisReplica, OtherReplicas, AllReplicas).

init(ThisReplica, OtherReplicas, AllReplicas) ->
    #rset{elements=[],
          timestamp=0,
          ivvmap=maps:from_list([{Rep, []} || Rep <- AllReplicas]),
          repinfo={ThisReplica, OtherReplicas, AllReplicas}}.

add({add_downstream, {_MsgVal, MsgTimestamp, MsgSourceReplica}=Element},
    #rset{elements=Elements,
          ivvmap=IVVMap}=Rset0) ->
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

    %% Downstream operation at this replica, which is the source replica.
    add({add_downstream, Element}, Rset#rset{timestamp=Timestamp+1}).
    %% [TODO]: Send downstream add operation to all replicas other than the
    %% source replica ideally handled by a messaging middleware.
