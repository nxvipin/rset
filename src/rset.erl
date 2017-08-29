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
               this         :: replica()}).

-type update()      :: any().
-type timestamp()   :: non_neg_integer().
-type replica()     :: any().
-type ivvmap()      :: #{replica() := ivv:ivv()}.
-type element()     :: #element{}.
-type elements()    :: list(element()).


%% -----------------------------------------------------------------------------


init(ThisReplica, AllReplicas) ->
    #rset{elements=[],
          timestamp=0,
          ivvmap=maps:from_list([{Rep, []} || Rep <- AllReplicas]),
          this=ThisReplica}.

add({add_downstream, {_MsgVal, MsgTimestamp, MsgSourceReplica}=Element},
    #rset{elements=Elements,
          ivvmap=IVVMap}=Rset) ->
    %% Downstream operation of Add. We have not see this element before so we
    %% add it and update our element list and ivv of the source replica.
    #{MsgSourceReplica := MsgSourceReplicaIVV0}=IVVMap,
    case not ivv:contains(MsgTimestamp, MsgSourceReplicaIVV0) of
         true ->
            #{MsgSourceReplica := MsgSourceReplicaIVV0}=IVVMap,
            MsgSourceReplicaIVV = ivv:add(MsgTimestamp, MsgSourceReplicaIVV0),
            Rset#rset{
              elements = [Element | Elements],
              ivvmap = IVVMap#{MsgSourceReplica := MsgSourceReplicaIVV}};
        false ->
            Rset
    end;

add(Val, #rset{this=SourceReplica, timestamp=Timestamp}=Rset) ->
    %% A value was added at this replica. We create an element() from this value
    %% and propagate it downstream.
    Element = {Val, Timestamp, SourceReplica},

    %% Downstream operation at this replica, which is the source replica.
    add({add_downstream, Element}, Rset#rset{timestamp=Timestamp+1}).
    %% [TODO]: Send downstream add operation to all replicas other than the
    %% source replica ideally handled by a messaging middleware.
