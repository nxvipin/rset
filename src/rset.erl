-module(rset).
-author("Vipin Nair <swvist@gmail.com>").

-export([init/2]).

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
