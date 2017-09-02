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