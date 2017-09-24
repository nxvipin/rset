%% Replicated Set
-record(rset, {elements      :: elements(),
               timestamp     :: timestamp(),
               ivvmap        :: ivvmap(),
               repinfo       :: {this_replica(),
                                 other_replicas(),
                                 all_replicas()}}).

-type value()           :: any().
-type timestamp()       :: non_neg_integer().

-type replica_name()    :: atom().
-type replica_node()    :: atom().
-type replica()         :: {replica_name(), replica_node()}.
-type this_replica()    :: replica().       %% Current replica
-type other_replicas()  :: list(replica()). %% All replicas except `this` replica
-type all_replicas()    :: list(replica()). %% All participating replicas
-type replica_ack_set() :: list(replica()). %% List of replicas that have
                                            %% acknowledged a particular
                                            %% downstream op.

-type ivvmap()          :: #{replica() := ivv:ivv()}.
-type delivvmap()       :: ivvmap().
-type ackivvmap()       :: ivvmap().

-type element()         :: {value(), timestamp(), replica()}.
-type elements()        :: list(element()).
-type del_element()     :: {delivvmap(), timestamp(), replica()}.
-type downstream_op()   :: element() | del_element().

%% Replicated Set
-type rset()            :: #rset{}.

%% Downstream Operation Log
-type dlog()            :: #{timestamp() := downstream_op()}.
