-module(replica_SUITE).
-author("Vipin Nair <swvist@gmail.com>").

-include_lib("rset/include/rset.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Boilerplate
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Rset tests
-export([test_sanity/1]).


%% -----------------------------------------------------------------------------


all() -> [test_sanity].

init_per_suite(Config) ->
    Nodes = rset_test_utils:test_nodes(),
    rset_test_utils:start_node(Nodes),
    [{test_nodes, Nodes} | Config].

end_per_suite(Config) ->
    TestNodes = ?config(test_nodes, Config),
    rset_test_utils:stop_node(TestNodes),
    ok.

init_per_testcase(_TestCase, Config) ->
    TestNodes = ?config(test_nodes, Config),
    rset_test_utils:start_rset(TestNodes),
    TestSet = list_to_atom("set-" ++ binary_to_list(base64:encode(crypto:strong_rand_bytes(5)))),
    [{test_set, TestSet} | Config].

end_per_testcase(_TestCase, Config) ->
    TestNodes = ?config(test_nodes, Config),
    rset_test_utils:stop_rset(TestNodes),
    Config.

test_sanity(Config) ->
    TestReplicas = test_replicas(Config),
    RandReplica = test_random_replica(Config),

    %% Create `test` set replicas on all nodes
    replica:create(TestReplicas),
    %% Set must be empty when initialized
    {ok, []} = replica:elements(RandReplica),
    %% Add to the set must succeed
    {ok, {xyz, AddTimestamp, RandReplica}} = replica:add(RandReplica, xyz),
    %% The set must contain the added element
    {ok, true} =replica:contains(RandReplica, xyz),
    %% Wait for all replicas to get this message
    wait({RandReplica, AddTimestamp}, TestReplicas),
    %% All replicas must contain added element
    [{ok, true} = replica:contains(Replica, xyz) || Replica <- TestReplicas],

    %% Delete the added element from the set
    {ok, {_, DelTimestamp, _}} = replica:delete(RandReplica, xyz),
    %% The set must not contain the deleted element
    {ok, false} =replica:contains(RandReplica, xyz),
    %% Wait for all replicas to get this message
    wait({RandReplica, DelTimestamp}, TestReplicas),
    %% The element must be deleted from all the replicas
    [{ok, false} = replica:contains(Replica, xyz) || Replica <- TestReplicas],

    ok.


%% -----------------------------------------------------------------------------

test_replicas(Config) ->
    TestSet = ?config(test_set, Config),
    TestNodes = ?config(test_nodes, Config),
    [{TestSet, TestNode} || TestNode <- TestNodes].

test_random_replica(Config) ->
    Replicas = test_replicas(Config),
    lists:nth(rand:uniform(length(Replicas)), Replicas).


wait({SourceReplica, SourceMsgTimestamp}, AllReplicas) ->
    %% Ensure that all replicas have the SourceTimestamp in the IVV
    %% corresponding to the SourceReplica in their respective IVVMaps. This
    %% means that all replicas have received the downstream message
    %% corresponding to the SourceMsgTimestamp
    AllContains = lists:map(fun(Replica) ->
                                    {ok, RepIVVMap} = replica:ivvmap(Replica),
                                    RepSrcIVV = ivv:mget(SourceReplica, RepIVVMap),
                                    ivv:contains(SourceMsgTimestamp, RepSrcIVV)
                            end, AllReplicas),
    case lists:all(fun(Contains) -> Contains == true end, AllContains) of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait({SourceReplica, SourceMsgTimestamp}, AllReplicas)
    end.
