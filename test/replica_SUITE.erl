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
-export([test_sanity/1,
         test_add_wins/1]).


%% -----------------------------------------------------------------------------


all() -> [test_sanity, test_add_wins].

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


test_add_wins(Config) ->
    TestReplicas = test_replicas(Config),
    RandReplica1 = test_random_replica(Config),
    RandReplica2 = test_random_replica(Config, RandReplica1),

    %% Create `test` set replicas on all nodes
    replica:create(TestReplicas),

    %% Add initial elements
    {ok, {xyz, T1, _}} = replica:add(RandReplica1, xyz),
    {ok, {xyz, T2, _}} = replica:add(RandReplica2, xyz),
    wait({RandReplica1, T1}, TestReplicas),
    wait({RandReplica2, T2}, TestReplicas),
    is_everywhere(xyz, TestReplicas),

    %% Disconnect the replicas.
    disconnect(RandReplica1, RandReplica2),

    %% Add into one set and delete from another set at the same timestamp
    {ok, {_, TF, _}} = replica:add(RandReplica1, xyz),
    {ok, {_, TF, _}} = replica:delete(RandReplica2, xyz),

    %% Connect the replicas
    connect(RandReplica1, RandReplica2),

    %% Sync the replicas
    replica:sync(RandReplica1),
    replica:sync(RandReplica2),

    %% Ensure that everything is synced
    wait({RandReplica1, TF}, TestReplicas),
    wait({RandReplica2, TF}, TestReplicas),

    %% Verify that add wins
    is_everywhere(xyz, TestReplicas),

    %% Issue one more delete and none of the replicas must have it
    {ok, {_, T5, _}} = replica:delete(RandReplica2, xyz),
    wait({RandReplica2, T5}, TestReplicas),
    is_nowhere(xyz, TestReplicas),

    ok.


%% -----------------------------------------------------------------------------


test_replicas(Config) ->
    TestSet = ?config(test_set, Config),
    TestNodes = ?config(test_nodes, Config),
    [{TestSet, TestNode} || TestNode <- TestNodes].

test_random_replica(Config) ->
    Replicas = test_replicas(Config),
    lists:nth(rand:uniform(length(Replicas)), Replicas).

test_random_replica(Config, Except) ->
    case test_random_replica(Config) of
        Except -> test_random_replica(Config, Except);
        Replica -> Replica
    end.



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

contains_all_or_none(_Value, [], _Boolean) ->
    ok;
contains_all_or_none(Value, [Replica|Rest] = _AllReplicas, Boolean) ->
    {ok, Boolean} = replica:contains(Replica, Value),
    contains_all_or_none(Value, Rest, Boolean).

is_everywhere(Value, AllReplicas) ->
    contains_all_or_none(Value, AllReplicas, true).

is_nowhere(Value, AllReplicas) ->
    contains_all_or_none(Value, AllReplicas, false).

disconnect({_, Node1}, {_, Node2}) ->
	ct_rpc:call(Node1, erlang, disconnect_node, [Node2]),
	ct_rpc:call(Node1, erlang, set_cookie, [Node2, wrong_cookie]).

connect({_, Node1}, {_, Node2}) ->
	ct_rpc:call(Node1, erlang, disconnect_node, [Node2]),
	Cookie = ct_rpc:call(Node1, erlang, get_cookie, []),
	ct_rpc:call(Node1, erlang, set_cookie, [Node2, Cookie]).
