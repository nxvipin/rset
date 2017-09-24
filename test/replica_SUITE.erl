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
    Config.

end_per_testcase(_TestCase, Config) ->
    TestNodes = ?config(test_nodes, Config),
    rset_test_utils:stop_rset(TestNodes),
    Config.

test_sanity(Config) ->
    [N1|_Rest]= TestNodes = ?config(test_nodes, Config),
    %% Create `test` set replicas on all nodes
    replica:create(test, TestNodes),
    %% Set must be empty when initialized
    {ok, []} = replica:elements({test, N1}),
    %% Add to the set must succeed
    {ok, _} = replica:add({test, N1}, xyz),
    %% The set must contain the added element
    {ok, [xyz]} =replica:elements({test, N1}),

    ok.
