-module(rset_test_utils).

-export([test_nodes/0,
         test_nodes/1,
         start_node/1,
         stop_node/1,
         start_rset/1,
         stop_rset/1]).

test_nodes() ->
    test_nodes(3).

test_nodes(N) ->
    [list_to_atom(NodeStr) ||
        NodeStr <- ["N" ++ integer_to_list(X) ++ "@127.0.0.1"
                    || X<- lists:seq(1, N)]].
start_node([]) ->
    ok;

start_node([Node | Rest]) ->
    start_node(Node),
    start_node(Rest);

start_node(NodeAtom) ->
    [Name, Host] = lists:map(fun erlang:list_to_atom/1,
                             string:tokens(atom_to_list(NodeAtom), "@")),

    SlaveArg = "-pa ../../../default/lib/*/ebin ../../../test/lib/*/ebin -config ../../../../app.config",
    case slave:start(Host, Name, SlaveArg) of
        {ok, _} ->
            NodeAtom;
        {error,timeout} ->
            exit("Unable to start slave, timeout");
        {error, {already_running, _}} ->
            %% Already running, ignore.
            NodeAtom
    end.

stop_node([]) ->
    ok;

stop_node([Node | Rest]) ->
    slave:stop(Node),
    stop_node(Rest).

start_rset([]) ->
    ok;

start_rset([Node | Rest]) ->
    start_rset(Node),
    start_rset(Rest);

start_rset(NodeAtom) ->
    rpc:call(NodeAtom, application, ensure_all_started, [rset]).


stop_rset([]) ->
    ok;

stop_rset([Node | Rest]) ->
    stop_rset(Node),
    stop_rset(Rest);

stop_rset(NodeAtom) ->
    rpc:call(NodeAtom, application, stop, [rset]).
