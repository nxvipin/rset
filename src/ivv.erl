-module(ivv).
-author("Vipin Nair <swvist@gmail.com>").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([pack/1,
         unpack/1,
         add/2,
         delete/2,
         max/1,
         contains/2]).

-export_type([ivv/0]).

-type int_set()         :: list(pos_integer()).
-type interval()        :: {nil, nil}
                         | {pos_integer(), nil}
                         | {pos_integer(), pos_integer()}.
-type ivv()             :: list(interval()).


%% -----------------------------------------------------------------------------


-spec pack(int_set()) -> ivv().
pack(Input) ->
    pack(lists:sort(Input), {nil, nil}, []).

pack([], _Interval, IVV) ->
    IVV;
pack([N0], {nil, nil}, IVV) ->
    [{N0, N0} | IVV];
pack([N0], {I0, nil}, IVV) ->
    [{I0, N0} | IVV];
pack([N0, N1 | Rest], {nil, nil}, IVV) when N1 =/= N0+1 ->
    pack([N1 | Rest], {nil, nil}, [{N0, N0} | IVV]);
pack([N0, N1 | Rest], {nil, nil}, IVV) when N1 == N0+1 ->
    pack([N1 | Rest], {N0, nil}, IVV);
pack([N0, N1 | Rest], {I0, nil}, IVV) when N1 =/= N0+1 ->
    pack([N1 | Rest], {nil, nil}, [{I0, N0} | IVV]);
pack([N0, N1 | Rest], {I0, nil}, IVV) when N1 == N0+1 ->
    pack([N1 | Rest], {I0, nil}, IVV).

-spec unpack(ivv()) -> int_set().
unpack(IVV) ->
    unpack(IVV, []).

unpack([], IntegerSet) ->
    lists:flatten(IntegerSet);
unpack([{I0, I1} | Rest], IntegerSet) ->
    unpack(Rest, [lists:seq(I0, I1) | IntegerSet]).

-spec add(pos_integer(), ivv()) -> ivv();
         (list(pos_integer()), ivv()) -> ivv().
add([], IVV) ->
    IVV;

add([Integer|Rest], IVV) ->
    add(Rest, add(Integer, IVV));

add(Integer, IVV) when is_integer(Integer) ->
    %% [TODO]: This is inefficient. Fix this.
    pack([Integer | unpack(IVV)]).

-spec delete(pos_integer(), ivv()) -> ivv().
delete(Integer, IVV) ->
    %% [TODO]: This is inefficient. Fix this.
    pack(lists:delete(Integer, unpack(IVV))).

-spec max(ivv()) -> pos_integer().
max(IVV) ->
    %% [TODO]: This is inefficient. Fix this.
    lists:max(unpack(IVV)).

-spec contains(pos_integer(), ivv()) -> boolean().
contains(Integer, IVV) ->
    %% [TODO]: This is inefficient. Fix this.
    lists:member(Integer, unpack(IVV)).


%% -----------------------------------------------------------------------------

-ifdef(TEST).

ivv_test_() ->
    %% [TODO]: This is a good case for property based testing. Consider that in
    %% the future.
    UNIQSRT = fun(List) -> lists:sort(sets:to_list(sets:from_list(List))) end,

    TestIntSet = UNIQSRT([rand:uniform(50)
                          || _ <- lists:seq(1, rand:uniform(50))]),
    AddElement1 = rand:uniform(100),
    AddElement2 = rand:uniform(100),
    DelElement = lists:nth(rand:uniform(length(TestIntSet)), TestIntSet),

    UnpackT =[?_assertEqual(UNIQSRT(unpack(pack([]))),
                            UNIQSRT([])),
              ?_assertEqual(UNIQSRT(unpack(pack([1]))),
                            UNIQSRT([1])),
              ?_assertEqual(UNIQSRT(unpack(pack(TestIntSet))),
                            UNIQSRT(TestIntSet))],

    PackT = [?_assertEqual(UNIQSRT(pack(unpack([]))),
                           UNIQSRT([])),
             ?_assertEqual(UNIQSRT(pack(unpack([{1,1}]))),
                           UNIQSRT([{1,1}])),
             ?_assertEqual(UNIQSRT(pack(unpack(pack(TestIntSet)))),
                           UNIQSRT(pack(TestIntSet)))],

    AddT = [?_assertEqual(UNIQSRT(unpack(add(AddElement1, pack([])))),
                          UNIQSRT([AddElement1 | []])),
            ?_assertEqual(UNIQSRT(unpack(add(AddElement1, pack(TestIntSet)))),
                          UNIQSRT([AddElement1 | TestIntSet]))],

    AddMultipleT =
        [?_assertEqual(
            UNIQSRT(add(AddElement2, add(AddElement1, pack(TestIntSet)))),
            UNIQSRT(add([AddElement1, AddElement2], pack(TestIntSet))))],

    DelT =
        [?_assertEqual(UNIQSRT(unpack(delete(DelElement, pack([DelElement])))),
                       UNIQSRT(lists:delete(DelElement, [DelElement]))),
         ?_assertEqual(UNIQSRT(unpack(delete(DelElement, pack(TestIntSet)))),
                       UNIQSRT(lists:delete(DelElement, TestIntSet)))],

    ContainsT = (fun(Elements, IVV) ->
                         [?_assert(contains(Val, IVV)) || Val <- Elements]
                 end)(TestIntSet, pack(TestIntSet)),

    UnpackT ++ PackT ++ AddT ++ AddMultipleT ++ DelT ++ ContainsT.

-endif.
