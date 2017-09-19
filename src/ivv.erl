-module(ivv).
-author("Vipin Nair <swvist@gmail.com>").

-include("rset.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Operations on IVV
-export([pack/1,
         unpack/1,
         add/2,
         delete/2,
         max/1,
         contains/2,
         union/2,
         diff/2]).

%% Operations on IVV Map
-export([minit/1,
         madd/2,
         madd/3,
         munion/2,
         mcontains/3]).

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

-spec union(ivv(), ivv()) -> ivv().
union(IVV1, IVV2) ->
    pack(sets:to_list(sets:from_list(unpack(IVV1) ++ unpack(IVV2)))).

-spec diff(ivv(), ivv()) -> list(pos_integer()).
diff(IVV1, IVV2) ->
    lists:subtract(unpack(IVV1), unpack(IVV2)).

-spec minit(list(any())) -> ivvmap().
minit(Keys) when is_list(Keys) ->
    maps:from_list([{Key, []} || Key <- Keys]).

-spec madd(list({pos_integer(), any()}), ivvmap()) -> ivvmap().
madd([], IVV) ->
    IVV;
madd([{Integer, Key} | Rest], IVV0) ->
    IVV = madd(Integer, Key, IVV0),
    madd(Rest, IVV).

-spec madd(pos_integer(), any(), ivvmap()) -> ivvmap().
madd(Integer, Key, IVVMap) ->
    #{Key := IVV} = IVVMap,
    IVVMap#{Key := add(Integer, IVV)}.

-spec munion(ivvmap(), ivvmap()) -> ivvmap().
munion(IVVMap1, IVVMap2) ->
    maps:map(fun(K, IVV1) ->
                     IVV2 = maps:get(K, IVVMap2),
                     ivv:union(IVV1, IVV2)
             end, IVVMap1).

-spec mcontains(pos_integer(), any(), ivvmap()) -> boolean().
mcontains(Integer, Key, IVVMap) ->
    #{Key := IVV} = IVVMap,
    contains(Integer, IVV).


%% -----------------------------------------------------------------------------

-ifdef(TEST).

ivv_test_() ->
    %% [TODO]: This is a good case for property based testing. Consider that in
    %% the future.
    UNIQSRT = fun(List) -> lists:sort(sets:to_list(sets:from_list(List))) end,

    TestIntSet = UNIQSRT([rand:uniform(50)
                          || _ <- lists:seq(1, rand:uniform(50))]),
    TestIntSet2 = UNIQSRT([rand:uniform(50)
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

    UnionT =
        [?_assertEqual(UNIQSRT(union([], [])),
                       UNIQSRT([])),
         ?_assertEqual(UNIQSRT(pack(unpack(union(pack(TestIntSet),
                                                 pack(TestIntSet2))))),
                       UNIQSRT(union(pack(TestIntSet), pack(TestIntSet2))))],

    UnionContainsT = (fun(Elements, IVV) ->
                              [?_assert(contains(Val, IVV)) || Val <- Elements]
                      end)(TestIntSet ++ TestIntSet2,
                           union(pack(TestIntSet), pack(TestIntSet2))),

    DiffT = [?_assertEqual(diff([], []), []),
             ?_assertEqual(diff(pack(TestIntSet), pack(TestIntSet2)),
                           lists:subtract(TestIntSet, TestIntSet2))],

    UnpackT ++ PackT ++ AddT ++ AddMultipleT ++ DelT ++ ContainsT ++ UnionT
        ++ UnionContainsT ++ DiffT.

-endif.
