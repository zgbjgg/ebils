-module(ebils_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_load_default/1,
    test_load_with_name/1,
    test_search_notfound/1,
    test_search_success/1,
    test_get_from_search/1,
    test_append_load_runtime/1,
    test_unload/1,
    test_reload/1]).

-define(KEY_SIZE, 20).
-define(PIECE_LAST, "|0|0").
-define(PATTERN, "\n").

all() ->
    [test_load_default,
     test_load_with_name,
     test_search_notfound,
     test_search_success,
     test_get_from_search,
     test_append_load_runtime,
     test_unload,
     test_reload].

test_load_default([{big_ben, BigBen}]) ->
    Loaded = ebils:load(BigBen, <<"\n">>),
    ?assertEqual(true, Loaded).

test_load_with_name([{big_ben, BigBen}]) ->
    Loaded = ebils:load(test, BigBen, <<"\n">>),
    ?assertEqual(true, Loaded).

test_search_notfound([{big_ben, BigBen}]) ->
    true = ebils:load(BigBen, <<"\n">>),
    NotFound = ebils:search(<<"ZZZZZZZZZ:-(ZZZZZZZZ">>),
    ?assertEqual({error, notfound}, NotFound).

test_search_success([{big_ben, BigBen}]) ->
    true = ebils:load(BigBen, <<"\n">>),
    {ok, Found, _Pid} = ebils:search(<<"ZZZZZZZZZZZZZZZZZZZZ">>),
    ?assertEqual({225,20}, Found).

test_get_from_search([{big_ben, BigBen}]) ->
    true = ebils:load(BigBen, <<"\n">>),
    {ok, Found, Pid} = ebils:search(<<"ZZZZZZZZZZZZZZZZZZZZ">>),
    {ok, Value} = ebils:fetch(Pid, {get, {found, Found}, 4}),
    ?assertEqual(<<"ZZZZZZZZZZZZZZZZZZZZ|0|0">>, Value).

test_append_load_runtime([{big_ben, BigBen}]) ->
    true = ebils:load(BigBen, <<"\n">>),
    % create another big ben :-P
    OtherBigBen = [ random_big_ben_piece() || _ <- lists:seq(1, 1000) ],
    true = ebils:load(list_to_binary(OtherBigBen), <<"\n">>),
    % now search again for key
    {ok, Found, _Pid} = ebils:search(<<"ZZZZZZZZZZZZZZZZZZZZ">>),
    ?assertEqual({225,20}, Found).

test_unload([{big_ben, BigBen}]) ->
    true = ebils:load(BigBen, <<"\n">>),
    ok = ebils:unload(),
    ?assertEqual(undefined, ets:info(ebils)).

test_reload([{big_ben, BigBen}]) ->
    true = ebils:load(BigBen, <<"\n">>),
    {ok, Found, Pid} = ebils:search(<<"ZZZZZZZZZZZZZZZZZZZZ">>),
    ok = ebils:reload(BigBen, <<"\n">>),
    {ok, Found, Pid1} = ebils:search(<<"ZZZZZZZZZZZZZZZZZZZZ">>),
    ?assertEqual({225,20}, Found).

init_per_testcase(_, _Config) ->
    % create a huge binary
    BigBen = [ random_big_ben_piece() || _ <- lists:seq(1, 1000) ] ++
        [ big_ben_piece() ],
    [{big_ben, list_to_binary(BigBen)}].

end_per_testcase(_, _Config) ->
    ok.

% INTERNAL

random_big_ben_piece() ->
    {A,B,C} = erlang:timestamp(),
    random:seed({A,B,C}),
    Num = fun() ->
        case random:uniform(3) of
            1 -> random:uniform($9-$1) + $1;
            2 -> random:uniform($9-$1) + $1;
            3 -> random:uniform($9-$1) + $1
        end
    end,
    Xs = [ Num() || _ <- lists:seq(1, ?KEY_SIZE) ],
    lists:flatten(Xs) ++ ?PIECE_LAST ++ ?PATTERN.

big_ben_piece() ->
    Xs = lists:duplicate(?KEY_SIZE, "Z"),
    lists:flatten(Xs) ++ ?PIECE_LAST ++ ?PATTERN.
