-module(ebils_pipe).

-export([receive_result/1]).

-type found() :: {ok, {non_neg_integer(), non_neg_integer()}, pid()}.

-spec receive_result(Timeout :: non_neg_integer()) -> timeout | found().
receive_result(Timeout) ->
    receive
        {ok, Found, Worker} ->
            {ok, Found, Worker}
    after Timeout ->
        timeout
    end.
