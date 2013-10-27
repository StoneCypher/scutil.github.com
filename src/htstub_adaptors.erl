
-module(htstub_adaptors).





-export([

    delay/1,
      delay/2,
      delay/3,

    test/0

]).





delay(X) ->

    delay(X, {500,2500}).





delay(Result, FixedTime) when is_integer(FixedTime) ->

    receive 
    after FixedTime ->
        Result
    end.





delay(Result, Min, Max) when Min =< Max ->

    receive 
    after sc:rand_between(Min, Max) ->
        Result
    end.





%% @doc (not testworthy) Runs the test suite in terse form. ``` c("wherever/htstub_adaptors.erl").
%% {ok,htstub_adaptors}
%%
%% 2> c("wherever/htstub_adaptors_tests.erl").
%% {ok,htstub_adaptors_tests}
%%
%% 3> htstub_adaptors:test().
%%   All 1 tests passed.
%% ok'''
%%
%% @since 2.0.3

-spec test() -> 
    ok   | 
    error.

test() ->

    eunit:test(htstub_adaptors).
