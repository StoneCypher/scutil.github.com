
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2011 John Haugeland
%% @version $Revision$
%% @doc scutil test set.

-module(sc_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").





extrema_test_() ->

    { "Extrema tests", [
        {"1,2,3,4",    ?_assert( {1,4}      =:= sc:extrema( [1,2,3,4]  ) ) },
        {"-1,-2,-3",   ?_assert( {-3,-1}    =:= sc:extrema( [-1,-2,-3] ) ) },
        {"-1.1,0,1.1", ?_assert( {-1.1,1.1} =:= sc:extrema( [-1.1,1.1] ) ) },
        {"a,b,c",      ?_assert( {a,c}      =:= sc:extrema( [a,b,c]    ) ) },
        {"1,a,{}",     ?_assert( {1,{}}     =:= sc:extrema( [1,a,{}]   ) ) },
        {"1",          ?_assert( {1,1}      =:= sc:extrema( [1]        ) ) },

        {"[] error",   ?_assertError(badarg, sc:extrema([]) ) }

    ] }.

