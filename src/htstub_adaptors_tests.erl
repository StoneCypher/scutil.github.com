
-module(htstub_adaptors_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").





delay_test_() ->

    TimeFrom = fun({T,_}) -> T*1000 end,

    { "delay/1,2", [
        { "Manual", [
            {"0-1s Timing within 20ms", ?_assert( 
                true =:= sc:is_between(TimeFrom(sc:benchmark(fun() -> htstub_adaptors:delay("hi",0,1000) end)), 0, 1020)
            ) }
        ] }
    ] }.
