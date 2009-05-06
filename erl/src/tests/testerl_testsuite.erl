
-module(testerl_testsuite).
-author("John Haugeland - stonecypher@gmail.com").
-webpage("").
-publicsvn("").

-svnversion("$Id$").
-svnhead("$HeadURL$").

-behaviour(testerl_testsuite_behaviour).

-description("").





-export([
    run/2
]).





test_must_equal() ->

    Ret = fun(X) -> X end,

    [   testerl:must_equal("Check that 5=5 returns pass",       Ret, [5],    5),
        testerl:must_equal("Check that true=true returns pass", Ret, [true], true)
    ].





test_must_fail() ->

    Ret = fun(X) -> X end,

    [   testerl:must_fail("Check that 5=7 returns fail",         testerl:must_equal("Force fail", Ret, [5], 7)),
        testerl:must_fail("Check that fail on 5=5 returns fail", testerl:must_fail("Force fail", testerl:must_equal("Force pass", Ret, [5], 5)))
    ].





run(_Hooks, _Options) ->

    lists:flatten([
        {pass,"Test suite is called by framework", []},
        testerl:assemble("Test must equal", test_must_equal()),
        testerl:assemble("Test must fail", test_must_fail())
    ]).
