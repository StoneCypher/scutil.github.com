
-module(sc_cq_testsuite).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-twitter({"JohnHaugeland", "http://twitter.com/JohnHaugeland"}).
-twitter({"ScUtil", "http://twitter.com/ScUtil"}).
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Testset for sc_cq (circular queue)").

-library_requirements([
    {scutil,  161},
    {testerl, 66}
]).





-testerl_export( this_is_a_testsuite ).   %  no_testsuite, this_is_a_testsuite or { [], foo_testsuite }
-testerl_sufficient_for(0).

-testerl_bindings([
    { {sc_cq,peek,1}, test_peek_1 },
    { {sc_cq,peek,2}, test_peek_2 }
]).





-export( [

    run/2,

    test_peek_1/0,
    test_peek_2/0

] ).





test_peek_1() ->

    InitiallyEmpty = sc_cq:create(5),
    T1             = testerl:must_equal("initially empty peek/1",            fun sc_cq:peek/1, [InitiallyEmpty], empty),

    sc_cq:write(a, InitiallyEmpty),
    T2             = testerl:must_equal("initially empty post-write peek/1", fun sc_cq:peek/1, [InitiallyEmpty], {value,a}),

    [ sc_cq:write(X, InitiallyEmpty) || X <- [b,c,d,e,f] ],
    T3             = testerl:must_equal("initially empty post-wrap peek/1",  fun sc_cq:peek/1, [InitiallyEmpty], {value,b}),

    [T1, T2, T3].





test_peek_2() ->

    [].





test_peek() ->


    Q  = sc_cq:create(5,{1,2,3,4,5}),

    T1 = testerl:must_equal("sc_cq:peek/1 pre-write", fun sc_cq:peek/1, [Q],   {value,1}),
    T2 = testerl:must_equal("sc_cq:peek/2 pre-write", fun sc_cq:peek/2, [3,Q], {value,3}),

    sc_cq:write(aa, Q),    sc_cq:write(ba, Q),
    sc_cq:write(ca, Q),    sc_cq:write(da, Q),
    sc_cq:write(ea, Q),

    T3 = testerl:must_equal("sc_cq:peek/1 post-write", fun sc_cq:peek/1, [Q],   {value,aa}),
    T4 = testerl:must_equal("sc_cq:peek/2 post-write", fun sc_cq:peek/2, [3,Q], {value,ca}),


    R  = sc_cq:create(5),

    T5 = testerl:must_equal("sc_cq:peek/1 on empty",   fun sc_cq:peek/1, [R],   empty),
    T6 = testerl:must_equal("sc_cq:peek/2 on empty",   fun sc_cq:peek/2, [3,R], empty),


    [ T1, T2, T3, T4, T5, T6 ].





run(_Hooks, _Options) ->

    [   testerl:assemble("Peek/1,2", test_peek())
    ].
