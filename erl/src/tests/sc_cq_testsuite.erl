
-module(sc_cq_testsuite).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-twitter("JohnHaugeland").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Testset for sc_cq (circular queue)").





-testerl_export( this_is_a_testsuite ).   %  no_testsuite, this_is_a_testsuite or { [], foo_testsuite }
-library_requirements( [] ).





-export( [
    run/2
] ).





test_read_write() ->

    Q  = sc_cq:create(5,{1,2,3,4,5}),

    T1 = testerl:must_equal("sc_cq:peek/1 pre-write", fun sc_cq:peek/1, [Q],   {value,1}),
    T2 = testerl:must_equal("sc_cq:peek/2 pre-write", fun sc_cq:peek/2, [3,Q], {value,3}),

    sc_cq:write(aa, Q),    sc_cq:write(ba, Q),
    sc_cq:write(ca, Q),    sc_cq:write(da, Q),
    sc_cq:write(ea, Q),

    T3 = testerl:must_equal("sc_cq:peek/1 post-write", fun sc_cq:peek/1, [Q],   {value,aa}),
    T4 = testerl:must_equal("sc_cq:peek/2 post-write", fun sc_cq:peek/2, [3,Q], {value,ca}),

    [ T1, T2, T3, T4 ].





run(_Hooks, _Options) ->

    [   testerl:assemble("Read and Write", test_read_write())
    ].
