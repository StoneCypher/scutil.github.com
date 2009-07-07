
-module(|_testsuite).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-twitter({"JohnHaugeland", "http://twitter.com/JohnHaugeland"}).
-twitter({"ScUtil", "http://twitter.com/ScUtil"}).
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").
-svn_date("$Date$").

-description("Testset for ()").

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
    run/2
] ).





test_group() ->

    T1 = testerl:must_equal("Descriptive Test", fun what:ever/1, [Args], {expected,output}),

    [ T1 ].





run(_Hooks, _Options) ->

    [   testerl:assemble("Test group name", test_group())
    ].
