
-module().

-author("John Haugeland <stonecypher@gmail.com>").
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

-description("").

-todo([unit_tests,stochastic_tests,lib_test_rig_integrate,dialyzer,docs,doc_examples,doc_extraction,build_system]).





-testerl_export( { [], _testsuite } ).





-library_requirements([
    {scutil,  161},
    {testerl, 66}
]).





-export([

]).
