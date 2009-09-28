
-module(sc_pert).

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

-description("A module for computing results from PERT chart graph data").

-todo([unit_tests,stochastic_tests,lib_test_rig_integrate,docs,doc_examples,doc_extraction]).





-testerl_export( { [], sc_pert_testsuite } ).





-library_requirements([
    {scutil,  161},
    {testerl, 66}
]).





-record(sc_pert, {
    name,
    start_milestone,
    end_milestone
}).





-record(sc_pert_activity, {
    name,
    origin,
    destination,
    duration
}).





-record(sc_pert_milestone, {
    name
}).





-export([

    blank/0,

    add_milestone/2,
    remove_milestone/2,

    add_activity/2,
    remove_activity/2,

    critical_path/1,
    slack/1

]).




blank() -> todo.

add_milestone(Pert, Milestone) when is_record(Pert, sc_pert), is_record(Milestone, sc_pert_milestone) -> todo.
remove_milestone(Pert, MilestoneName) when is_record(Pert, sc_pert) -> todo.

add_activity(Pert, Activity) when is_record(Pert, sc_pert), is_record(Activity, sc_pert_activity) -> todo.
remove_activity(Pert, ActivityName) when is_record(Pert, sc_pert) -> todo.

critical_path(Pert) -> critical_path(Pert, Pert#sc_pert.start_milestone, Pert#sc_pert.end_milestone).
critical_path(Pert, From, To) -> todo.

slack(Pert) -> todo.
