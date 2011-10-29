
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
    name            = 'sc_pert:Unnamed',
    start_milestone = 'sc_pert:No Milestone',
    end_milestone   = 'sc_pert:No Milestone',
    milestones      = [],
    activities      = []
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

    new/0,
      new/1,
      new/3,
      new/4,
      new/5,

    set_name/2,

    add_milestone/2,
    remove_milestone/2,
    set_start_milestone/2,
    set_end_milestone/2,

    add_activity/2,
    remove_activity/2,

    critical_path/1,
    slack/1

]).




%% @since Version 414

new() ->

    #sc_pert{}.



%% @since Version 414

new(Name) ->

    #sc_pert{name=Name}.



%% @since Version 414

new(Name, Start, End) -> #sc_pert{name=Name, start_milestone=Start, end_milestone=End}.



%% @since Version 414

new(Name, Start, End, Milestones) ->

    #sc_pert{name=Name, start_milestone=Start, end_milestone=End, milestones=Milestones}.



%% @since Version 414

new(Name, Start, End, Milestones, Activities) ->

    #sc_pert{name=Name, start_milestone=Start, end_milestone=End, milestones=Milestones, activities=Activities}.





%% @since Version 414

set_name(Pert, _NewName) when is_record(Pert, sc_pert) ->

    todo.





%% @since Version 413

add_milestone(Pert, Milestone) when is_record(Pert, sc_pert), is_record(Milestone, sc_pert_milestone) ->

    Pert#sc_pert{milestones=[Milestone]++Pert#sc_pert.milestones}.





%% @since Version 413

remove_milestone(Pert, _MilestoneName) when is_record(Pert, sc_pert) ->

    todo.





%% @since Version 414

set_start_milestone(Pert, MilestoneName) when is_record(Pert, sc_pert) ->

    Pert#sc_pert{start_milestone=MilestoneName}.





%% @since Version 414

set_end_milestone(Pert, MilestoneName) when is_record(Pert, sc_pert) ->

    Pert#sc_pert{end_milestone=MilestoneName}.





%% @since Version 413

add_activity(Pert, Activity) when is_record(Pert, sc_pert), is_record(Activity, sc_pert_activity) ->

    Pert#sc_pert{activities=[Activity]++Pert#sc_pert.activities}.





%% @since Version 413

remove_activity(Pert, _ActivityName) when is_record(Pert, sc_pert) ->

    todo.





%% @since Version 413

critical_path(Pert) when is_record(Pert, sc_pert) ->

    critical_path(Pert, Pert#sc_pert.start_milestone, Pert#sc_pert.end_milestone).



%% @since Version 413

critical_path(Pert, _From, _To) when is_record(Pert, sc_pert) ->

    todo.





%% @since Version 413

slack(Pert) when is_record(Pert, sc_pert) ->

    todo.
