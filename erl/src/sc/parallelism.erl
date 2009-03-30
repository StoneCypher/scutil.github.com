
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 254 $
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-03-15 13:47:01 -0600 (Sun, 15 Mar 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc.parallelism).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: parallelism.erl 254 2009-03-22 06:41:06Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/src/sc/parallelism.erl $").
-svn_revision("$Revision: 254 $").

-description("Functions for parallelization across nodes in a virtual machine (eg map_reduce).").

-testerl_export( { [], sc_parallelism_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [
  
    map_reduce/2,
      map_reduce/3,
      map_reduce/4

] ).





%% @equiv map_reduce(Function, Workload, 1, nodes())

map_reduce(Function, Workload) ->

    map_reduce(Function, Workload, 1, nodes() ).



%% @equiv map_reduce(Function, Workload, JobsPerNode, nodes())

map_reduce(Function, Workload, JobsPerNode) ->

    map_reduce(Function, Workload, JobsPerNode, nodes() ).



%% @spec map_reduce(Function::function(), Workload::list(), JobsPerNode::positive_integer(), Nodes::list()) -> list()

%% @doc {@section Parallelism} Takes a workload, a function, a count of jobs per node and a node list, and distributes work on demand to available nodes until fulfilled.  Results are provided in the order the workload was provided, regardless of in what order they are completed or received. ```
%% '''<span style="color:red">TODO: add crash handling behavior, progress querying behavior</span>

map_reduce(Function, Workload, JobsPerNode, Nodes) ->

    Computers      = .lists:flatten(.lists:duplicate(JobsPerNode, Nodes)),
    TaggedWorkload = .lists:zip(.lists:seq(1,length(Workload)), Workload),
    WorkOut        = [],
    WorkDone       = [],

    map_reduce_worker(Function, TaggedWorkload, Computers, WorkOut, WorkDone).





%% @private

map_reduce_worker(_Function, [],             _Computers, [],      WorkDone) -> {_,Out} = .lists:unzip(.lists:keysort(1,WorkDone)), Out;                               % no work left, no work out?  done.
map_reduce_worker( Function, [],              Computers, WorkOut, WorkDone) -> map_reduce_wait_for_work(Function, [],             Computers, WorkOut, WorkDone);    % no work left, work out?  wait.
map_reduce_worker( Function, TaggedWorkload,  [],        WorkOut, WorkDone) -> map_reduce_wait_for_work(Function, TaggedWorkload, [],        WorkOut, WorkDone);    % work left, no computers left?  wait.
map_reduce_worker( Function, TaggedWorkload,  Computers, WorkOut, WorkDone) -> map_reduce_do_work(      Function, TaggedWorkload, Computers, WorkOut, WorkDone).    % work left, computers left?  do work.





%% @private
map_reduce_wait_for_work(Function, TaggedWorkload, Computers, WorkOut, WorkDone) ->

    receive

        { work_done, Computer, Tag, Result } ->
            map_reduce_worker(Function, TaggedWorkload, Computers ++ [Computer], WorkOut -- [Tag], WorkDone ++ [{Tag,Result}] )

    end.





%% @private
map_reduce_do_work(Function, [{Tag,Workload}|RemWorkload], [Computer|RemComputers], WorkOut, WorkDone) ->

    spawn(

        Computer,

        fun(Who,What,Which,With) ->
            Who ! {work_done, node(), Which, apply(What,With) }
        end,

        [ self(), Function, Tag, Workload ]

    ),

    map_reduce_worker(Function, RemWorkload, RemComputers, WorkOut++[Tag], WorkDone).





%receive_all_answers(MessageList, ProcessList) when length(MessageList) /= length(ProcessList) -> { error, message_list_and_process_list_must_have_same_length };
%receive_all_answers(MessageList, ProcessList) when is_list(MessageList), is_list(ProcessList) ->
%
%    IDs = lists:seq(1,length(MessageList)),
%    [ spawn(?MODULE, receive_all_answers_worker, [self(), ID, Message, Process]) || { ID, Message, Process } <- lists:zip3(IDs, MessageList, ProcessList) ],
%    wait_on_answers(IDs, []).
%
%
%
%
%
%receive_all_answers_worker(Collector, Id, Message, Process) ->
%
%    Process ! Message,
%    receive
%        X -> Collector ! { an_answer, Id, X }
%    end.
%
%
%
%
%
%wait_on_answers([],        Work) -> {_, Out} = lists:keysort(1,Work), Out;
%wait_on_answers(Remaining, Work) ->
%
%    receive
%        { an_answer, AnswerID, Answer } -> wait_on_answers(Remaining -- [AnswerID], Work++{AnswerID,Answer});
%        Other                           -> { error, { misunderstood_result, Other }, { work_completed, Work }, { work_remaining, Remaining } }
%    end.
