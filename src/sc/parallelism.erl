




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

    Computers      = lists:flatten(lists:duplicate(JobsPerNode, Nodes)),
    TaggedWorkload = lists:zip(lists:seq(1,length(Workload)), Workload),
    WorkOut        = [],
    WorkDone       = [],

    map_reduce_worker(Function, TaggedWorkload, Computers, WorkOut, WorkDone).





%% @private

map_reduce_worker(_Function, [],             _Computers, [],      WorkDone) -> {_,Out} = lists:unzip(lists:keysort(1,WorkDone)), Out;                               % no work left, no work out?  done.
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
