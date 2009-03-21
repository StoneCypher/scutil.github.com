




%% @type timestamp() = {Megaseconds::non_negative_integer(), Seconds::non_negative_integer(), MicroSeconds::non_negative_integer()}.

%% @spec diff_timestamp(A::timestamp(), B::timestamp()) -> float()

%% @doc {@section Utility} Returns the difference, in seconds as a float, between two erlang timestamps as returned by `erlang:now()'.  Negative differences are returned if the latter timestamp `B' is earlier than the former timestamp `A'.  ```1> A = now().
%% {1232,947675,340000}
%%
%% 2> B = now().
%% {1232,947679,412000}
%%
%% 3> scutil:diff_timestamp(A,B).
%% 4.072
%%
%% 4> scutil:diff_timestamp(B,A).
%% -4.072'''

%% @since Version 9

diff_timestamp({AM,AS,AU}, {BM, BS, BU}) ->

    ((BM-AM) * 1000000) + (BS-AS) + ((BU-AU)/1000000).





%% @since Version 38

benchmark(Fun, Args) ->

    Start  = now(),
    Result = apply(Fun, Args),
    End    = now(),

    { diff_timestamp(Start,End), Result }.





benchmark(Module, Func, Args) ->

    Start  = now(),
    Result = apply(Module, Func, Args),
    End    = now(),

    { diff_timestamp(Start,End), Result }.





%% @private

% Handler must be no_handler_pid or { handler, PID [, idhandle] }

call_after_worker(MS, Func, Args, Handler) ->

    receive 
        % Nothing

    after MS ->


        case Func of

            { Module, FuncName } ->
                Result = apply(Module, FuncName, Args);

            FuncName ->
                Result = apply(FuncName, Args)

        end,


        case Handler of

            { handler, PID, Handle } ->
                PID ! { call_after_result, Result, Handle };

            { handler, PID } ->
                PID ! { call_after_result, Result };

            no_handler_pid ->
                ok

        end


    end.





%% @equiv call_after(Length, Func, [],   {handler,self()})

call_after(Length, Func) ->

    call_after(Length, Func, [], {handler, self()} ).




%% @equiv call_after(Length, Func, Args, {handler,self()})
call_after(Length, Func, Args) -> 

    call_after(Length, Func, Args, {handler,self()}).



%% @type handler() = { handler, PID } | { handler, PID, IdHandle } | no_handler_pid.  Use a handler() to provide handling behavior to functions like {@link call_after/1}.  The PID given is the pid to which result messages will be sent.  If an IdHandle is given, that IdHandle is passed back with the result, to pass extra information back to identify which result it is.  The atom no_handler_pid specifies that you would prefer the result to be discarded.

%% @spec call_after(Length::integer(), Func::function(), Args::atom(), Handler::handler()) -> { ok, spawned_worker, Worker::pid() }

%% @doc {@section Utility} Spawns a side process to non-blockingly make a call after a specified delay.  Will send the result as a message `{ call_after_result, Result }' to the handler process, which is the calling process unless otherwise specified (with {handler,OtherPid} or the atom no_handler_pid).  Delayed return value can include an ID, in the format `{ call_after_result, Result, IdHandle }', if the Handler PID is specified `{handler,PID,ID}', to help distinguish between returned calls if needed. ```1> Dbl = fun(X) -> X*2 end.
%% #Fun<erl_eval.6.13229925>
%%
%% 2> scutil:call_after(1000, Dbl, [3]).
%% {ok,spawned_worker,<0.3810.0>}
%%
%% 3> scutil:receive_one().
%% {item,{call_after_result,6}}
%%
%% 4> Receiver = fun() -> io:format("Listening.~n"), receive X -> io:format("Got ~p~n", [X]), ok end end.
%% #Fun<erl_eval.20.67289768>
%%
%% 5> R = spawn(Receiver).
%% Listening.
%% <0.3817.0>
%%
%% 6> scutil:call_after(1000, Dbl, [3], {handler, R}).
%% {ok,spawned_worker,<0.3829.0>}
%% Got {call_after_result,6}'''

%% @since Version 26

call_after(Length, Func, Args, Handler) ->

    Worker = spawn(?MODULE, call_after_worker, [Length, Func, Args, Handler]),
    { ok, spawned_worker, Worker }.





%% @equiv wait_until_terminate(quiet)

wait_until_terminate() ->

    wait_until_terminate(quiet).



%% @spec wait_until_terminate(IsQuiet::atom()) -> ok

wait_until_terminate(quiet) ->

    receive

        terminate ->
            ok;

        _ ->
            wait_until_terminate(quiet)

    end;



wait_until_terminate(loud) ->

    receive

        terminate ->
            ok;

        X ->
            io:format("Received ~p~n", [X]),
            wait_until_terminate(loud)

    end.