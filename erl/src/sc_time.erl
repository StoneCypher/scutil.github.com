
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-04-05 16:16:32 -0600 (Sun, 05 Apr 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc_time).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Time measurement and benchmarking utilities.").

-testerl_export( { [], sc_time_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    diff/2,

    benchmark/2,
      benchmark/3,

    call_after/2,
      call_after/3,
      call_after/4,

    %% @todo this should live somewhere else
    wait_until_terminate/0,
      wait_until_terminate/1,

    % exports
    call_after_worker/4

] ).





%% @type timestamp() = {Megaseconds::non_negative_integer(), Seconds::non_negative_integer(), MicroSeconds::non_negative_integer()}.

%% @spec diff(A::timestamp(), B::timestamp()) -> float()

%% @doc {@section Utility} Returns the difference, in seconds as a float, between two erlang timestamps as returned by `erlang:now()'.  Negative differences are returned if the latter timestamp `B' is earlier than the former timestamp `A'.  ```1> A = now().
%% {1232,947675,340000}
%%
%% 2> B = now().
%% {1232,947679,412000}
%%
%% 3> scutil:diff(A,B).
%% 4.072
%%
%% 4> scutil:diff(B,A).
%% -4.072'''

%% @since Version 9

diff({AM,AS,AU}, {BM, BS, BU}) ->

    ((BM-AM) * 1000000) + (BS-AS) + ((BU-AU)/1000000).





%% @since Version 38

benchmark(Fun, Args) ->

    Start  = now(),
    Result = apply(Fun, Args),
    End    = now(),

    { diff(Start,End), Result }.





benchmark(Module, Func, Args) ->

    Start  = now(),
    Result = apply(Module, Func, Args),
    End    = now(),

    { diff(Start,End), Result }.





%% @private

% Handler must be no_handler_pid or { handler, PID [, idhandle] }

call_after_worker(MS, Func, Args, Handler) ->

    receive
        % Nothing

    after MS ->


        Result = case Func of

            { Module, FuncName } ->
                apply(Module, FuncName, Args);

            FuncName ->
                apply(FuncName, Args)

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
