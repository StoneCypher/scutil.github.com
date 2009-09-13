
-module(sc_generator).

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

-description("A lazy generator template type").

-todo([unit_tests,stochastic_tests,lib_test_rig_integrate,docs,doc_examples,doc_extraction]).





-testerl_export( { [], sc_generator_testsuite } ).





-library_requirements([
    {scutil,  161},
    {testerl, 66}
]).





-export([

    create/2,

    current/1,
    next/1,
    next_n/2,
%    all/1,
    reset/1,

%    terminate/1,

%     create/3

%   one/0
%   many/1,
%   all/0,

%   filtered_one/1,
%   filtered_all/1

    fibonacci/1,



    % private, do not call functions below this line

    core/3

]).





%% @since Version 398

fibonacci( {First, Second} ) when is_integer(First), is_integer(Second) -> { state, { Second, First+Second } };
fibonacci(_)                                                            -> corrupt.





%% @since Version 397

core(InitializationValue, CurrentValue, Generator) ->

    {ShouldContinue, NewValue} = receive

        { Sender, current } ->
            {true, CurrentValue};

        { Sender, next } ->
            case CurrentValue of complete       -> {true, complete};
                                 corrupt        -> {true, corrupt};
                                 {state, State} -> {true, Generator(State)};
                                 _AnythingElse  -> {true, corrupt}
            end;

        { Sender, reset } ->
            {true, {state,InitializationValue}};

        { Sender, terminate } ->
            {false, { terminating, self() } }

    end,

    Sender ! { sc_generator, NewValue },

    case ShouldContinue of
        true  -> core(InitializationValue, NewValue, Generator);
        false -> ok
    end.





%% @since Version 397

create(InitializationValue, Generator) ->

    { sc_generator_core, spawn(fun() -> core(InitializationValue, {state,InitializationValue}, Generator) end) }.





% create(InitialValue, Generator, Filter) ->





act_on({ sc_generator_core, Pid }, Action) when is_pid(Pid) ->

    Pid ! { self(), Action },
    receive
        { sc_generator, Answer } -> Answer
    end.





%% @since Version 400
current(Core) -> act_on(Core, current).

%% @since Version 403
reset(Core)   -> act_on(Core, reset).

%% @since Version 401
next(Core)    -> act_on(Core, next).

%% @since Version 402
next_n(Core, N) when N >= 0 -> next_n(Core, N, []).

next_n(Core, 0, Work) -> lists:reverse(Work);
next_n(Core, N, Work) -> next_n(Core, N-1, [next(Core)] ++ Work).
