
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





%% @todo Move random module into one of these





-export([

    create/2,

    current/1,
    all/1,

    next/1,
      next/2,

    reset/1,

    terminate/1,

    fibonacci/1,
    countdown/1,



    % private, do not call functions below this line

    core/3

]).





%% @since Version 398

fibonacci( {First, Second} ) when is_integer(First), is_integer(Second) -> { state, { Second, First+Second } };
fibonacci(_)                                                            -> corrupt.





%% @since Version 398

countdown( {Last, Last} ) when is_integer(Last)                   -> complete;
countdown( {Next, Last} ) when is_integer(Next), is_integer(Last) -> { state, {Next-1, Last} };
countdown(_)                                                      -> corrupt.





%% @since Version 406

eat_all(CurrentValue, Generator) -> eat_all(CurrentValue, Generator, []).

eat_all(complete, _Generator, Work) -> lists:reverse(Work);
eat_all(corrupt,  _Generator, Work) -> lists:reverse(Work);
eat_all({state,S}, Generator, Work) -> V = Generator(S), eat_all(V, Generator, [V]++Work).





%% @since Version 397

core(InitializationValue, CurrentValue, Generator) ->

    {ShouldContinue, ToSend, NewValue} = receive

        { Sender, current } ->                                           {true,  CurrentValue,                     CurrentValue                };

        { Sender, next } ->
            case CurrentValue of complete       ->                       {true,  complete,                         complete                    };
                                 corrupt        ->                       {true,  corrupt,                          corrupt                     };
                                 {state, State} -> G = Generator(State), {true,  G,                                G                           };
                                 _AnythingElse  ->                       {true,  corrupt,                          corrupt                     }
            end;

        { Sender, reset }     ->                                         {true,  {state,InitializationValue},      {state,InitializationValue} };
        { Sender, all }       ->                                         {true,  eat_all(CurrentValue, Generator), complete                    };
        { Sender, terminate } ->                                         {false, { terminating, self() },          corrupt                     }

    end,

    Sender ! { sc_generator, ToSend },

    case ShouldContinue of
        true  -> core(InitializationValue, NewValue, Generator);
        false -> ok
    end.





%% @since Version 397

create(InitializationValue, Generator) ->

    { sc_generator_core, spawn(fun() -> core(InitializationValue, {state,InitializationValue}, Generator) end) }.





%% @since Version 403

act_on({ sc_generator_core, Pid }, Action) when is_pid(Pid) ->

    Pid ! { self(), Action },
    receive
        { sc_generator, Answer } -> Answer
    end.





%% @since Version 400
current(Core)   -> act_on(Core, current).

%% @since Version 403
reset(Core)     -> act_on(Core, reset).

%% @since Version 404
terminate(Core) -> act_on(Core, terminate).





%% @since Version 401
next(Core) -> act_on(Core, next).


%% @since Version 402
next( Core, N) when N >= 0 -> next(Core, N, []).

next(_Core, 0, Work)       -> lists:reverse(Work);
next( Core, N, Work)       ->

    case next(Core) of
        complete -> lists:reverse([complete] ++ Work);
        corrupt  -> lists:reverse([corrupt]  ++ Work);
        Other    -> next(Core, N-1, [Other] ++ Work)
    end.





%% @since Version 405
% Risky, eg for unbounded or huge series this will crash you, possibly slowly
all(Core) -> act_on(Core, all).
