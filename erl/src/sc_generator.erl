
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





fibonacci( {state, {First,Second}} ) when is_integer(First), is_integer(Second) -> { state, { Second, First+Second } };
fibonacci(_)                                                                    -> corrupt.





core(InitializationValue, CurrentValue, Generator) ->

    NewValue = receive

        { Sender, next } ->
            case CurrentValue of complete       -> complete;
                                 corrupt        -> corrupt;
                                 {state, State} -> Generator(State);
                                 _AnythingElse  -> corrupt
            end;

        { Sender, reset } ->
            Generator(InitializationValue);

        { Sender, terminate } ->
            Sender ! { terminating, { sc_generator, self() } },
            ok

    end,

    Sender ! NewValue,
    core(InitializationValue, NewValue, Generator).





create(InitializationValue, Generator) ->

    { sc_generator_core, spawn(fun() -> core(InitializationValue, Generator(InitializationValue), Generator) end) }.


% create(InitialValue, Generator, Filter) ->




% one()