
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date$</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks

%% @todo build persistence on note, which in turn can be dets or mnesia





-module(sc_counter).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Counter management module.").

-testerl_export( { [], sc_counter_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    at/1,

    inc/1,
      inc/2,

    dec/1,
      dec/2,

    adjust_by/2,
    reset/1,

    set/2,

    % exports
    counter_process/0

] ).




%% @spec counter(Name::any()) -> number()

%% @doc {@section Counters} Checks a counter's value; if the counter was not already defined, it will report zero. ```1> sc_counter:counter(hello).
%% 0
%%
%% 2> sc_counter:inc(hello).
%% 1
%%
%% 3> sc_counter:inc(hello).
%% 2
%%
%% 4> sc_counter:inc(hello).
%% 3
%%
%% 5> sc_counter:counter(hello).
%% 3
%%
%% 6> sc_counter:reset(hello).
%% 0
%%
%% 7> sc_counter:counter(hello).
%% 0'''

%% @since Version 54

at(Names) when is_list(Names) ->

    [ at(X) || X <- Names ];

at(Name) ->

    sc_process:start_register_if_not_running(sc_counter_counter_process, sc_counter, counter_process, []),
    sc_counter_counter_process ! {self(), get_counter, Name},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv adjust_by(Name,1)

inc(Name) ->

    adjust_by(Name, 1).



%% @equiv adjust_by(Name,By)

inc(Name,By) ->

    adjust_by(Name, By).



%% @equiv adjust_by(Name,-1)

dec(Name) ->

    adjust_by(Name, -1).



%% @equiv adjust_by(Name,-1*By)

dec(Name,By) ->

    adjust_by(Name, -1*By).





%% @spec adjust_by(Name::any(), By::number()) -> number()

%% @doc {@section Counters} Adds to a counter's value; if the counter was not already defined, it will become the value in the `By' argument. ```1> sc_counter:counter(hello).
%% 0
%%
%% 2> sc_counter:inc(hello).
%% 1
%%
%% 3> sc_counter:adjust_by(hello, 3).
%% 4'''

%% @since Version 54

adjust_by(Name, By) when is_number(By) ->

    sc_process:start_register_if_not_running(sc_counter_counter_process, sc_counter, counter_process, []),
    sc_counter_counter_process ! {self(), adjust_by_counter, Name, By},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv set(Name, 0)

%% @since Version 54

reset(Name) -> 

    set(Name, 0).





%% @spec set(Name::any(), To::number()) -> 0

%% @doc {@section Counters} Sets a counter's value to a specific value. ```1> sc_counter:counter(hello).
%% 0
%%
%% 2> sc_counter:set(hello,4).
%% 4
%%
%% 3> sc_counter:counter(hello).
%% 4
%%
%% 4> sc_counter:reset(hello).
%% 0
%%
%% 5> sc_counter:counter(hello).
%% 0'''

%% @since Version 54

set(Name, To) when is_number(To) ->

    sc_process:start_register_if_not_running(sc_counter_counter_process, sc_counter, counter_process, []),
    sc_counter_counter_process ! {self(), set_counter, Name, To},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @private

counter_process() ->

    receive


        shutdown ->
            ok;


        {Caller, get_counter, Name} ->

            case get(Name) of

                undefined ->
                    Caller ! {counter_at, Name, 0},
                    put(Name,0),
                    counter_process();

                Defined ->
                    Caller ! {counter_at, Name, Defined},
                    counter_process()

            end;


        {Caller, adjust_by_counter, Name, By} ->

            case get(Name) of

                undefined ->
                    Caller ! {counter_at, Name, By},
                    put(Name,By),
                    counter_process();

                Defined ->
                    New = Defined+By,
                    Caller ! {counter_at, Name, New},
                    put(Name,New),
                    counter_process()

            end;


        {Caller, set_counter, Name, To} ->

            Caller ! {counter_at, Name, To},

            case To of

                0 ->
                    erase(Name);

                T ->
                    put(Name,T)

            end,

            counter_process()


    end.
