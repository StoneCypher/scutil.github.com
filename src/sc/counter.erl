
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
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

%% @todo build persistence on note, which in turn can be dets or mnesia





-module(sc.counter).

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

    counter/1,
    counters/1,
    
    inc_counter/1,
      inc_counter/2,

    dec_counter/1,
      dec_counter/2,
      
    adjust_counter/2,
    reset_counter/1,
    
    set_counter/2,
    
    % exports
    counter_process/0

] ).




%% @spec counter(Name::any()) -> number()

%% @doc {@section Counters} Checks a counter's value; if the counter was not already defined, it will report zero. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:inc_counter(hello).
%% 1
%%
%% 3> scutil:inc_counter(hello).
%% 2
%%
%% 4> scutil:inc_counter(hello).
%% 3
%%
%% 5> scutil:counter(hello).
%% 3
%%
%% 6> scutil:reset_counter(hello).
%% 0
%%
%% 7> scutil:counter(hello).
%% 0'''

%% @since Version 54

counter(Name) ->

    .sc.process:start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), get_counter, Name},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @spec counters(Names::list()) -> list_of_integers()

%% @doc {@section Counters} Checks a counter's value; if the counter was not already defined, it will report zero. ```1> scutil:counter(hello).
%% 0'''

%% @since Version 138

counters(Names) ->

    [ counter(X) || X <- Names ].





%% @equiv adjust_counter(Name,1)

inc_counter(Name) ->

    adjust_counter(Name, 1).



%% @equiv adjust_counter(Name,By)

inc_counter(Name,By) ->

    adjust_counter(Name, By).



%% @equiv adjust_counter(Name,-1)

dec_counter(Name) ->

    adjust_counter(Name, -1).



%% @equiv adjust_counter(Name,-1*By)

dec_counter(Name,By) ->

    adjust_counter(Name, -1*By).





%% @spec adjust_counter(Name::any(), By::number()) -> number()

%% @doc {@section Counters} Adds to a counter's value; if the counter was not already defined, it will become the value in the `By' argument. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:inc_counter(hello).
%% 1
%%
%% 3> scutil:adjust_counter(hello, 3).
%% 4'''

%% @since Version 54

adjust_counter(Name, By) when is_number(By) ->

    .sc.process:start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), adjust_counter, Name, By},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv set_counter(Name, 0)

%% @since Version 54

reset_counter(Name) -> 

    set_counter(Name, 0).





%% @spec set_counter(Name::any(), To::number()) -> 0

%% @doc {@section Counters} Sets a counter's value to a specific value. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:set_counter(hello,4).
%% 4
%%
%% 3> scutil:counter(hello).
%% 4
%%
%% 4> scutil:reset_counter(hello).
%% 0
%%
%% 5> scutil:counter(hello).
%% 0'''

%% @since Version 54

set_counter(Name, To) when is_number(To) ->

    .sc.process:start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), set_counter, Name, To},

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


        {Caller, adjust_counter, Name, By} ->

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
