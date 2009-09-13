
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





-module(sc_lazy).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Support for lazy computation").

-testerl_export( { [], sc_lazy_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    eval/2,
    dump/0,
    terminate/0,





    % private, do not call below this line

    cache_core/0

] ).





%% @since Version 408

cache_core() ->

    {ShouldContinue, Result} = receive

        { Sender, terminate } ->
            { do_not_continue, { terminating, self() } };

        { Sender, dump } ->
            { continue, get() };

        { Sender, dump, Fun } ->
            { continue, [ {{F,A},Val} || {{F,A},Val} <- get(), F == Fun ] };

        { Sender, lazy_apply, Fun, Args } ->
            case get({Fun,Args}) of

                undefined ->
                    E = apply(Fun, Args),
                    put({Fun,Args}, {val, E}),
                    {continue, E};

                {val,V} ->
                    {continue, V}
            end

    end,
    Sender ! { sc_lazy_cache, Result },

    case ShouldContinue of
        continue        -> cache_core();
        do_not_continue -> ok
    end.





%% @since Version 408

terminate() ->

    case whereis(sc_lazy_cache) of

        undefined ->
            { error, sc_lazy_cache_not_running };

        Defined   ->
            Defined ! { self(), terminate },
            receive
                { sc_lazy_cache, Message } -> { sc_lazy_cache, Message }
            end

    end.





%% @since Version 408

dump() ->

    case whereis(sc_lazy_cache) of

        undefined ->
            { error, sc_lazy_cache_not_running };

        Defined   ->
            Defined ! { self(), dump },
            receive
                { sc_lazy_cache, Message } -> { sc_lazy_cache, Message }
            end

    end.





%% @since Version 408

% 1> F = fun(X) -> receive after 10000 -> X*2 end end.
% #Fun<erl_eval.6.13229925>
%
% 2> sc_time:benchmark(fun sc_lazy:eval/2, [F,[4]]).
% {10.00791,8}
%
% 3> sc_time:benchmark(fun sc_lazy:eval/2, [F,[4]]).
% {1.0e-6,8}


eval(Fun, Args) ->

    Pid = case whereis(sc_lazy_cache) of

        undefined ->
            P = spawn(fun cache_core/0),
            register(sc_lazy_cache, P),
            P;

        Defined ->
            Defined

    end,

    Pid ! { self(), lazy_apply, Fun, Args },

    receive
        { sc_lazy_cache, Result } -> Result
    end.
