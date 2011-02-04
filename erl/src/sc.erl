
%%%%%%%%%
%%
%%  Stonecypher's Erlang Utility Library - reboot 2011
%%  Written by John Haugeland, http://fullof.bs/
%%
%%  To skip to code, which is hundreds of lines down, search for "-module("
%%  without quotes.
%%
%%  This was last tested by the author in Erl OTP 14b1 / 5.8.1.1 .  Please
%%  run sc:test(deep) or sc:test([verbose,deep]) before using, to verify that
%%  this library functions correectly in the current Erlang virtual machine
%%  and environment.  Removing deep will execute a faster, less trustworthy
%%  subset of the tests.  Removing verbose will dump much less information to
%%  the console.
%%
%%  There is significant documentation.  With paths appropriate for your
%%  system, call sc:gen_docs("/path/to/source", "/path/for/docs/to/live")
%%  to generate.  Do not use trailing slashes.  Windows paths are fine;
%%  remember to use \\ , because it's a string and you're quoting the
%%  backslash.  Automatic documentation generation via edoc will then
%%  generate HTML docs.
%%
%%  Past here, documentation should be generally be read in the HTML format.





%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since September 14, 2007





%% @doc This is the 2011 revamp of <a href="http://scutil.com/" target="_blank">scutil</a>'s erlang library.
%%
%% <a href="http://fullof.bs/">I'm</a> modernizing my library.  Originally it got too big, so I split it up.  However, that's proven more trouble than it's worth - I often found myself asking whether the thing I wanted was
%% in `_math', `_correlate', `_statistics', `_lists', `_operators', etc.  No more.  It's all getting dumped into `sc.erl' and `sc_tests.erl'.  Prototypes are pushed into `sc_prototype.erl'; that is
%% code which is either insufficiently tested or unfinished.  It's moving to eunit; sc_test is being dumped.  I'm taking a lot of the customization stuff out of the generated docs, too.
%% There was always an emphasis on testing, but it's getting taken much more seriously this time.  I am also no longer a language novice; I can replace at least some of the naive implementations
%% with marginally less naive ones, break fewer conventions, replicate fewer extant library functions, choose better naming and a more appropriate argument order, et cetera.
%%
%% This also means I can completely ditch all the remnants of the botched move to packages, provide radically less automation which is radically more effective, and generally do a better job of
%% presenting the actual volume of functionality present here.  We can get the coverage tools out.  We can run prototypical work in a single separate file, to distinguish quality.  Etc.
%%
%% Generally, this is intended to just be an all around improvement and replacement for the library I was pretty happy with, but which was showing strain.
%%
%% As always, this work remains open source under the MIT license, because free isn't real unless it's free for everyone.  <a href="http://scutil.com/license/">http://scutil.com/license/</a>
%%
%% ScUtil is free.  However, I'd like to know where it's ended up.  Therefore, please consider mail to stonecypher@gmail.com with text saying where this went a form of "registration."  This is not required.
%%
%% @end

-module(sc).





-export([

    extrema/1,

    test/0,
      test/1,

    gen_docs/0,
      gen_docs/2

]).





-include_lib("eunit/include/eunit.hrl").





%% @spec gen_docs() -> ok|{'EXIT',any()}
%% @equiv gen_docs("/projects/scutil/erl/src", "/projects/scutil/erl/src/docs")
%% @doc Generates library documentation using the paths appropriate for the author's PC; you almost certainly want {@link gen_docs/2} instead.  ```1> sc:gen_docs().
%% ok'''
%% @since 458

gen_docs() ->

    gen_docs("/projects/scutil/erl/src", "/projects/scutil/erl/src/docs").





%% @spec gen_docs(WhereIsSrc::string(), WhereToPutDocs::string()) -> ok|{'EXIT',any()}
%% @doc Generates library documentation from and to the specified paths `WhereIsSrc' and `WhereToPutDocs' respectively.  Do not use trailing slashes.  Windows paths are okay; remember to double your
%% backslashes, as backslashes in strings need to be quoted.  ```1> sc:gen_docs("/projects/scutil/erl/src", "/projects/scutil/erl/src/docs").
%% ok'''
%% @since 458

gen_docs(WhereIsSrc, WhereToPutDocs) ->

    filelib:ensure_dir(WhereToPutDocs),
    edoc:files([WhereIsSrc++"/sc.erl", WhereIsSrc++"/sc_tests.erl"], [{dir, WhereToPutDocs}, {new,true}]).
%   edoc:files([WhereIsSrc++"/sc.erl", WhereIsSrc++"/sc_proto.erl", WhereIsSrc++"/sc_tests.erl", WhereIsSrc++"/sc_proto_tests.erl"], [{dir, WhereToPutDocs}, {new,true}]).





%% @spec test() -> ok|error
%% @doc Runs the test suite in terse form. ```1> sc:test().
%%   All 9 tests passed.
%% ok'''
%% @since 458

test() ->

    eunit:test(sc).





%% @spec test(verbose) -> ok|error
%% @doc Runs the test suite in verbose form. ```1> sc:test(verbose).
%% ======================== EUnit ========================
%% module 'sc'
%%   module 'sc_tests'
%%     Extrema tests
%%       sc_tests:18: extrema_test_ (1,2,3,4)...ok
%%       sc_tests:19: extrema_test_ (-1,-2,-3)...ok
%%       sc_tests:20: extrema_test_ (-1.1,0,1.1)...ok
%%       sc_tests:21: extrema_test_ (a,b,c)...ok
%%       sc_tests:22: extrema_test_ (1,a,{})...ok
%%       sc_tests:23: extrema_test_ (1)...ok
%%       sc_tests:25: extrema_test_ ([] error)...ok
%%       [done in 0.109 s]
%%     [done in 0.109 s]
%%   [done in 0.109 s]
%% =======================================================
%%   All 7 tests passed.
%% ok'''
%% @since 460

test(verbose=_Style) ->

    eunit:test(sc, [verbose]).






%% @spec extrema(List::non_empty_list()) -> {Low::any(),Hi::any()}
%% @doc Returns the lowest and highest values in a list of one or more member in the form `{Lo,Hi}'.  `error()'s `badarg' for empty lists.  Mixed-type safe; sorts according to type order rules.  ```1> sc:extrema([1,2,3,4]).
%% {1,4}
%%
%% 2> sc:extrema([1,2,3,a,b,c]).
%% {1,c}'''
%% @since Version 460

extrema([]) ->

    error(badarg);



extrema(List) ->

    [First | _] = List,

    Next = fun(Next,T) ->

               {Lo, Hi} = T,

               Lo2 = if
                   Next < Lo -> Next;
                   true      -> Lo
               end,

               Hi2 = if
                   Next > Hi -> Next;
                   true      -> Hi
               end,

               {Lo2, Hi2}

           end,

    lists:foldl(Next, {First,First}, List).
