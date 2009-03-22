
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <p></p>


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





-module(sc.serialism).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Serialism routines, simplifying single process batch jobs.").

-testerl_export( { [], sc_serialism_testsuite } ).  % todo needs test suite

-library_requirements([
    % TODO
]).





-export( [
    multi_do/3,
      multi_do/4
] ).





%% @equiv multi_do(C,M,F,[])
%% @since Version 38

multi_do(C, Module, Func) ->

    multi_do(C, Module, Func, [],   []).





%% @spec multi_do(Count::integer(), Module::atom(), Function::atom(), Args::list()) -> list()

%% @doc {@section Serialism} Take an iteration count, a module name, a function name and an argument list, and repeatedly apply the argument list to the module/function, count times.  This is primarily useful with nondeterministic functions whose result might change despite identical arguments, such as functions with random behavior; for example, this function is invoked to implement stochastic testing in <a href="http://testerl.com/">TestErl</a>. ```1> scutil:multi_do(10, scutil, rand, [100]).
%% [9,94,4,82,77,44,89,19,45,92]
%%
%% 2> scutil:multi_do(10, scutil, rand, [10000]).
%% [2377,2559,1713,8489,4468,3261,3344,3751,380,2525]'''

%% @since Version 38

multi_do(C, Module, Func, Args) ->

    multi_do(C, Module, Func, Args, []).





%% @private

multi_do(0,_Module,_Func,_Args, Work) ->

    Work;





%% @private

multi_do(I, Module, Func, Args, Work) ->

    multi_do(I-1, Module, Func, Args, Work ++ [apply(Module, Func, Args)]).
