
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





-module(sc_process).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Process management and process utility routines").

-testerl_export( { [], sc_process_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    get_linked_processes/0,

    start_register_if_not_running/3,
      start_register_if_not_running/4,
      start_register_if_not_running/5

] ).





%% @since Version 175

get_linked_processes() ->

    [U] = [ 
              V 
          ||
              {links,V} <- process_info(self())
          ],

    U.





%% @equiv start_register_if_not_running(node(), Name, Module, Function, [])

start_register_if_not_running(Name, Module, Function) -> 

    start_register_if_not_running(node(), Name, Module, Function, []).
    


%% @equiv start_register_if_not_running(node(), Name, Module, Function, Args)

start_register_if_not_running(Name, Module, Function, Args) ->

    start_register_if_not_running(node(), Name, Module, Function, Args).



%% @spec start_register_if_not_running(Node::atom(), Name::atom(), Module::atom(), Function::atom(), Args::list()) -> pid() | ok

%% @doc {@section Documentary} Check whether a process is registered locally, and if not, spawn it with a give function and arguments.  ```1> whereis(test).
%% undefined
%%
%% 2> scutil:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% ok
%%
%% 3> whereis(test).
%% <0.726.0>
%%
%% 4> test ! terminate.
%% terminate
%%
%% 5> whereis(test).
%% undefined
%%
%% 6> scutil:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% true
%%
%% 7> whereis(test).
%% <0.731.0>
%%
%% 8> scutil:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% ok
%%
%% 9> whereis(test).
%% <0.731.0>'''

%% @since Version 8

start_register_if_not_running(Node, Name, Module, Function, Args) when is_atom(Name), is_atom(Module), is_atom(Function), is_list(Args) ->

    case whereis(Name) of

        undefined -> 
            register(Name, spawn(Node, Module, Function, Args)),
            ok;

        _ ->
            ok

    end.
