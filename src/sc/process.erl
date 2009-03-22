
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





%% @since Version 175

get_linked_processes() ->

    [U] = [ V || 
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
