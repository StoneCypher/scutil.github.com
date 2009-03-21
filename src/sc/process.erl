





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
