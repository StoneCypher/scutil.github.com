
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





-module(heckerl).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Stochastic test counterverification a la Ruby's Heckle.").

-testerl_export( { [], heckerl_testsuite } ).





-export( [

    is_pure/2, is_pure/3, is_pure/4

] ).





is_pure(Module, Function) ->
    is_pure_clauselist(Module, Function, lists:flatten([ ClauseList || { _Id, Func, _Ar, ClauseList } <- scutil:abstract_function(Module, Function), Func==Function ])).

is_pure(Module, Function, Arities) when is_list(Arities) ->
    is_pure_clauselist(Module, Function, lists:flatten([ ClauseList || { _Id, Func, Ar, ClauseList } <- scutil:abstract_function(Module, Function),  Func==Function, lists:member(Ar,Arities) ]));

is_pure(Module, Function, Arity) ->
    is_pure_clauselist(Module, Function, lists:flatten([ ClauseList || { _Id, Func, Ar, ClauseList } <- scutil:abstract_function(Module, Function),  Func==Function, Ar==Arity ])).

is_pure(Module, FunctionID, Function, Arity) ->
    is_pure_clauselist(Module, Function, lists:flatten([ ClauseList || {  Id, Func, Ar, ClauseList } <- scutil:abstract_function(Module, Function),  Func==Function, Id==FunctionID, Ar==Arity ])).





is_pure_clauselist(Module, ThisFunction, ClauseList) ->
    resolve_purities([ is_pure_clause(Module, ThisFunction, Clause) || Clause <- ClauseList ]).

is_pure_clause(Module, ThisFunction, Clause) ->
    resolve_purities([ purity(Module, ThisFunction, code_from_clause(Clause)), purity(Module, ThisFunction, where_from_clause(Clause)) ]).





code_from_clause({clause, _Id, _Args, _Where, Code}) ->
    Code.





where_from_clause({clause, _Id, _Args, Where, _Code}) ->
    Where.





resolve_purities(PurityList) -> resolve_purities(PurityList, pure).

resolve_purities([],                    Worst      ) -> Worst;
resolve_purities([pure|RemList],        Worst      ) -> resolve_purities(RemList, Worst);
resolve_purities([{unknown,X}|RemList], pure       ) -> resolve_purities(RemList, {unknown,X});
resolve_purities([{unknown,_}|RemList], {unknown,X}) -> resolve_purities(RemList, {unknown,X});
resolve_purities([{impure,X}|_RemList], _)           -> {impure,X}.





purity(_ThisModule, _ThisFunction, {var,     _UnknownIdNumberTodo, _Label}) -> pure;
purity(_ThisModule, _ThisFunction, {atom,    _UnknownIdNumberTodo, _Label}) -> pure;
purity(_ThisModule, _ThisFunction, {integer, _UnknownIdNumberTodo, _Label}) -> pure;

purity(_ThisModule, _ThisFunction, {nil,     _UnknownIdNumberTodo}) -> pure;            % actually empty list, huhu



purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '==',   Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '/=',   Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '=:=',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '=/=',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);

purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '<',    Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '>',    Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '=<',   Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '>=',   Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);



purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '+',    Oper1}       ) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '+',    Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);

purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '-',    Oper1}       ) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '-',    Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);

purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '*',    Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '/',    Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'rem',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'div',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);

purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'band', Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'bnot', Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'bxor', Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'bor',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'bsl',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'bsr',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);



purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'not',  Oper1}       ) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'and',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'or',   Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'xor',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);



purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'orelse',  Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, 'andalso', Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);



%% @todo bit syntax expressions 6.16 http://erlang.org/doc/reference_manual/expressions.html
%% @todo fun expressions 6.17 http://erlang.org/doc/reference_manual/expressions.html



purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '++', Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);
purity( ThisModule,  ThisFunction, {op, _UnknownIdNumberTodo, '--', Oper1, Oper2}) -> resolve_purities([purity(ThisModule, ThisFunction, Oper1), purity(ThisModule, ThisFunction, Oper2)]);





purity('dets', ThisFunction, _) -> { impure, "dets:" ++ atom_to_list(ThisFunction) ++ " has side effects on disk" };





purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_integer},   [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_float},     [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_list},      [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_tuple},     [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_binary},    [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_atom},      [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_pid},       [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_port},      [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_reference}, [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_bitstring}, [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_boolean},   [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;
purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, is_function},  [{var, _UnknownIdNumber3Todo, _SomeVar}]}) -> pure;





purity( ThisModule,  ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, ThisFunction}, Args}) -> resolve_purities([purity(ThisModule, ThisFunction, Arg)||Arg<-Args]);
purity( ThisModule,  ThisFunction, {call, _UnknownIdNumberTodo, {atom, _UnknownIdNumber2Todo, NewFunction},  Args}) -> resolve_purities([is_pure(ThisModule, NewFunction)] ++ [purity(ThisModule,ThisFunction,Arg)||Arg<-Args]);

% purity(_ThisModule, _ThisFunction, {call, _UnknownIdNumberTodo, {remote, UnknownIdNumber2Todo, {atom, _UnknownIdNumber3Todo, NewModule}, {atom, UnknownIdNumber4Todo, NewFunc}}, Args}) -> purity(NewModule, NewFunc, {call, UnknownIdNumber2Todo, {atom, UnknownIdNumber4Todo, NewFunc}, Args});

purity( ThisModule,  ThisFunction, {cons, _UnknownIdNumberTodo, Item1, Item2}) -> resolve_purities([purity(ThisModule, ThisFunction, Item1), purity(ThisModule, ThisFunction, Item2)]);

purity( ThisModule,  ThisFunction, {tuple, _UnknownIdNumberTodo, Items}) -> resolve_purities([purity(ThisModule, ThisFunction, Item) || Item <- Items]);

purity( ThisModule,  ThisFunction, List) when is_list(List) -> resolve_purities([ purity(ThisModule, ThisFunction, Item) || Item <- List ]);

purity(_ThisModule, _ThisFunction, X) -> { unknown, X }.
