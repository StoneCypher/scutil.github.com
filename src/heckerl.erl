
-module(heckerl).





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
