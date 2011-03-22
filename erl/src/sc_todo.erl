
from sc_lists

%% @todo TODO

% key_split(KeyId, TupleList)           when is_list(TupleList) -> key_split(KeyId, TupleList,                       unsorted).
% key_split(KeyId, TupleList, unsorted) when is_list(TupleList) -> key_split(KeyId, lists:keysort(KeyId, TupleList), sorted);
% key_split(KeyId, TupleList, sorted)   when is_list(TupleList) ->

% key_minimum(
% key_maximum(

% todo invert this so that it returns {currentcount, fun, result} so that it can be continued
% generate(0, _) -> [];
% generate(N, Fun) when is_integer(N) andalso N > 0 andalso is_function(Fun) -> [Fun()] ++ generate(N-1,Fun).
