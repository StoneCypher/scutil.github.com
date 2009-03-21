




%% @spec expected_value(List::mixed_weight_list()) -> number()

%% @doc {@section Probability} Returns the expected value of infinite selection from a weighted numeric list.  ```1> scutil:expected_value([1,2,3,4,5,6]).
%% 3.50000
%%
%% 2> scutil:expected_value([ {-1,37}, {35,1} ]).
%% -5.26316e-2'''

%% @since Version 222

expected_value(List) -> expected_value(List, 0, 0).

expected_value( []                                , Sum, Range) -> Sum/Range;
expected_value( [ {Value,Probability} | Remainder], Sum, Range) -> expected_value(Remainder, Sum+(Value*Probability), Range+Probability);
expected_value( [ UnweightedItem      | Remainder], Sum, Range) -> expected_value([{UnweightedItem,1}] ++ Remainder, Sum, Range).
