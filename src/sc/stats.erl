




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





%% @spec median_absolute_deviation(List::numericlist()) -> number()

%% @doc {@section Statistics} Calculate the median absolute deviation of a {@type numericlist()}. ```1> scutil:median_absolute_deviation([1,1,2,2,4,6,9]).
%% 1'''

%% @since Version 81

median_absolute_deviation(List) when is_list(List) ->

    ListMedian = scutil:median(List),
    scutil:median( [ abs(ListItem - ListMedian) || ListItem <- List ] ).





% Thanks for some math help on erl-b, erl-c and engset, Vat and Wintermute

%% @private
% todo incomplete

erlang_b_distribution(N,A) ->

   Num   = math:pow(A,N) / scutil:factorial(N),
   Denom = lists:sum([ math:pow(A,I) / scutil:factorial(I) || I <- lists:seq(0,N) ]),

   Num / Denom.





%% @private
% todo incomplete

erlang_c_distribution(N,A) ->

   Num   = (math:pow(A,N) / scutil:factorial(N)) * (N/(N-A)),
   Denom = lists:sum([ math:pow(A,I) / scutil:factorial(I) || I <- lists:seq(0,N-1) ]) + ((math:pow(A,N)/scutil:factorial(N))*(N/(N-A))),

   {wait_probability, Num / Denom}.
