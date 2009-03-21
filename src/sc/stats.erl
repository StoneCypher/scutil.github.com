




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





%% @equiv root_sum_square(VX)

%% @doc {@section Math} Returns the magnitude of a vector.  A vector's magnitude is the length of its hypoteneuse (and is as such the root sum square of its components).  A vector can be seen as the product of its unit vector and its magnitude; as such many people see a vector's magnitude as its scale. ```1> scutil:vector_magnitude([0,0,0]).
%% 0.0
%%
%% 2> scutil:vector_magnitude([1,0,0]).
%% 1.0
%%
%% 3> scutil:vector_magnitude([1,1,1]).
%% 1.7320508075688772
%%
%% 4> scutil:vector_magnitude([1,2,3]).
%% 3.7416573867739413
%%
%% 5> scutil:vector_magnitude([0,0.4,0.6,0.2,0.4,0.529150262213]).
%% 1.0000000000000433'''

%% @since Version 85

vector_magnitude(VX) -> 

    root_sum_square(VX).





%% @type unit_vector() = vector().  The hypoteneuse of a unit vector is precisely one unit long.  Unit vectors are also called normalized or magnitude-normalized vectors.

%% @spec normalize_vector(Vector::vector()) -> unit_vector()

%% @doc {@section Math} Returns the magnitude of a vector.  A vector's magnitude is the length of its hypoteneuse.  A vector can be seen as the product of its unit vector and its magnitude; as such many people see a vector's magnitude as its scale.  The normal of the zero vector is undefined, in the way that dividing by zero is undefined, and will throw an arithmetic exception. ```1> scutil:normalize_vector([0,3,4]).
%% [0.0,0.6,0.8]'''<span style="color:red">TODO: When tuple comprehensions are introduced to the language, convert this to using them.</span>

%% @since Version 85

normalize_vector(VX) when is_list(VX) ->
    
    VM = vector_magnitude(VX),
    [ X / VM || X <- VX ];



normalize_vector(VX) when is_tuple(VX) -> 

    list_to_tuple(normalize_vector(tuple_to_list(VX))).





%% @spec amean_vector_normal(VX::numeric_list()) -> number()

%% @doc {@section Statistics} Returns the arithmetic mean of the elements of the unit vector for the vector provided.

%% @since Version 85

amean_vector_normal(VX) -> 

    arithmetic_mean(normalize_vector(VX)).





%% @spec gmean_vector_normal(VX::numeric_list()) -> number()

%% @doc {@section Statistics} Returns the geometric mean of the elements of the unit vector for the vector provided.

%% @since Version 85

gmean_vector_normal(VX) -> 

    geometric_mean(normalize_vector(VX)).





%% @spec hmean_vector_normal(VX::numeric_list()) -> number()

%% @doc {@section Statistics} Returns the harmonic mean of the elements of the unit vector for the vector provided.

%% @since Version 85

hmean_vector_normal(VX) -> 

    harmonic_mean(normalize_vector(VX)).
