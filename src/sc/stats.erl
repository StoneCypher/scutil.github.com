




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





% thanks to Chile and Kraln for straightening me out on moments and central moments

%% @spec moment(List::list(), N::number()) -> float()

%% @doc {@section Statistics} Takes the Nth moment of a list.  The Nth moment of a list is the arithmetic mean of the list items, each taken to the Nth power.  Fractional Ns are well defined
%% and have obscure uses, though most will only ever use this with integer values of N; this function is valid for both.  Not to be confused with {@link central_moment/2}.  {@section Thanks}
%% to Kraln and Chile for straightening me out on moments and central moments.  ```1> scutil:moment([1,1,1], 2).
%% 1.0
%%
%% 2> scutil:moment([2,2,2], 2).
%% 4.0
%%
%% 3> scutil:moment([1,2,3], 2).
%% 4.666666666666667
%%
%% 4> scutil:moment([1,2,3], 3).
%% 12.0
%%
%% 5> scutil:moment([1,2,3], 3.5).
%% 19.693026767781483'''

%% @since Version 50

moment(List, N) when is_list(List), is_number(N) ->
    
    scutil:arithmetic_mean( [ math:pow(Item, N) || Item <- List ] ).



%% @equiv [ moment(List, N) || N <- [2,3,4] ]

moments(List) -> 

    moments(List, [2,3,4]).
    


%% @equiv [ moment(List, N) || N <- Moments ]

moments(List, Moments) when is_list(Moments) -> 

    [ moment(List, M) || M <- Moments ].





% thanks to Chile and Kraln for straightening me out on moments and central moments

%% @spec central_moment(List::list(), N::integer()) -> float()

%% @doc {@section Statistics} Takes the Nth cetral moment of a list.  The Nth central moment of a list is the arithmetic mean of (the list items each minus the mean of the list, each
%% taken to the Nth power).  In a sense, this is the "normalized" moment.  Fractional Ns are not defined.  Not to be confused with {@link moment/2}.  {@section Thanks} to Kraln and
%% Chile for straightening me out on moments and central moments.  ```1> scutil:central_moment([1,1,1], 2).
%% 0.0
%%
%% 2> scutil:central_moment([2,2,2], 2).
%% 0.0
%%
%% 3> scutil:central_moment([1,2,3], 2).
%% 0.666666666666666
%%
%% 4> scutil:central_moment([1,2,3], 3).
%% 0.0'''

%% @since Version 50

central_moment(List, N) when is_list(List), is_integer(N) ->
    
    ListAMean = scutil:arithmetic_mean(List),
    scutil:arithmetic_mean( [ math:pow(Item-ListAMean, N) || Item <- List ] ).



%% @equiv [ central_moment(List, N) || N <- [2,3,4] ]

central_moments(List) ->

    central_moments(List, [2,3,4]).



%% @equiv [ central_moment(List, N) || N <- Moments ]

central_moments(List, Moments) when is_list(Moments) -> 

    [ central_moment(List, M) || M <- Moments ].





%% @equiv central_moment(List, 3)

skewness(List) -> 

    central_moment(List, 3).
    


%% @equiv central_moment(List, 4)

kurtosis(List) -> 

    central_moment(List, 4).





%% @spec std_deviation(Values::numericlist()) -> float()

%% @doc {@section Statistics} Measures the standard deviation of the values in the list.  ```1> scutil:std_deviation([1,2,3,4,5]).
%% 1.4142135623730951
%%
%% 2> scutil:std_deviation([2,2,2,2,2]).
%% 0.0'''

%% @since Version 39

std_deviation(Values) when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ (Val-Mean)*(Val-Mean) || Val <- Values ])).





%% @spec histograph(List::list()) -> weightlist()

%% @doc {@section Statistics} Takes a histograph count of the items in the list.  Mixed type lists are safe.  Input lists do not need to be sorted.  The histograph is shallow - that is, the histograph of `[ [1,2], [1,2], [2,2] ]' is `[ {[1,2],2}, {[2,2],1} ]', not `[ {1,2}, {2,4} ]'. ```1> scutil:histograph([1,2,a,2,b,1,b,1,b,2,a,2,2,1]).
%% [{1,4},{2,5},{a,2},{b,3}]
%%
%% 2> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,100000) ]).
%% [{0,10044}, {1,9892}, {2,10009}, {3,10016}, {4,10050}, {5,10113}, {6,9990}, {7,9994}, {8,10004}, {9,9888}]'''

%% @since Version 19

%% @todo add an argument presort to this and other functions to skip the sorting pass

histograph(List) when is_list(List) ->

    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).



%% @private

histo_count( [], Current, Count, Work) -> 

     lists:reverse([{Current,Count}]++Work);



histo_count( [Current|Tail], Current, Count, Work) -> 

    histo_count(Tail, Current, Count+1, Work);
    
    

histo_count( [New|Tail], Current, Count, Work) -> 

    histo_count(Tail, New, 1, [{Current,Count}] ++ Work).
