
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





-module(sc.stats).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Statistics routines.").

-testerl_export( { [], sc_stats_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    expected_value/1,
    median_absolute_value/1,
    erlang_b_distribution/2,
    erlang_c_distribution/2,

    moment/2,

    moments/1,
      moments/2,

    central_moment/2,

    central_moments/1,
      central_moments/2,
      
    skewness/1,
    kurtosis/1,
    std_deviation/1,
    
    histograph/1,
    mode/1,
    median/1,
    
    amean_vector_normal/1,
    gmean_vector_normal/1,
    hmean_vector_normal/1,

    arithmetic_mean/1,
    geometric_mean/1,
    harmonic_mean/1,


] ).





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





%% @spec median(List::numericlist()) -> any()

%% @doc {@section Statistics} Takes the median (central) value of a list.  Sorts the input list, then finds and returns the middle value.  ```1> scutil:median([1,2,999]).
%% 2'''

%% @see arithmetic_mean/1
%% @see mode/1

%% @since Version 8

median(List) when is_list(List) ->

    SList = lists:sort(List),
    Length = length(SList),

    case even_or_odd(Length) of

        even ->
            [A,B] = lists:sublist(SList, round(Length/2), 2),
            (A+B)/2;

        odd ->
            lists:nth( round((Length+1)/2), SList )

    end.





%% @spec mode(List::numericlist()) -> any()

%% @doc {@section Statistics} Takes the mode (most common) value(s) of a list, as a list.  If there are more than one value tied for most common, all tied will be returned.  This function is safe for mixed-type lists, and does not perform deep traversal (that is, the mode of `[ [2,2] ]' is `[2,2]', not `2'). ```scutil:mode([1,2,1,3,1,4]).
%% [1]
%%
%% 2> scutil:mode([ [1,2,3], [2,3,4], [3,4,5], [2,3,4] ]).
%% [[2,3,4]]
%%
%% 3> scutil:mode([ a,b, 1, a,b, 2, a,b, 3 ]).
%% [a,b]'''

%% @see arithmetic_mean/1
%% @see median/1

%% @since Version 8

mode([]) ->

    [];



mode(List) when is_list(List) ->
    
    mode_front(lists:reverse(lists:keysort(2, scutil:histograph(List)))).



mode_front([{Item,Freq}|Tail]) ->
    
    mode_front(Tail, Freq, [Item]).



mode_front([ {Item, Freq} | Tail],  Freq,   Results) ->

    mode_front(Tail, Freq, [Item]++Results);



mode_front([ {_Item,_Freq} |_Tail], _Better, Results) ->

    Results;



mode_front( [], _Freq, Results) ->

    Results.





%% @type numericlist() = list().  All members of a numeric list must be number()s.
%% @spec arithmetic_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the arithmetic mean (often called the average) of a list of numbers. ```1> scutil:arithmetic_mean([1,2,3,4,5]).
%% 3.0'''

%% @see geometric_mean/1
%% @see harmonic_mean/1
%% @see weighted_arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 33

arithmetic_mean([]) ->

    0.0;



arithmetic_mean(List) when is_list(List) ->
    
    lists:sum(List) / length(List).





%% @spec geometric_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the geometric mean of a list of numbers. ```1> scutil:geometric_mean([1,2,3,4,5]).
%% 2.6051710846973517''' The naive approach ```geometric_mean(List) -> math:pow(scutil:list_product(List), 1/length(List)).''' is not used because it accumulates error very quickly, and is as such unsuited to huge lists.

%% @see arithmetic_mean/1
%% @see harmonic_mean/1
%% @see gmean_vector_normal/1

%% @since Version 34

geometric_mean([]) ->
    
    0.0;



geometric_mean(List) when is_list(List) ->
    
    math:exp(scutil:arithmetic_mean([math:log(X)||X<-List])).





%% @spec harmonic_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the harmonic mean of a list of numbers. ```1> scutil:harmonic_mean([1,2,3,4,5]).
%% 2.18978102189781'''

%% @see arithmetic_mean/1
%% @see geometric_mean/1
%% @see hmean_vector_normal/1

%% @since Version 35

harmonic_mean([]) ->
    
    0.0;



harmonic_mean(List) when is_list(List) ->

    length(List) / lists:sum([ 1/X || X<-List ]).





%% @spec weighted_arithmetic_mean(InputList::weightlist()) -> float()

%% @doc {@section Statistics} Take the weighted arithmetic mean of the input values. ```1> scutil:weighted_arithmetic_mean([ {8,1}, {3,4}, {16,1} ]).
%% 6.0'''

%% @see arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 44

weighted_arithmetic_mean(List) when is_list(List) ->
    
    weighted_arithmetic_mean(List, 0, 0).



weighted_arithmetic_mean([], Num, Denom) ->

    Num/Denom;



weighted_arithmetic_mean( [{V,W} | Tail], Num, Denom) ->

    weighted_arithmetic_mean(Tail, Num+(W*V), Denom+W).





