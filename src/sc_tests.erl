
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2011 John Haugeland
%% @version $Revision$
%% @doc scutil test set.

-module(sc_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").





extrema_test_() ->

    { "Extrema tests", [

        {"8,6,7,5,3,0,9",      ?_assert( {0,9}      =:= sc:extrema( [8,6,7,5,3,0,9] ) ) },
        {"1,2,3,4",            ?_assert( {1,4}      =:= sc:extrema( [1,2,3,4]       ) ) },
        {"-1,-2,-3",           ?_assert( {-3,-1}    =:= sc:extrema( [-1,-2,-3]      ) ) },
        {"-1.1,0,1.1",         ?_assert( {-1.1,1.1} =:= sc:extrema( [-1.1,1.1]      ) ) },
        {"a,b,c",              ?_assert( {a,c}      =:= sc:extrema( [a,b,c]         ) ) },
        {"1,a,{}",             ?_assert( {1,{}}     =:= sc:extrema( [1,a,{}]        ) ) },
        {"1",                  ?_assert( {1,1}      =:= sc:extrema( [1]             ) ) },

        {"[] error undefined", ?_assertError(function_clause, sc:extrema([]) ) }

    ] }.





prop_key_duplicate_correct_length() ->

    ?FORALL( {L,I}, {proper_types:non_neg_integer(), proper_types:any()}, abs(L) == length( sc:key_duplicate([ {abs(L),I} ]) ) ).





key_duplicate_test_() ->

    { "Key duplicate tests", [

        {"[ ]",                        ?_assert( []          =:= sc:key_duplicate([ ])             ) },
        {"[ {2,a} ]",                  ?_assert( [a,a]       =:= sc:key_duplicate([ {2,a} ])       ) },
        {"[ {2,a},{3,b} ]",            ?_assert( [a,a,b,b,b] =:= sc:key_duplicate([ {2,a},{3,b} ]) ) },

        {"Regression: key empty-list", ?_assert( [ [], [] ]  =:= sc:key_duplicate([ {2, []} ])     ) },

        {"Stochastic: correct length", ?_assert( true        =:= proper:quickcheck(prop_key_duplicate_correct_length()) ) }

    ] }.





prop_rotate_list_correct_length() ->

    ?FORALL( {R,L},
             {int(), list(proper_types:any())},
             length(L) == length(sc:rotate_list(R,L))
           ).





prop_rotate_list_same_histo() ->

    ?FORALL( {R,L},
             {int(), list(proper_types:any())},
             sc:histograph(L) == sc:histograph(sc:rotate_list(R,L))
           ).





rotate_list_test_() ->

    { "Rotate list tests", [

        {"0,  [ ]",       ?_assert( []      =:= sc:rotate_list(0,  [ ])       ) },
        {"1,  [ ]",       ?_assert( []      =:= sc:rotate_list(1,  [ ])       ) },
        {"-1, [ ]",       ?_assert( []      =:= sc:rotate_list(-1, [ ])       ) },

        {"0,  [ a,b,c ]", ?_assert( [a,b,c] =:= sc:rotate_list(0,  [ a,b,c ]) ) },
        {"1,  [ a,b,c ]", ?_assert( [b,c,a] =:= sc:rotate_list(1,  [ a,b,c ]) ) },
        {"-1, [ a,b,c ]", ?_assert( [c,a,b] =:= sc:rotate_list(-1, [ a,b,c ]) ) },
        {"3,  [ a,b,c ]", ?_assert( [a,b,c] =:= sc:rotate_list(3,  [ a,b,c ]) ) },
        {"-3, [ a,b,c ]", ?_assert( [a,b,c] =:= sc:rotate_list(-3, [ a,b,c ]) ) },
        {"9,  [ a,b,c ]", ?_assert( [a,b,c] =:= sc:rotate_list(9,  [ a,b,c ]) ) },

        {"Stochastic: correct length",  ?_assert( true  =:= proper:quickcheck(prop_rotate_list_correct_length()) ) },
        {"Stochastic: same histograph", ?_assert( true  =:= proper:quickcheck(prop_rotate_list_same_histo()) ) }

    ] }.





index_of_first_test_() ->

    { "Index of first tests", [

        {"0, [ ]",       ?_assert( undefined =:= sc:index_of_first(0, [ ])       ) },
        {"b, [ a,b,c ]", ?_assert( 2         =:= sc:index_of_first(b, [ a,b,c ]) ) },
        {"g, [ a,b,c ]", ?_assert( undefined =:= sc:index_of_first(g, [ a,b,c ]) ) }

    ] }.





rotate_to_first_test_() ->

    { "Rotate to first tests", [

        {"3, [1,2,3,4]", ?_assert( [3,4,1,2] =:= sc:rotate_to_first( 3, [1,2,3,4]  ) ) },
        {"4, [1,2,3,4]", ?_assert( [4,1,2,3] =:= sc:rotate_to_first( 4, [1,2,3,4]  ) ) },
        {"1, [1,2,3,4]", ?_assert( [1,2,3,4] =:= sc:rotate_to_first( 1, [1,2,3,4]  ) ) },

        {"f, [1,2,3,4]", ?_assert( no_such_element =:= sc:rotate_to_first( f, [1,2,3,4]  ) ) }

    ] }.





rotate_to_last_test_() ->

    { "Rotate to last tests", [

        {"3, [1,2,3,4]", ?_assert( [4,1,2,3] =:= sc:rotate_to_last( 3, [1,2,3,4]  ) ) },
        {"1, [1,2,3,4]", ?_assert( [2,3,4,1] =:= sc:rotate_to_last( 1, [1,2,3,4]  ) ) },
        {"4, [1,2,3,4]", ?_assert( [1,2,3,4] =:= sc:rotate_to_last( 4, [1,2,3,4]  ) ) },

        {"f, [1,2,3,4]", ?_assert( no_such_element =:= sc:rotate_to_last( f, [1,2,3,4]  ) ) }

    ] }.





flag_sets_test_() ->

    { "Flag sets tests", [

        {"[1,2,3]",  ?_assert( [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]  =:= sc:flag_sets( [1,2,3] )  ) },
        {"[1]",      ?_assert( [[],[1]]                                    =:= sc:flag_sets( [1] )      ) },
        {"[]",       ?_assert( [[]]                                        =:= sc:flag_sets( [] )       ) },
        {"[{[{}]}]", ?_assert( [[],[{[{}]}]]                               =:= sc:flag_sets( [{[{}]}] ) ) }

    ] }.





member_sets_test_() ->

    { "Member sets tests", [

        {"[]",                           ?_assert( [[]]                         =:= sc:member_sets( [] )                           ) },
        {"[ [] ]",                       ?_assert( [[]]                         =:= sc:member_sets( [ [] ] )                       ) },
        {"[ [1],[2,3] ]",                ?_assert( [[1,2],[1,3]]                =:= sc:member_sets( [ [1],[2,3] ] )                ) },
        {"[ [1],[2,3] ], no_absence",    ?_assert( [[1,2],[1,3]]                =:= sc:member_sets( [ [1],[2,3] ], no_absence )    ) },
        {"[ [1],[2,3] ], allow_absence", ?_assert( [[],[2],[3],[1],[1,2],[1,3]] =:= sc:member_sets( [ [1],[2,3] ], allow_absence ) ) }

    ] }.





count_of_test_() ->

    { "Count of tests", [

        {"3, [1,2,3,4,5]", ?_assert( 1 =:= sc:count_of(3, [1,2,3,4,5]) )},
        {"6, [1,2,3,4,5]", ?_assert( 0 =:= sc:count_of(6, [1,2,3,4,5]) )},
        {"2, [1,2,3,2,1]", ?_assert( 2 =:= sc:count_of(2, [1,2,3,2,1]) )},
        {"b, [1,a,2,b,3]", ?_assert( 1 =:= sc:count_of(b, [1,a,2,b,3]) )}

    ] }.





list_intersection_test_() ->

    { "List intersection tests", [

        {"[3,1,4],[1,5,9]", ?_assert( [1]     =:= sc:list_intersection([3,1,4],[1,5,9]) )},
        {"[3,1,4],[2,5,9]", ?_assert( []      =:= sc:list_intersection([3,1,4],[2,5,9]) )},
        {"[3,1,4],[1,4,3]", ?_assert( [4,3,1] =:= sc:list_intersection([3,1,4],[1,4,3]) )},
        {"[3,1,4],[3,1,4]", ?_assert( [4,3,1] =:= sc:list_intersection([3,1,4],[3,1,4]) )},
        {"[3,a,4],[a,5,3]", ?_assert( [a,3]   =:= sc:list_intersection([3,a,4],[a,5,3]) )}

    ] }.





zip_n_test_() ->

    { "Zip N test", [

        {"[ [1,2,3],[a,b,c] ]",                 ?_assert( [{1,a},  {2,b},  {3,c}]         =:= sc:zip_n([ [1,2,3],[a,b,c] ])                 )},
        {"[ [1,2,3],[a,b,c],[x,y,z] ]",         ?_assert( [{1,a,x},{2,b,y},{3,c,z}]       =:= sc:zip_n([ [1,2,3],[a,b,c],[x,y,z] ])         )},
        {"[ [1,2,3],[a,b,c],[x,y,z],[d,e,f] ]", ?_assert( [{1,a,x,d},{2,b,y,e},{3,c,z,f}] =:= sc:zip_n([ [1,2,3],[a,b,c],[x,y,z],[d,e,f] ]) )},
        {"[ ]",                                 ?_assert( []                              =:= sc:zip_n([ ])                                 )}

    ] }.





combinations_test_() ->

    { "Combinations test", [

        {"0, [a,b,c,d]", ?_assert( []                                    =:= sc:combinations(0, [a,b,c,d]) )},
        {"1, [a,b,c,d]", ?_assert( [[a],[b],[c],[d]]                     =:= sc:combinations(1, [a,b,c,d]) )},
        {"2, [a,b,c,d]", ?_assert( [[a,b],[a,c],[a,d],[b,c],[b,d],[c,d]] =:= sc:combinations(2, [a,b,c,d]) )},
        {"3, [a,b,c,d]", ?_assert( [[a,b,c],[a,b,d],[a,c,d],[b,c,d]]     =:= sc:combinations(3, [a,b,c,d]) )},
        {"4, [a,b,c,d]", ?_assert( [[a,b,c,d]]                           =:= sc:combinations(4, [a,b,c,d]) )},
        {"5, [a,b,c,d]", ?_assert( []                                    =:= sc:combinations(5, [a,b,c,d]) )}

    ] }.





expand_labels_test_() ->

    Villains = [ {villain,lex_luthor}, {villain,sinistar}, {villain,gargamel} ],
    VLabel   = [ {villain, [lex_luthor,sinistar,gargamel] } ],

    HandV    = [ {hero,superman}, {hero,tinyship}, {hero,papa_smurf}, {villain,lex_luthor}, {villain,sinistar}, {villain,gargamel} ],
    HVLabel  = [ {hero, [superman,tinyship,papa_smurf]}, {villain, [lex_luthor,sinistar,gargamel]} ],

    { "Expand labels test", [

        {"[]",                    ?_assert( []                        =:= sc:expand_labels([])                    ) },
        {"[{1,[a,b]}]",           ?_assert( [{1,a},{1,b}]             =:= sc:expand_labels([{1,[a,b]}])           ) },
        {"[{1,[a,b]},{2,[c,d]}]", ?_assert( [{1,a},{1,b},{2,c},{2,d}] =:= sc:expand_labels([{1,[a,b]},{2,[c,d]}]) ) },
        { "Doc ex 1 - Villains",  ?_assert( Villains                  =:= sc:expand_labels(VLabel)                ) },
        { "Doc ex 2 - Heroes",    ?_assert( HandV                     =:= sc:expand_labels(HVLabel)               ) }

    ] }.





permute_test_() ->

    { "Permute tests", [

        {"[1,2,3]",   ?_assert([[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] =:= sc:permute([1,2,3])   )},
        {"[1,2,3],2", ?_assert([[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]             =:= sc:permute([1,2,3],2) )},
        {"[]",        ?_assert([]                                                =:= sc:permute([])        )}

    ] }.





shared_keys_test_() ->

    { "Shared keys tests", [

        {"[],[]",                                   ?_assert([]                =:= sc:shared_keys([],[])                                   )},
        {"[{1,a},{2,a},{3,a}],[{1,b},{3,b},{4,b}]", ?_assert([{1,a,b},{3,a,b}] =:= sc:shared_keys([{1,a},{2,a},{3,a}],[{1,b},{3,b},{4,b}]) )},
        {"[{1,a},{2,a}],[{3,b},{4,b}]",             ?_assert([]                =:= sc:shared_keys([{1,a},{2,a}],[{3,b},{4,b}])             )}

    ] }.





list_product_test_() ->

    { "List product tests", [

        {"[1,2,3]",   ?_assert(6    =:= sc:list_product([1,2,3]))},
        {"[1,2,5.4]", ?_assert(10.8 =:= sc:list_product([1,2,5.4]))},
        {"[1]",       ?_assert(1    =:= sc:list_product([1]))},
        {"[]",        ?_assert(1    =:= sc:list_product([]))}

    ] }.





even_or_odd_test_() ->

    { "Even or odd tests", [

        {"-2", ?_assert(even =:= sc:even_or_odd(-2))},
        {"-1", ?_assert(odd  =:= sc:even_or_odd(-1))},
        {"0",  ?_assert(even =:= sc:even_or_odd(0) )},
        {"1",  ?_assert(odd  =:= sc:even_or_odd(1) )},
        {"2",  ?_assert(even =:= sc:even_or_odd(2) )}

    ] }.





implode_test_() ->

    { "Implode tests", [

        {"a,b,c",    ?_assert("a,b,c"    =:= sc:implode(",", ["a",  "b",  "c" ]))},
        {"ab,cd,ef", ?_assert("ab,cd,ef" =:= sc:implode(",", ["ab", "cd", "ef"]))},
        {",,",       ?_assert(",,"       =:= sc:implode(",", ["",   "",   ""  ]))},
        {"",         ?_assert(""         =:= sc:implode("",  ["",   "",   ""  ]))}

    ] }.





explode_test_() ->

    { "Explode tests", [

        {"a,b,c",    ?_assert(["a",  "b",  "c" ] =:= sc:explode(",", "a,b,c"))},
        {"ab,cd,ef", ?_assert(["ab", "cd", "ef"] =:= sc:explode(",", "ab,cd,ef"))},

        {"Doc ex 1 - num comma",            ?_assert(["1","2","5","10","20"]                   =:= sc:explode(",", "1,2,5,10,20"))},
        {"Doc ex 2 - name space",           ?_assert(["John","Jacob","Jingleheimer","Schmidt"] =:= sc:explode(" ", "John Jacob Jingleheimer Schmidt"))},
        {"Doc ex 3 - dir semi,space",       ?_assert(["North","East","South","West"]           =:= sc:explode("; ", "North; East; South; West"))},
        {"Doc ex 4 - no such delim",        ?_assert(["I am the monarch of the sea"]           =:= sc:explode("No such delimiter", "I am the monarch of the sea"))},
        {"Doc ex 5 - comma gaps",           ?_assert(["", "", ""]                              =:= sc:explode(",", ",,"))},
        {"Doc ex 6 - atoms",                ?_assert([[alpha],[gamma,delta],[epsilon]]         =:= sc:explode([beta], [alpha, beta, gamma, delta, beta, epsilon]))},

        {"Doc ex 7 - [] delim throws",      ?_assertError( badarg, sc:explode([], [alpha, beta, gamma, delta, beta, epsilon])   ) },
        {"Doc ex 8 - nonlist delim throws", ?_assertError( badarg, sc:explode(beta, [alpha, beta, gamma, delta, beta, epsilon]) ) }

    ] }.





prop_ceil_ints_as_floats_identity() ->

    ?FORALL( I, proper_types:integer(), sc:ceiling(I*1.0) =:= I ).





prop_ceil_floats_smaller_within_1() ->

    ?FORALL( R, proper_types:real(), (sc:ceiling(R) - R) < 1 andalso (sc:ceiling(R) - R) >= 0 ).





prop_ceil_always_gives_integers() ->

    ?FORALL( N, proper_types:number(), is_integer(sc:ceiling(N)) ).





ceil_test_() ->

    { "Ceil/Ceiling tests", [

        {"0.5",  ?_assert(  1 =:= sc:ceil(0.5)  ) },
        {"0",    ?_assert(  0 =:= sc:ceil(0)    ) },
        {"0.0",  ?_assert(  0 =:= sc:ceil(0.0)  ) },
        {"1.0",  ?_assert(  1 =:= sc:ceil(1.0)  ) },
        {"-1.0", ?_assert( -1 =:= sc:ceil(-1.0) ) },
        {"-1.5", ?_assert( -1 =:= sc:ceil(-1.5) ) },
        {"-1",   ?_assert( -1 =:= sc:ceil(-1)   ) },
        {"1",    ?_assert(  1 =:= sc:ceil(1)    ) },

        {"Stochastic: all integers-as-floats are identity", ?_assert( true =:= proper:quickcheck(prop_ceil_ints_as_floats_identity()) ) },
        {"Stochastic: all floats are smaller within 1",     ?_assert( true =:= proper:quickcheck(prop_ceil_floats_smaller_within_1()) ) },
        {"Stochastic: all numbers give integer results",    ?_assert( true =:= proper:quickcheck(prop_ceil_always_gives_integers())   ) }

    ] }.





prop_floor_ints_as_floats_identity() ->

    ?FORALL( I, proper_types:int(), sc:floor(I*1.0) =:= I ).





prop_floor_floats_larger_within_1() ->

    ?FORALL( R, proper_types:real(), (R - sc:floor(R)) < 1 andalso (R - sc:floor(R)) >= 0 ).





prop_floor_always_gives_integers() ->

    ?FORALL( N, proper_types:number(), is_integer(sc:floor(N)) ).





floor_test_() ->

    { "Floor tests", [

        {"0.5",  ?_assert(  0 =:= sc:floor(0.5)  ) },
        {"0",    ?_assert(  0 =:= sc:floor(0)    ) },
        {"0.0",  ?_assert(  0 =:= sc:floor(0.0)  ) },
        {"1.0",  ?_assert(  1 =:= sc:floor(1.0)  ) },
        {"-1.0", ?_assert( -1 =:= sc:floor(-1.0) ) },
        {"-1.5", ?_assert( -2 =:= sc:floor(-1.5) ) },
        {"-1",   ?_assert( -1 =:= sc:floor(-1)   ) },
        {"1",    ?_assert(  1 =:= sc:floor(1)    ) },

        {"Stochastic: all integers-as-floats are identity", ?_assert( true =:= proper:quickcheck(prop_floor_ints_as_floats_identity()) ) },
        {"Stochastic: all floats are larger within 1",      ?_assert( true =:= proper:quickcheck(prop_floor_floats_larger_within_1())  ) },
        {"Stochastic: all numbers give integer results",    ?_assert( true =:= proper:quickcheck(prop_floor_always_gives_integers())   ) }

    ] }.





to_lines_test_() ->

    { "To lines tests", [

        { "Doc ex 1 - Readable",          ?_assert( ["one","two","three","four","five"] =:= sc:to_lines("one\rtwo\nthree\r\nfour\r\r\rfive") ) },

        { "Doc ex 4 - Single DOS-style",  ?_assert( ["a","b"] =:= sc:to_lines("a\r\nb") ) },
        {            "Double DOS-style",  ?_assert( ["a","b"] =:= sc:to_lines("a\r\n\r\nb") ) },

        { "Doc ex 2 - Single unix-style", ?_assert( ["a","b"] =:= sc:to_lines("a\nb") ) },
        { "Doc ex 3 - Double unix-style", ?_assert( ["a","b"] =:= sc:to_lines("a\n\nb") ) },

        { "Doc ex 5 - Single mac-style",  ?_assert( ["a","b"] =:= sc:to_lines("a\rb") ) },
        {            "Double mac-style",  ?_assert( ["a","b"] =:= sc:to_lines("a\r\rb") ) },

        {            "Backwards",         ?_assert( ["a","b"] =:= sc:to_lines("a\n\rb") ) },

        { "Doc ex 6 - Long stack",        ?_assert( ["a","b","c","d","e"] =:= sc:to_lines("a\rb\nc\r\nd\n\r\r\ne") ) },
        { "Doc ex 7 - Empty string",      ?_assert( []                    =:= sc:to_lines("") ) },
        { "Doc ex 8 - Just rseqs",        ?_assert( []                    =:= sc:to_lines("\r\n\r\r\n\n\r") ) }

    ] }.





naive_bayes_likelihood_test_() ->

    LongForm = [ {contributing_difference,0.7000000000000001}, {likelihood_featured,0.8}, {likelihood_nonfeatured,0.1}, {evident_feature_likelihood,0.9230769230769231} ],

    { "Naive bayes likelihood tests", [

        { "Doc ex 1 - implicit", ?_assert( 0.9230769230769231 =:= sc:naive_bayes_likelihood(48, 60, 4, 40)         ) },
        { "Doc ex 2 - simple",   ?_assert( 0.9230769230769231 =:= sc:naive_bayes_likelihood(48, 60, 4, 40, simple) ) },
        { "Doc ex 3 - full",     ?_assert( LongForm           =:= sc:naive_bayes_likelihood(48, 60, 4, 40, full)   ) }

    ] }.





range_scale_test_() ->

    { "Range scale tests", [

        { "Doc ex 1 - range",     ?_assert( 2.0  =:= sc:range_scale([3, 4, 5, 6])         ) },
        { "Doc ex 2 - bookends",  ?_assert( 2.0  =:= sc:range_scale([3, 6])               ) },
        { "Doc ex 3 - backwards", ?_assert( 2.0  =:= sc:range_scale([6, 5, 3])            ) },
        { "Doc ex 4 - float",     ?_assert( 2.5  =:= sc:range_scale([3, 4, 5, 6, 7, 7.5]) ) },
        { "Doc ex 5 - irregular", ?_assert( 33.0 =:= sc:range_scale([3, 10, 12, 99])      ) },
        { "Doc ex 6 - repeat",    ?_assert( 1.0  =:= sc:range_scale([3, 3, 3])            ) }

    ] }.





expected_value_test_() ->

    { "Range scale tests", [

        { "Doc ex 1 - 1..6=3.5",                     ?_assert( 3.5                 =:= sc:expected_value([1,2,3,4,5,6])         ) },
        { "Doc ex 2 - 5x1,1x10=2.5",                 ?_assert( 2.5                 =:= sc:expected_value([ {1,5}, {10,1} ])     ) },
        { "Doc ex 3 - 1,1,1,1,1;1x10=2.5",           ?_assert( 2.5                 =:= sc:expected_value([ 1,1,1,1,1, {10,1} ]) ) },
        { "Doc ex 4 - 8x1,7x-1=0.66",                ?_assert( 0.06666666666666667 =:= sc:expected_value([ {1,8}, {-1,7} ])     ) },

        { "Doc assertion - [] throws",               ?_assertError( badarith, sc:expected_value([]) ) },
        { "Doc assertion - [ {1,0}, {2,0} ] throws", ?_assertError( badarith, sc:expected_value([]) ) }

    ] }.





nybble_to_hex_test_() ->

    { "Nybble to hex tests", [

        { "0n", ?_assert( $0 =:= sc:nybble_to_hex(0)  ) },
        { "1n", ?_assert( $1 =:= sc:nybble_to_hex(1)  ) },
        { "2n", ?_assert( $2 =:= sc:nybble_to_hex(2)  ) },
        { "3n", ?_assert( $3 =:= sc:nybble_to_hex(3)  ) },
        { "4n", ?_assert( $4 =:= sc:nybble_to_hex(4)  ) },
        { "5n", ?_assert( $5 =:= sc:nybble_to_hex(5)  ) },
        { "6n", ?_assert( $6 =:= sc:nybble_to_hex(6)  ) },
        { "7n", ?_assert( $7 =:= sc:nybble_to_hex(7)  ) },
        { "8n", ?_assert( $8 =:= sc:nybble_to_hex(8)  ) },
        { "9n", ?_assert( $9 =:= sc:nybble_to_hex(9)  ) },
        { "an", ?_assert( $a =:= sc:nybble_to_hex(10) ) },
        { "bn", ?_assert( $b =:= sc:nybble_to_hex(11) ) },
        { "cn", ?_assert( $c =:= sc:nybble_to_hex(12) ) },
        { "dn", ?_assert( $d =:= sc:nybble_to_hex(13) ) },
        { "en", ?_assert( $e =:= sc:nybble_to_hex(14) ) },
        { "fn", ?_assert( $f =:= sc:nybble_to_hex(15) ) }

    ] }.





prop_arithmetic_mean_result_numeric() ->

    ?FORALL( Ln, list(number()), true =:= is_number(sc:arithmetic_mean(Ln)) ).





prop_arithmetic_mean_between_eq_extrema_p(L) ->

    {Lo,Hi} = sc:extrema(L),
    sc:is_between(sc:arithmetic_mean(L), Lo, Hi, inclusive).


prop_arithmetic_mean_between_eq_extrema() ->

    ?FORALL( Ln, non_empty(list(number())), true =:= prop_arithmetic_mean_between_eq_extrema_p(Ln) ).





arithmetic_mean_test_() ->

    { "Arithmetic mean tests", [

        { "Doc ex 1 - 1..5=3.0",  ?_assert(  3.0 =:= sc:arithmetic_mean([1,2,3,4,5]) ) },
        { "Doc ex 1 - []=0.0",    ?_assert(  0.0 =:= sc:arithmetic_mean([])          ) },
        { "Doc ex 1 - 4x2=2.0",   ?_assert(  2.0 =:= sc:arithmetic_mean([2,2,2,2])   ) },
        { "Doc ex 1 - -3,2=-0.5", ?_assert( -0.5 =:= sc:arithmetic_mean([-3,2])      ) },

        {"Stochastic: all results are numbers",        ?_assert( true =:= proper:quickcheck(prop_arithmetic_mean_result_numeric())     ) },
        {"Stochastic: all results between-eq extrema", ?_assert( true =:= proper:quickcheck(prop_arithmetic_mean_between_eq_extrema()) ) }

    ] }.





prop_tuple_duplicate_result_tuple() ->

    ?FORALL( {Sz,Inp}, {proper_types:non_neg_integer(), proper_types:any()}, true =:= is_tuple(sc:tuple_duplicate(Sz,Inp)) ).





prop_tuple_duplicate_correct_size() ->

    ?FORALL( {Sz,Inp}, {proper_types:non_neg_integer(), proper_types:any()}, Sz =:= size(sc:tuple_duplicate(Sz,Inp)) ).





prop_tuple_duplicate_first_item() ->

    % this excludes 0-size tuples for obvious reasons

    ?FORALL( {Sz,Inp}, {proper_types:pos_integer(), proper_types:any()}, Inp =:= element(1, sc:tuple_duplicate(Sz,Inp)) ).





tuple_duplicate_test_() ->

    { "Tuple duplicate tests", [

        {"3,hi", ?_assert( {hi,hi,hi} =:= sc:tuple_duplicate(3, hi) ) },
        {"0,hi", ?_assert( {}         =:= sc:tuple_duplicate(0, hi) ) },

        {"Stochastic: all results are tuples",                ?_assert( true =:= proper:quickcheck(prop_tuple_duplicate_result_tuple()) ) },
        {"Stochastic: all results have correct member count", ?_assert( true =:= proper:quickcheck(prop_tuple_duplicate_correct_size()) ) },
        {"Stochastic: first item is correct",                 ?_assert( true =:= proper:quickcheck(prop_tuple_duplicate_first_item()) ) },

        {"-1,hi error undefined", ?_assertError(function_clause, sc:tuple_duplicate(-1, hi) ) }

    ] }.
