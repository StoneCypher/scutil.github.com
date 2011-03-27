
%%%%%%%%%
%%
%%  Stonecypher's Erlang Utility Library - reboot 2011
%%  Written by John Haugeland, http://fullof.bs/
%%
%%  To skip to code, which is hundreds of lines down, search for "-module("
%%  without quotes.
%%
%%  This was last tested by the author in Erl OTP 14b1 / 5.8.1.1 .  Please
%%  run sc:test(deep) or sc:test([verbose,deep]) before using, to verify that
%%  this library functions correectly in the current Erlang virtual machine
%%  and environment.  Removing deep will execute a faster, less trustworthy
%%  subset of the tests.  Removing verbose will dump much less information to
%%  the console.
%%
%%  There is significant documentation.  With paths appropriate for your
%%  system, call sc:gen_docs("/path/to/source", "/path/for/docs/to/live")
%%  to generate.  Do not use trailing slashes.  Windows paths are fine;
%%  remember to use \\ , because it's a string and you're quoting the
%%  backslash.  Automatic documentation generation via edoc will then
%%  generate HTML docs.
%%
%%  For example, if you have this source in /projects/scutil or
%%  c:\projects\scutil, and you wanted the documentation in
%%  /project/scutil/erl/src/docs or c:\projects\scutil\docs
%%
%%  Past here, documentation should be generally be read in the HTML format.





%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since September 14, 2007

%% @todo burn out sc.
%% @todo burn out scutil:
%% @todo burn out @section
%% @todo burn out was





%% @doc This is the 2011 revamp of <a href="http://scutil.com/" target="_blank">scutil</a>'s erlang library.
%%
%% <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <a href="http://fullof.bs/">I'm</a> modernizing my library.  Originally it got too big, so I split it up.  However, that's proven more trouble than it's worth - I often found myself asking whether the thing I wanted was
%% in `_math', `_correlate', `_statistics', `_lists', `_operators', etc.  No more.  It's all getting dumped into `sc.erl' and `sc_tests.erl'.  Prototypes are pushed into `sc_prototype.erl'; that is
%% code which is either insufficiently tested or unfinished.  It's moving to eunit; sc_test is being dumped.  I'm taking a lot of the customization stuff out of the generated docs, too.
%% There was always an emphasis on testing, but it's getting taken much more seriously this time.  I am also no longer a language novice; I can replace at least some of the naive implementations
%% with marginally less naive ones, break fewer conventions, replicate fewer extant library functions, choose better naming and a more appropriate argument order, et cetera.
%%
%% This also means I can completely ditch all the remnants of the botched move to packages, provide radically less automation which is radically more effective, and generally do a better job of
%% presenting the actual volume of functionality present here.  We can get the coverage tools out.  We can run prototypical work in a single separate file, to distinguish quality.  Etc.
%%
%% Generally, this is intended to just be an all around improvement and replacement for the library I was pretty happy with, but which was showing strain.
%%
%% As always, this work remains open source under the MIT license, because free isn't real unless it's free for everyone.  <a href="http://scutil.com/license/">http://scutil.com/license/</a>
%%
%% ScUtil is free.  However, I'd like to know where it's ended up.  Therefore, please consider mail to stonecypher@gmail.com with text saying where this went a form of "registration."  This is not required.
%%
%% @end

-module(sc).





-export([

    extrema/1,
    key_duplicate/1,
    index_of_first/2,  %% todo needs index of last then too, no?
    count_x/2,
    combinations/2,
    flag_sets/1,
    list_product/1,
    sanitize_tokens/2,
    naive_bayes_likelihood/4,
    caspers_jones_estimate/1,
    range_scale/1,
    even_or_odd/1,
    absolute_difference/2,
    mod/2,
    factorial/1,
    mersenne_prime/1,
    factorize/1,
    centroid/1,
    nearest_to/2,
    svn_revision/1,
    module_atoms/1,
    key_cluster/2,
    split_at/2,
    is_postfix/2,
    walk_unique_pairings/2,
    count_of/2,
    integer_to_radix_list/2,
    receive_one/0,

    union/1,
      union/2,
      union/3,
      union/4,

    send_receive/2,
      send_receive/3,
      send_receive_masked/3,
      send_receive_masked/4,

    euclidean_distance/2,  % todo 3d version?

    list_to_term/1,
      list_to_number/1,

    io_list_to_hex_string/1,
      nybble_to_hex/1,
      byte_to_hex/1,
      hex_to_int/1,

    halstead_complexity/4,
      halstead_complexity/5,

    simple_ranking/1,
      tied_ranking/1,
      tied_ordered_ranking/1,

    spearman_correlation/1,
      spearman_correlation/2,

    pearson_correlation/1,
      pearson_correlation/2,

    kendall_correlation/1,
      kendall_correlation/2,

    eval/1,
      eval/2,

    every_member_representation/1,
      every_member_representation/2,
      every_flag_representation/1,

    zip_n/1,
      zip_n/2,

    expand_label/1,
      expand_labels/1,

    differences/1,
      first_difference/1,
      second_difference/1,
      third_difference/1,
      nth_difference/2,

    elements/2,
      elements/3,
      elements/4,

    reverse_map/2,
      reverse_filter/2,
      reverse_map_filter/3,

    keygroup/2,
      keygroup/3,

    first_pos/2,
      first_pos/3,

    last_while_pos/2,
      last_while_pos/3,

    partition_by_residue/2,

    all_neighbor_pairs/1,
      all_neighbor_pairs/2,

    distinct_neighbor_pairs/1,
      distinct_neighbor_pairs/2,

    abstract/1,
      abstract/2,
      abstract_attributes/1,

    entrypoints/1,
      entrypoints/2,

    abstract_function/2,
      abstract_functions/1,

    function_stats/1,
      function_point_count/1,

    module_attribute/1,
      module_attribute/2,

    module_feature/2,

    by_distance/2,
      by_distance_raw/2,

    alarm_set/3,
      alarm_terminate/1,

    is_sorted_list/1,
      is_unique_list/1,

    floor/1,
      ceil/1,
      ceiling/1,

    square/1,
      cube/1,
%     nth_root/2,    % comeback todo

    instant_runoff_vote/1,

    dstat/2,
      extended_dstat/2,

    moment/2,
      moments/1,
      moments/2,

    root_mean_square/1,
      root_sum_square/1,
      vector_magnitude/1,

    central_moment/2,
      central_moments/1,
      central_moments/2,

    histograph/1,

    skewness/1,
      kurtosis/1,

    median/1,
      mode/1,

    expected_value/1,

    median_absolute_deviation/1,
%     median_absolute_value/1,    comeback todo

    arithmetic_mean/1,
      geometric_mean/1,
      harmonic_mean/1,
      weighted_arithmetic_mean/1,

    amean_vector_normal/1,
      gmean_vector_normal/1,
      hmean_vector_normal/1,

    zipf_nearness/1,
      zipf_estimate_list/1,
      zipf_position_estimate/2,

    bandwidth_calc/1,
      bandwidth_calc/2,

    member_sets/1,
      member_sets/2,

    list_intersection/2,
      list_intersection/3,

    shared_keys/1,
      shared_keys/2,
      shared_keys/3,

    rotate_list/2,
      rotate_to_first/2,
      rotate_to_last/2,

    permute/1,
      permute/2,

    test/0,
      test/1,

    gen_docs/0,
      gen_docs/2

]).





-include_lib("eunit/include/eunit.hrl").





%% @spec gen_docs() -> ok|{'EXIT',any()}
%% @equiv gen_docs("/projects/scutil/erl/src", "/projects/scutil/erl/src/docs")
%% @doc Generates library documentation using the paths appropriate for the author's PC; you almost certainly want {@link gen_docs/2} instead.  ```1> sc:gen_docs().
%% ok'''
%% @since 458

gen_docs() ->

    gen_docs("/projects/scutil/erl/src", "/projects/scutil/erl/doc").





%% @spec gen_docs(WhereIsSrc::string(), WhereToPutDocs::string()) -> ok|{'EXIT',any()}
%% @doc Generates library documentation from and to the specified paths `WhereIsSrc' and `WhereToPutDocs' respectively.  Do not use trailing slashes.  Windows paths are okay; remember to double your
%% backslashes, as backslashes in strings need to be quoted.  ```1> sc:gen_docs("/projects/scutil/erl/src", "/projects/scutil/erl/src/docs").
%% ok'''
%% @since 458

gen_docs(WhereIsSrc, WhereToPutDocs) ->

    filelib:ensure_dir(WhereToPutDocs),
    edoc:files([WhereIsSrc++"/sc.erl", WhereIsSrc++"/sc_tests.erl"], [{dir, WhereToPutDocs}, {new,true}]).
%   edoc:files([WhereIsSrc++"/sc.erl", WhereIsSrc++"/sc_proto.erl", WhereIsSrc++"/sc_tests.erl", WhereIsSrc++"/sc_proto_tests.erl"], [{dir, WhereToPutDocs}, {new,true}]).





%% @spec test() -> ok|error
%% @doc Runs the test suite in terse form. ```1> sc:test().
%%   All 9 tests passed.
%% ok'''
%% @since 458

test() ->

    eunit:test(sc).





%% @spec test(verbose) -> ok|error
%% @doc Runs the test suite in verbose form.  Also responds to [verbose] to be more familiar to eunit devs.  An (ancient) example of output: ```1> sc:test(verbose).
%% ======================== EUnit ========================
%% module 'sc'
%%   module 'sc_tests'
%%     Index of first tests
%%       sc_tests:73: index_of_first_test_ (0,  [ ])...ok
%%       sc_tests:74: index_of_first_test_ (b,  [ a,b,c ])...ok
%%       sc_tests:75: index_of_first_test_ (g,  [ a,b,c ])...ok
%%       [done in 0.046 s]
%%     Rotate list tests
%%       sc_tests:52: rotate_list_test_ (0,  [ ])...ok
%%       sc_tests:53: rotate_list_test_ (1,  [ ])...ok
%%       sc_tests:54: rotate_list_test_ (-1, [ ])...ok
%%       sc_tests:56: rotate_list_test_ (0,  [ a,b,c ])...ok
%%       sc_tests:57: rotate_list_test_ (1,  [ a,b,c ])...ok
%%       sc_tests:58: rotate_list_test_ (-1, [ a,b,c ])...ok
%%       sc_tests:59: rotate_list_test_ (3,  [ a,b,c ])...ok
%%       sc_tests:60: rotate_list_test_ (-3, [ a,b,c ])...ok
%%       sc_tests:61: rotate_list_test_ (9,  [ a,b,c ])...ok
%%       [done in 0.141 s]
%%     Key duplicate tests
%%       sc_tests:38: key_duplicate_test_ ([ ])...ok
%%       sc_tests:39: key_duplicate_test_ ([ {2,a} ])...ok
%%       sc_tests:40: key_duplicate_test_ ([ {2,a},{3,b} ])...ok
%%       [done in 0.047 s]
%%     Extrema tests
%%       sc_tests:19: extrema_test_ (1,2,3,4)...ok
%%       sc_tests:20: extrema_test_ (-1,-2,-3)...ok
%%       sc_tests:21: extrema_test_ (-1.1,0,1.1)...ok
%%       sc_tests:22: extrema_test_ (a,b,c)...ok
%%       sc_tests:23: extrema_test_ (1,a,{})...ok
%%       sc_tests:24: extrema_test_ (1)...ok
%%       sc_tests:26: extrema_test_ ([] error)...ok
%%       [done in 0.109 s]
%%     [done in 0.343 s]
%%   [done in 0.343 s]
%% =======================================================
%%   All 22 tests passed.
%% ok'''
%% @since 460

test(verbose=_Style) ->

    eunit:test(sc, [verbose]);



test([verbose]=_Style) ->

    eunit:test(sc, [verbose]).






%% @spec extrema(List::non_empty_list()) -> {Low::any(),Hi::any()}
%% @doc Returns the lowest and highest values in a list of one or more member in the form `{Lo,Hi}'.  `error()'s `badarg' for empty lists.  Mixed-type safe; sorts according to type order rules.  ```1> sc:extrema([1,2,3,4]).
%% {1,4}
%%
%% 2> sc:extrema([1,2,3,a,b,c]).
%% {1,c}'''
%% @since Version 460

extrema([]) ->

    error(badarg);



extrema(List) ->

    [First | _] = List,

    Next = fun(Next,T) ->

               {Lo, Hi} = T,

               Lo2 = if
                   Next < Lo -> Next;
                   true      -> Lo
               end,

               Hi2 = if
                   Next > Hi -> Next;
                   true      -> Hi
               end,

               {Lo2, Hi2}

           end,

    lists:foldl(Next, {First,First}, List).





%% @spec key_duplicate(CvList::list()) -> [any()]
%% @doc Iterates a list of `{Count,Term}', producing a list of `[Term,Term,...]'.  ```1> sc:key_duplicate([ {3,bork} ]).
%% [bork,bork,bork]
%%
%% 2> sc:key_duplicate([ {3,sunday}, {2,monster}, {2,truck}, {1,'MADNESS'} ]).
%% [sunday,sunday,sunday,monster,monster,truck,truck,'MADNESS']'''
%% @since Version 462

key_duplicate(KeyList) ->

    lists:flatten( [ lists:duplicate(Key, Value) || {Key,Value} <- KeyList ] ).





%% @spec rotate_list(Distance::integer(), ListData::list()) -> list()
%% @doc Rotates the front `Distance' elements of a list to the back, in order.  Negative distances rotate the back towards the front.  Distances over the length of
%% the list wrap in modulus.  ```1> sc:rotate_list(2, [1,2,3,4,5,6,7,8]).
%% [3,4,5,6,7,8,1,2]
%%
%% 2> sc:rotate_list(-2, [1,2,3,4,5,6,7,8]).
%% [7,8,1,2,3,4,5,6]
%%
%% 3> sc:rotate_list(0, [1,2,3,4,5,6,7,8]).
%% [1,2,3,4,5,6,7,8]
%%
%% 4> sc:rotate_list(16, [1,2,3,4,5,6,7,8]).
%% [1,2,3,4,5,6,7,8]'''
%% @since Version 463

rotate_list(_, []) ->

    [];



rotate_list(0, List) ->

    List;



rotate_list(By, List) when By =< (-(length(List))) ->

    rotate_list(By rem length(List), List);



rotate_list(By, List) when By < 0 ->

    rotate_list(length(List) + By, List);



rotate_list(By, List) when By >= length(List) ->

    rotate_list(By rem length(List), List);



rotate_list(By, List) ->

    { Front, Rear } = lists:split(By, List),
    Rear ++ Front.










%% @spec index_of_first(Item, List) -> integer()|undefined
%% @doc Returns the index of the first instance of `Item' in the `List', or `undefined' if `Item' is not present.  ```1> sc:index_of_first(c, [a,b,c,d,e]).
%% 3
%%
%% 2> sc:index_of_first(j, [a,b,c,d,e]).
%% undefined'''
%% @since Version 463

index_of_first(Item, List) ->

    index_of_first(Item, List, 1).



index_of_first(_Item, [], _Pos) ->

    undefined;



index_of_first(Item, [Item|_ListRem], Pos) ->

    Pos;



index_of_first(Item, [_OtherItem|ListRem], Pos) ->

    index_of_first(Item, ListRem, Pos+1).






%% @spec rotate_to_first(Item, List) -> list()
%% @doc Rotates the list to the first instance of Item.  ```1> sc:rotate_to_first(c, [a,b,c,d,e]).
%% [c,d,e,a,b]
%%
%% 2> sc:rotate_to_first(j, [a,b,c,d,e]).
%% no_such_element'''
%% @since Version 464

rotate_to_first(Item, List) ->

    case index_of_first(Item, List) of

        undefined ->
            no_such_element;

        IoF ->
            rotate_list(IoF-1, List)

    end.





%% @spec rotate_to_last(Item, List) -> list()
%% @doc Rotates the list so that the first instance of Item becomes the last element in the list.  ```1> sc:rotate_to_last(c, [a,b,c,d,e]).
%% [d,e,a,b,c]
%%
%% 2> sc:rotate_list(j, [a,b,c,d,e]).
%% no_such_element'''
%% @since Version 464

rotate_to_last(Item, List) ->

    case index_of_first(Item, List) of

        undefined ->
            no_such_element;

        IoF ->
            rotate_list(IoF, List)

    end.





%% @spec flag_sets(Flags::list()) -> list_of_lists()

%% @doc Returns every interpretation of the list as a set of boolean flags, including all-off and all-on. ```1> sc:flag_sets([1,2,3,4]).
%% [ [], [4], [3], [3,4], [2], [2,4], [2,3], [2,3,4], [1], [1,4], [1,3], [1,3,4], [1,2], [1,2,4], [1,2,3], [1,2,3,4] ]
%%
%% 2> length(sc:flag_sets(lists:seq(1,16))).
%% 65536
%%
%% 3> sc:flag_sets([]).
%% [ [] ]
%%
%% 4> SourceOfPowers = sc:flag_sets([magic,technology,evil,alien]).
%% [[],                              % Batman
%%  [alien],                         % Superman
%%  [evil],                          % Darkseid
%%  [evil,alien],                    % Sinestro
%%  [technology],                    % Mister Terrific (Michael Holt)
%%  [technology,alien],              % The Blue Beetle
%%  [technology,evil],               % The OMACs
%%  [technology,evil,alien],         % Braniac
%%  [magic],                         % Shazam
%%  [magic,alien],                   % Green Lantern (Alan Scott)
%%  [magic,evil],                    % Lucifer Morningstar
%%  [magic,evil,alien],              % pre-crisis Star Sapphire
%%  [magic,technology],              % Alexander Luthor Jr.
%%  [magic,technology,alien],        % Mister Miracle
%%  [magic,technology,evil],         % pre-crisis Sinestro
%%  [magic,technology,evil,alien]]   % Granny Goodness'''

%% @since Version 465

flag_sets([]) ->

    [[]];




flag_sets([Flag|RemFlags]) ->

    [ MaybeFlag ++ Reps ||
        MaybeFlag <- [[],[Flag]],
        Reps      <- flag_sets(RemFlags)
    ].





%% @equiv member_sets(Memberships, no_absence)

member_sets(Memberships) ->

    member_sets(Memberships, no_absence).



%% @spec member_sets(Memberships::list_of_lists(), AllowAbsence::atom()) -> list_of_lists()

%% @doc For a list of memberships, return every possible combination of one representative member from each list.
%% The parameter `AllowAbsence' controls whether memberships may be unrepresented; if unrepresented memberships are possible, then
%% one possible representation becomes the empty list. ```1> sc:member_sets([ [a,b],[1,2,3],[i,ii,iii] ], no_absence).
%% [[a,1,i], [a,1,ii], [a,1,iii], [a,2,i], [a,2,ii], [a,2,iii], [a,3,i], [a,3,ii], [a,3,iii],
%%   [b,1,i], [b,1,ii], [b,1,iii], [b,2,i], [b,2,ii], [b,2,iii], [b,3,i], [b,3,ii], [b,3,iii]]
%%
%% 2> sc:member_sets([ [a,b],[1,2],[i,ii] ], allow_absence).
%% [ [], [i], [ii], [1], [1,i], [1,ii], [2], [2,i], [2,ii], [a], [a,i], [a,ii], [a,1], [a,1,i],
%%   [a,1,ii], [a,2], [a,2,i], [a,2,ii], [b], [b,i], [b,ii], [b,1], [b,1,i], [b,1,ii], [b,2],
%%   [b,2,i], [b,2,ii] ]
%%
%% 3> sc:member_sets([ [toast,pancakes], [sausage,bacon] ] ).
%% [[toast,sausage],
%%  [toast,bacon],
%%  [pancakes,sausage],
%%  [pancakes,bacon]]
%%
%% 4> sc:member_sets([ [toast,pancakes], [sausage,bacon] ], no_absence ).
%% [[toast,sausage],
%%  [toast,bacon],
%%  [pancakes,sausage],
%%  [pancakes,bacon]]
%%
%% 5> sc:member_sets([ [toast,pancakes], [sausage,bacon] ], allow_absence).
%%  [[],
%%  [sausage],
%%  [bacon],
%%  [toast],
%%  [toast,sausage],
%%  [toast,bacon],
%%  [pancakes],
%%  [pancakes,sausage],
%%  [pancakes,bacon]]
%%
%% 6> Format = fun(Person, Place, Weapon) -> "It was " ++ Person ++ " in the " ++ Place ++ " with the " ++ Weapon ++ "!" end.
%% #Fun<erl_eval.18.105910772>
%%
%% 7> [ Format(Pe,Pl,WW) || [Pe,Pl,WW] <- sc:member_sets( [ ["Col. Mustard", "Ms. Scarlett"], ["conservatory", "hallway", "kitchen"], ["lead pipe"] ] ) ].
%% ["It was Col. Mustard in the conservatory with the lead pipe!",
%%  "It was Col. Mustard in the hallway with the lead pipe!",
%%  "It was Col. Mustard in the kitchen with the lead pipe!",
%%  "It was Ms. Scarlett in the conservatory with the lead pipe!",
%%  "It was Ms. Scarlett in the hallway with the lead pipe!",
%%  "It was Ms. Scarlett in the kitchen with the lead pipe!"]'''
%% @since Version 466

member_sets([], _) ->

    [[]];



member_sets([[]], _) ->

    [[]];



member_sets( [Membership|RemMemberships], no_absence   ) ->

    [ [Member] ++ RemRep ||
        Member <- Membership,
        RemRep <- member_sets(RemMemberships, no_absence)
    ];



member_sets( [Membership|RemMemberships], allow_absence) ->

    Compact = fun(Member, RemRep) ->

        case Member of

            empty ->
                RemRep;

            {item,X} ->
                [X] ++ RemRep

        end

    end,

    [ Compact(Member, RemRep) ||
        Member <- [empty] ++ [{item,X}||X<-Membership],
        RemRep <- member_sets(RemMemberships, allow_absence)
    ].





%% @type non_negative_integer() = integer().  A {@type non_negative_integer()} must be equal to or greater than zero.

%% @spec count_x(Item::any(), List::list()) -> non_negative_integer()

%% @doc Counts the number of instances of Item in List.  ```1> sc:count_x(alpha, [alpha, beta, gamma, beta, alpha]).
%% 2
%%
%% 2> sc:count_x(3, [1,2,4,5,6,7]).
%% 0'''

%% @since Version 468

count_x(Item, List) ->

    lists:foldl(

        fun(X, Counter) ->

            case X of

                Item ->
                    Counter+1;

                _ ->
                    Counter

            end

        end,

        0,
        List

    ).





%% @equiv list_intersection(List1, List2, unsorted)

list_intersection(List1, List2) ->

    list_intersection(List1, List2, unsorted).



%% @spec list_intersection(List1::list(), List2::list(), IsSorted::atom()) -> list()

%% @doc Efficiently computes the intersection of two lists.  The third parameter, which is optional and defaults to `unsorted', is either the atom `sorted' or `unsorted'.  If `sorted' is used, the function will sort both inputs before proceeding, as it requires sorted lists; as such, if you already know your lists to be sorted, passing `unsorted' will save some time.  The return list will be reverse sorted. ```1> sc:list_intersection([1,2,3,4,5,2,3,10,15,25,30,40,45,55],[1,3,5,5,5,15,20,30,35,40,50,55]).
%% [55,40,30,15,5,3,1]
%%
%% 2> sc:list_intersection([1],[2]).
%% []''' {@section Thanks} to Ayrnieu for catching a defect in the initial implementation.

%% @since Version 471

list_intersection(List1, List2, unsorted) ->

    list_intersection(lists:sort(List1), lists:sort(List2), sorted);



list_intersection(List1, List2, sorted) ->

    intersect_walk(List1, List2, []).



%% @private

intersect_walk( [], _L2, Work ) ->

    Work;



intersect_walk( _L1, [], Work) ->

    Work;



intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work) when L1Head == L2Head ->

    intersect_walk(L1Rem, L2Rem, [L1Head]++Work);



intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work) when L1Head < L2Head ->

    intersect_walk(L1Rem, [L2Head|L2Rem], Work);



intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work) when L1Head > L2Head ->

    intersect_walk( [L1Head|L1Rem], L2Rem, Work).





%% @spec (Ls::list()) -> list_of_tuples()
%% @equiv zip_n(Ls, to_tuple)

zip_n(Ls) ->

    zip_n(Ls, to_tuple).



%% @spec zip_n(Ls::list(), ResultType::atom()) -> list_of_tuples()

%% @doc Computes a zip on any sized group of lists, rather than just two or three as offered by the lists module. ```1> sc:zip_n([ [1,2,3], [a,b,c], [i,ii,iii] ]).
%% [{1,a,i},{2,b,ii},{3,c,iii}]
%%
%% 2> sc:zip_n([ [1,2,3], [a,b,c], [i,ii,iii], [x,y,z], [red,blue,green], [april,may,june] ]).
%% [{1,a,i,x,red,april},
%%  {2,b,ii,y,blue,may},
%%  {3,c,iii,z,green,june}]
%%
%% This is actually more efficient than one might expect at first glance.  I ran a benchmark of 100,000 transformations of a list of lists into a list of tuples using {@link benchmark/3} and {@link multi_do/4} against both zip_n and the library function zip3; the library function won at 150 seconds to 175, which is a far smaller difference than I expected.```3> Testy = [ [1,2,3], [1,2,3], [1,2,3] ].
%% [[1,2,3],[1,2,3],[1,2,3]]
%%
%% 4> sc:benchmark(sc, multi_do, [100000, sc, zip_n, [Testy]]).
%% {174.95563, [[{1,1,1},{2,2,2},{3,3,3}], [{1,1,1},{2,2,2},{3,3,3}], ... }
%%
%% 5> sc:benchmark(sc, multi_do, [100000, lists, zip3, Testy]).
%% {149.605, [[{1,1,1},{2,2,2},{3,3,3}], [{1,1,1},{2,2,2},{3,3,3}], ... }'''
%%
%% {@section Thanks} Thanks to Vladimir Sessikov for contributing this to and thus allowing conscription from <a href="http://www.erlang.org/ml-archive/erlang-questions/200207/msg00066.html">the mailing list</a>.

%% @since Version 472

zip_n(Ls, to_tuple) ->

    [ list_to_tuple(L) ||
        L <- zip_n_listn(Ls)
    ];



zip_n(Ls, to_list) ->

    zip_n_listn(Ls).



%% @private

zip_n_listn(Ls) ->

    [ lists:reverse(L) ||
        L <- zip_n_foldn(fun (A, Acc) -> [A|Acc] end, [], Ls)
    ].



%% @private

zip_n_foldn(_, _, []) -> 

    [];



zip_n_foldn(Fun, Acc0, Ls) -> 

    zip_n_foldn(Fun, Acc0, Ls, []).
    


zip_n_foldn(_, _, [ [] | _ ], Ret) -> 

    lists:reverse(Ret);



zip_n_foldn(Fun, Acc0, Ls, Ret) -> 

    zip_n_foldn(
        Fun,
        Acc0,
        [ tl(L) || L <- Ls ],
        [ lists:foldl(Fun, Acc0, [hd(L) || L <- Ls] ) | Ret ]
    ).





%% @type list_of_lists() = list().  Every member of a {@type list_of_lists()} is a {@type list()}.

%% @spec combinations(OutputItemSize::positive_integer(), Items::list()) -> list_of_lists()

%% @doc Provides a list of every unique combination of input terms, order-ignorant; contrast {@link permute/2}.  Permutations are all unique combinations of a set of tokens; the 2-permutations of `[a,b,c]' for example are `[a,b]', `[a,c]' and `[b,c]'.  Note the absence of other orderings, such as `[b,a]', which are provided by {@link permute/2}.  Combinations are taken of a smaller count of tokens than the main set.  Combinations are not ordered, but this implementation happens to provide answers in the same order as the input list.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> sc:combinations(2, [a,b,c,d]).
%% [[a,b],[a,c],[a,d],[b,c],[b,d],[c,d]]
%%
%% 2> sc:combinations(2, ["dave","kate","pat"]).
%% [["dave","kate"],["dave","pat"],["kate","pat"]]
%%
%% 3> sc:combinations(2, [fast, strong, smart, lucky]).
%% [[fast,strong], [fast,smart], [fast,lucky], [strong,smart], [strong,lucky], [smart,lucky]]''' {@section Thanks} to Alisdair Sullivan for this implementation, which has been slightly but not significantly modified since receipt.

%% @since Version 473

combinations(1, Items) when is_list(Items) ->

    [ [I] || I <- Items ];



combinations(_N, []) ->

   [];




combinations(0, L) when is_list(L) ->

    [];




combinations(N, Items) when is_list(Items), is_integer(N), N > 0 ->

    [ lists:flatten(lists:append( [lists:nth(I, Items)], [J] )) ||
      I <- lists:seq(1, length(Items)),
      J <- combinations( (N-1), lists:nthtail(I, Items) )
    ].





%% @spec expand_labels([{Label::any(),List::list()}]) -> list_of_lists()

%% @doc Expands a series of labels over lists to create a cartesian 2-ary tuple expansion.  ```1> sc:expand_labels([{villain,[lex_luthor,sinistar,gargamel]}]).
%% [{villain,lex_luthor},{villain,sinistar},{villain,gargamel}]
%%
%% 2> sc:expand_labels([ {hero,[superman,tinyship,papa_smurf]}, {villain,[lex_luthor,sinistar,gargamel]} ]).
%% [{hero,superman},
%%  {hero,tinyship},
%%  {hero,papa_smurf},
%%  {villain,lex_luthor},
%%  {villain,sinistar},
%%  {villain,gargamel}]'''

%% @since Version 474

expand_labels(List) when is_list(List) ->

    lists:flatten( [ expand_label(X) || X <- List ] ).



%% @private

expand_label({Label,List}) when is_list(List) ->

     [ {Label,L} || L<-List ].





%% @equiv permute(List, length(List))

permute(List) ->

    permute(List, length(List)).



%% @type positive_integer() = integer().  Positive integer must be greater than zero.

%% @spec permute(List::list(), Depth::positive_integer()) -> list()

%% @doc Calculate either the full or the depth-limited permutations of a list, order sensitive; contrast {@link combinations/2}.  Permutations are all valid orderings of a set of tokens; the permutations of `[a,b]' for example are `[a,b]' and `[b,a]'.  Depth limitation means the permutations of a smaller count of tokens from the main set; the 2-limited permutations of `[a,b,c]' for example are `[a,b]', `[a,c]', `[b,a]', `[b,c]', `[c,a]' and `[c,b]'.  Permutations are not ordered.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> sc:permute(["dave","kate","pat"]).
%% [{"pat","kate","dave"}, {"kate","pat","dave"}, {"pat","dave","kate"}, {"dave","pat","kate"}, {"kate","dave","pat"}, {"dave","kate","pat"}]
%%
%% 2> sc:permute([fast, strong, smart, lucky], 2).
%% [{strong,fast}, {smart,fast}, {lucky,fast}, {fast,strong}, {smart,strong}, {lucky,strong}, {fast,smart}, {strong,smart}, {lucky,smart}, {fast,lucky}, {strong,lucky}, {smart,lucky}]'''

%% @since Version 474

permute(List, 1) when is_list(List) ->

    [ [T] ||
        T <- List
    ];



permute(List, Depth) when is_list(List), is_integer(Depth) ->

    [ [T]++R ||
        T <- List,
        R <- permute(List--[T], Depth-1)
    ].





%% @spec shared_keys(TupleList::sorted_keylist()) -> sorted_keylist()

%% @doc Create sorted list X of 3-ary tuples {K,Ai,Bi} from sorted lists A, B of 2ary {K,Ai}/{K,Bi} tuples, where key K appears in both A and B. ```1> sc:shared_keys([{1,a},{2,a},{3,a}],[{1,b},{3,b},{4,b}]).
%% [{1,a,b},{3,a,b}]
%%
%% 2>sc:shared_keys([{1,a},{2,a}],[{3,b},{4,b}]).
%% []'''

%% @since Version 475

shared_keys(TupleList) when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    shared_keys(lists:sort(A),lists:sort(B)).



%% @type keylist() = keylist().  All members of keylists are tuples of two-or-greater arity, and the first element is considered their key in the list.  List keys are unique; therefore `[{a,1},{b,1}]' is a keylist, but `[{a,1},{a,1}]' is not.
%% @type sorted_keylist() = keylist().  A sorted keylist is a keylist in the order provided by {@link lists:sort/1}.  Because of erlang tuple ordering rules and the fact that keylist keys are unique, this means the list will be ordered by key.

%% @equiv shared_keys(lists:zip(lists:sort(A), lists:sort(B)))
%% @spec shared_keys(TupleList::sorted_keylist(), Presorted::presorted) -> sorted_keylist()
%% @doc Equivalent to {@link shared_keys/1}, but skips sorting the lists (and thus requires pre-sorted lists), which may save significant work repetition.

shared_keys(TupleList, presorted) when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    shared_keys(A,B);



%% @doc Create sorted list X of 3-ary tuples `{K,Ai,Bi}' from sorted lists A, B of 2ary `{K,Ai}'/`{K,Bi}' tuples, where key `K' appears in both `A' and `B'.

shared_keys(A,B) when is_list(A), is_list(B) ->

    both_lists_next_item(lists:sort(A),lists:sort(B),[]).



%% @equiv shared_keys(lists:sort(A),lists:sort(B))
%% @spec shared_keys(A::sorted_keylist(), B::sorted_keylist(), Presorted::presorted) -> sorted_keylist()
%% @doc Equivalent to {@link shared_keys/2}, but skips sorting the lists (and thus requires pre-sorted lists), which may save significant work repetition.

shared_keys(A,B,presorted) when is_list(A), is_list(B) ->

    both_lists_next_item(A,B,[]).



both_lists_next_item([], _, Work) ->

    lists:reverse(Work);



%% @private

both_lists_next_item(_, [], Work) ->

    lists:reverse(Work);



both_lists_next_item([ {K,Ai} | Ar], [ {K,Bi} | Br], Work) ->

    both_lists_next_item(Ar, Br, [{K,Ai,Bi}]++Work);



both_lists_next_item(IA, IB, Work) ->

    [{Ka,_}|Ar] = IA,
    [{Kb,_}|Br] = IB,

    if

        Ka < Kb ->
            both_lists_next_item(Ar, IB, Work);

        true ->
            both_lists_next_item(IA, Br, Work)

    end.





%% @spec list_product(A::numericlist()) -> number()

%% @doc Takes the product of all numbers in the list.  Offered mostly to make dependant code clearer. ```1> sc:list_product([1,2,5.4]).
%% 10.8'''

%% @since Version 476

list_product(List) when is_list(List) ->

    list_product(List, 1).



list_product([], Counter) ->

    Counter;



list_product([Head|Tail], Counter) ->

    list_product(Tail, Counter*Head).





%% @type filterfunction() = function().  Filter functions are 1ary binary predicates - they accept an argument and return either true or false.
%% @type sanitizer() = list() | filterfunction().  Sanitizers are used by {@link sanitize_tokens/2} for input sanitization; they define what parts of an input list are valid, and the remainder are removed.  Sanitizers may either be a list of acceptable elements or a filter function.

%% @spec sanitize_tokens(InputList::list(), Allowed::sanitizer()) -> list()

%% @doc Remove unacceptable elements from an input list, as defined by another list or a filter function.  Common reasons for sanitization include reducing arbitrary or bulk data to key format (such as using an original filename and new size to generate a new filename or database key) and removing malformed items from a list before processing. ```1> sc:sanitize_tokens("ae0z4nb'wc-04bn ze0e 0;4ci ;e0o5rn;", "ace").
%% "aeceece"
%%
%% 2> Classifier = fun(apple) -> true; (banana) -> true; (cherry) -> true; (date) -> true; (elderberry) -> true; (_) -> false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 3> sc:sanitize_tokens([apple, boat, cherry, dog, elderberry], Classifier).
%% [apple,cherry,elderberry]
%%
%% 4> Vowels = fun($a)->true; ($e)->true; ($i)->true; ($o)->true; ($u)->true; ($A)->true; ($E)->true; ($I)->true; ($O)->true; ($U)->true; (_)->false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 5> sc:sanitize_tokens("A quick brown fox jumped over the lazy dog", Vowels).
%% "Auiooueoeeao"
%%
%% 6> sc:sanitize_tokens("A quick brown fox jumped over the lazy dog", "abcdefABCDEF").
%% "Acbfedeead"
%%
%% 7> BobcatGoldthwait = fun(X) -> sc:sanitize_tokens(X, "aeiouAEIOU") end.
%% #Fun<erl_eval.6.13229925>
%%
%% 8> BobcatGoldthwait("A quick brown fox jumped over the lazy dog").
%% "Auiooueoeeao"'''
%%
%% @see sanitize_filename/1

%% @since Version 477

sanitize_tokens(List, Allowed) when is_list(List), is_function(Allowed) ->

    lists:filter(Allowed, List);



sanitize_tokens(List, Allowed) when is_list(List), is_list(Allowed) ->

    lists:filter(fun(X) -> lists:member(X,Allowed) end, List).





%% @spec  bandwidth_calc(Data) -> list_of_2ary_tuples()
%% @equiv bandwidth_calc(Data,all)

%% @since Version 478

bandwidth_calc(Data) ->

    bandwidth_calc(Data, all).





%% @type bw_scale() = { Unit::atom(),      Timescale::atom() }.  `bw_scale' - Bandwidth scale - is a units-per-time notation for bandwidth measurement, eg `{megabits,day}'.
%% @type bw_rate()  = { Scale::bw_scale(), Rate::float() }.      `bw_rate' - Bandwidth rate - is a rate-in-units-per-time notation for bandwidth measurement, eg `{{megabits,day},10.5}'.

%% @spec bandwidth_calc(Data, Scale::bw_scale|all) -> bw_rate()|[bw_rate()]

%% @doc <span style="color:orange;font-style:italic">Untested</span> Calculates digital line bandwidth over timescales in converted units. ```1>'''
%% Also knows the shorthand notations `{X,meg}', `{X,gig}' and `{X,t}' for input only in base-10 only. ```5> sc:bandwidth_calc({10,meg}, {gigabits,day}).
%% {{gigabits,day},864.0}'''

%% @since Version 478

%% @todo That return type specification is wrong.  It needs to self-recurse.  Figure out how to at-spec that later.

bandwidth_calc({bits_per_second, BitsPerSecond}, To) ->

    bandwidth_calc(BitsPerSecond, To);





bandwidth_calc({megabits_per_second, MbpS}, To) ->

    bandwidth_calc(MbpS * 1000000, To);





bandwidth_calc({MbpS, meg}, To) ->

    bandwidth_calc(MbpS * 1000000, To);





bandwidth_calc({MbpS, gig}, To) ->

    bandwidth_calc(MbpS * 1000000000, To);





bandwidth_calc({MbpS, t}, To) ->

    bandwidth_calc(MbpS * 1000000000000, To);





bandwidth_calc(BitsPerSecond, all) when is_integer(BitsPerSecond) ->

    [
        bandwidth_calc(BitsPerSecond, {Unit, Timeframe})
    ||
        Unit      <- [bits, megabits, mebibits, gigabits, gibibits, terabits, tebibits],
        Timeframe <- [second, minute, hour, day, week, month_28, month_29, month_30, month_31]
    ];





bandwidth_calc(BitsPerSecond, {Unit, Timeframe}) when is_integer(BitsPerSecond) ->

    Divisor = scale_i(bits, Unit),

    Timescale = case Timeframe of
        second   -> 1;
        minute   -> 60;
        hour     -> 60*60;
        day      -> 60*60*24;
        week     -> 60*60*24*7;
        month_28 -> 60*60*24*28;
        month_29 -> 60*60*24*29;
        month_30 -> 60*60*24*30;
        month_31 -> 60*60*24*31;
        year_365 -> 60*60*24*365;
        year_366 -> 60*60*24*366
    end,

    { {Unit, Timeframe}, (BitsPerSecond / Divisor) * Timescale }.





% todo comeback Expose?

%% @doc <span style="color:orange;font-style:italic">Untested</span>

scale_i(X,X)            -> 1;

scale_i(inches, feet)   -> 12;
scale_i(inches, yards)  -> 36;

scale_i(bits, kilobits) -> 1000;
scale_i(bits, kibibits) -> 1024;
scale_i(bits, megabits) -> 1000*1000;
scale_i(bits, mebibits) -> 1024*1024;
scale_i(bits, gigabits) -> 1000*1000*1000;
scale_i(bits, gibibits) -> 1024*1024*1024;
scale_i(bits, terabits) -> 1000*1000*1000*1000;
scale_i(bits, tebibits) -> 1024*1024*1024*1024;
scale_i(bits, petabits) -> 1000*1000*1000*1000*1000;
scale_i(bits, pebibits) -> 1024*1024*1024*1024*1024;
scale_i(bits, exabits)  -> 1000*1000*1000*1000*1000*1000;
scale_i(bits, exbibits) -> 1024*1024*1024*1024*1024*1024.





% http://www.drdobbs.com/architecture-and-design/225701139?pgno=1

%% @since Version 478

%% @doc <span style="color:orange;font-style:italic">Untested</span>

caspers_jones_estimate(FunctionPoints) when

    is_integer(FunctionPoints),
    FunctionPoints >= 0 ->


    [   { estimated_defects,   math:pow(FunctionPoints, 1.25) },
        { unit_test_baseline,  math:pow(FunctionPoints, 1.2)  },
        { months_to_implement, math:pow(FunctionPoints, 0.4)  },
        { staff_to_implement,  FunctionPoints/150             }
    ].





%% @spec naive_bayes_likelihood(FeatureEvident::non_negative_integer(), FeatureTotal::positive_integer(), NonFeatureEvident::non_negative_integer(), NonFeatureTotal::positive_integer()) -> Result::list()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Calculates the contributing difference probability, feature likelihood and non-feature likelihood of an event
%% by the naive Bayes likelihood method.

%% @since Version 478

naive_bayes_likelihood(FeatureEvident, FeatureTotal, NonFeatureEvident, NonFeatureTotal) when

    is_integer(FeatureEvident),
    is_integer(FeatureTotal),
    is_integer(NonFeatureEvident),
    is_integer(NonFeatureTotal),

    FeatureEvident    >= 0,
    FeatureTotal      >  0,
    NonFeatureEvident >= 0,
    NonFeatureTotal   >  0 ->


    LF = FeatureEvident / FeatureTotal,
    NF = NonFeatureEvident / NonFeatureTotal,
    CD = LF - NF,

    [ {contributing_difference, CD}, {likelihood_featured, LF}, {likelihood_nonfeatured, NF} ].





%% @spec range_scale(NumList::numeric_list()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Get the scale of a same-sign numeric range.  Gives nonsense results for non-numeric lists, or for lists which have both positive and negative members.  For a numeric list [4,5,6,12], the scale of the range 4..12 is 3:1, which is represented as 3.0 . ```1> sc:range_scale([3, 4, 5, 6]).
%% 2.0
%% 2> sc:range_scale([3, 6]).
%% 2.0
%% 3> sc:range_scale([6, 3]).
%% 2.0
%% 4> sc:range_scale([3, 7.5]).
%% 2.5
%% 5> sc:range_scale([3, 99]).
%% 33.0
%% 6> sc:range_scale([3, 3]).
%% 1.0'''

%% @since Version 479

range_scale(Nums) when is_list(Nums) ->

    {Lo, Hi} = sc:extrema(Nums),
    Hi/Lo.





%% @spec zipf_position_estimate(Score::number(), Rank::positive_integer()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Estimates the zipf baseline from a score and a rank position. ```1> sc:zipf_position_estimate(120, 3).
%% 360'''

%% @since Version 480

zipf_position_estimate(Score, Rank) ->

    Score * Rank.





%% @spec zipf_estimate_list(PosNumericList::positive_numeric_list()) -> positive_numeric_list()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Estimates the zipf baseline from each number in a numeric list. ```1> sc:zipf_estimate_list([ 120, 60, 40, 30, 24, 20 ]).
%% [120, 120, 120, 120, 120, 120]
%%
%% 2> sc:zipf_estimate_list([411,198,135,101,82]).
%% [411, 396, 405, 404, 410]
%%
%% 3> sc:zipf_estimate_list([630,298,231,180,118]).
%% [630, 596, 693, 720, 590]'''

%% @since Version 480

zipf_estimate_list(PosNumericList) ->

    [ zipf_position_estimate(Li, I) || {Li,I} <- lists:zip(PosNumericList, lists:seq(1, length(PosNumericList)) ) ].





%% @spec zipf_nearness(PosNumericList::positive_numeric_list()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> todo. ```1> sc:zipf_nearness([ 120, 60, 40, 30, 24, 20 ]).
%% [[ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ]]
%%
%% 2> sc:zipf_nearness([ 411, 198, 135, 101, 82 ]).
%% [[{strength,0.9635036496350365}, {center,405.2}],
%%  [{strength,0.9658536585365854}, {center,403.75}],
%%  [{strength,0.9853658536585366}, {center,406.3333333333333}],
%%  [{strength,0.9853658536585366}, {center,407.0}],
%%  [{strength,1.0},                {center,410.0}]]
%%
%% 3> sc:zipf_nearness([640,244,231,180,148]).
%% [[{strength, 0.6594594594594595}, {center,656.2}],
%%  [{strength, 0.6594594594594595}, {center,660.25}],
%%  [{strength, 0.9364864864864865}, {center,717.6666666666666}],
%%  [{strength, 0.972972972972973},  {center,730.0}],
%%  [{strength, 1.0},                {center,740.0}]]'''

%% @since Version 480

zipf_nearness(PosNumericList) ->

    ZD = zipf_estimate_list(PosNumericList),
    zipf_nearness_walk_strengths(ZD, []).





zipf_nearness_walk_strengths([], Work) ->

    lists:reverse(Work);





zipf_nearness_walk_strengths([_|Rem]=ZD, Work) ->

    Strength = 1 / (range_scale(ZD)),
    AMean    = arithmetic_mean(ZD),

    zipf_nearness_walk_strengths(Rem, [[{strength,Strength}, {center,AMean}]] ++ Work).





%% @type numericlist() = list().  All members of a numeric list must be number()s.
%% @spec arithmetic_mean(InputList::numericlist()) -> float()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Take the arithmetic mean (often called the average) of a list of numbers. ```1> sc:arithmetic_mean([1,2,3,4,5]).
%% 3.0'''

%% @see geometric_mean/1
%% @see harmonic_mean/1
%% @see weighted_arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 481

arithmetic_mean([]) ->

    0.0;



arithmetic_mean(List) when is_list(List) ->

    lists:sum(List) / length(List).





%% @spec geometric_mean(InputList::numericlist()) -> float()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Take the geometric mean of a list of numbers. ```1> sc:geometric_mean([1,2,3,4,5]).
%% 2.6051710846973517'''
%%
%% <a href="http://www.wolframalpha.com/input/?i=geometric+mean+{1%2C2%2C3%2C4%2C5}">WolframAlpha Confirms</a>
%%
%% The geometric mean is not defined for lists including 0.
%%
%% The naive approach `geometric_mean(List) -> math:pow(sc:list_product(List), 1/length(List))' is not used because it accumulates error very quickly, and is as such unsuited to huge lists.  This is the same as the expected function nth-root(prod, 1/n), but calculated differently for machine reasons.'''
%%
%% Thanks to Forest (anonymous by choice) for help resolving 0-correctness.

%% @see arithmetic_mean/1
%% @see harmonic_mean/1
%% @see gmean_vector_normal/1

%% @since Version 482

geometric_mean([]) ->

    0.0;





geometric_mean(List) when is_list(List) ->

    math:exp(arithmetic_mean([ math:log(X) || X<-List ])).





%% @spec harmonic_mean(InputList::numericlist()) -> float()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Take the harmonic mean of a list of numbers. ```1> sc:harmonic_mean([1,2,3,4,5]).
%% 2.18978102189781'''
%%
%% <a href="http://www.wolframalpha.com/input/?i=harmonic+mean+{1%2C2%2C3%2C4%2C5}">WolframAlpha Confirms</a>
%%
%% The harmonic mean is not defined for lists including 0.
%%
%% Thanks to Forest (anonymous by choice) for help resolving 0-correctness.

%% @see arithmetic_mean/1
%% @see geometric_mean/1
%% @see hmean_vector_normal/1

%% @since Version 483

harmonic_mean([]) ->

    0.0;



harmonic_mean(List) when is_list(List) ->

    length(List) / lists:sum([ 1/X || X<-List ]).





%% @spec weighted_arithmetic_mean(InputList::weightlist()) -> float()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Take the weighted arithmetic mean of the input values. ```1> sc:weighted_arithmetic_mean([ {8,1}, {3,4}, {16,1} ]).
%% 6.0'''

%% @see arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 484

weighted_arithmetic_mean(List) when is_list(List) ->

    weighted_arithmetic_mean(List, 0, 0).



weighted_arithmetic_mean([], Num, Denom) ->

    Num/Denom;



weighted_arithmetic_mean( [{V,W} | Tail], Num, Denom) ->

    weighted_arithmetic_mean(Tail, Num+(W*V), Denom+W).





%% @spec instant_runoff_vote(ListOfVoteLists::list_of_lists()) -> any()

%% @doc <span style="color:orange;font-style:italic">Untested</span>  Performs an instant runoff vote.  http://en.wikipedia.org/wiki/Instant-runoff_voting ```1>'''

%% @since Version 485

%% not complete todo comeback
%% also do other voting types

instant_runoff_vote(ListOfVoteLists) ->

    instant_runoff_vote(ListOfVoteLists, []).



instant_runoff_vote(ListOfVoteLists, Exclude) ->

    FilteredVotes = [ VoteList || VoteList <- ListOfVoteLists, VoteList =/= [] ],
    FilteredVotes.





% inspired by J dstat from some blog, http://bbot.org/blog/archives/2011/03/17/on_being_surprised_by_a_programming_language/
% herp derp

% J seems to be assuming sample for statistics.  I do not like assumptions.

%% @since Version 487

%% @doc <span style="color:orange;font-style:italic">Untested</span>

dstat(NumericList, PopulationOrSample) ->

    { Min, Max } = extrema(NumericList),

    [   { sample_size,        length(NumericList)                                 },
        { minimum,            Min                                                 },
        { maximum,            Max                                                 },
        { median,             median(NumericList)                                 },
        { arithmetic_mean,    arithmetic_mean(NumericList)                        },
        { standard_deviation, standard_deviation(NumericList, PopulationOrSample) },
        { skewness,           skewness(NumericList)                               },
        { kurtosis,           kurtosis(NumericList)                               }
    ].





% inspired by J dstat from some blog, http://bbot.org/blog/archives/2011/03/17/on_being_surprised_by_a_programming_language/
% herp derp

% J seems to be assuming sample for statistics.  I do not like assumptions.

% moar todo comeback

%% @since Version 487

%% @doc <span style="color:orange;font-style:italic">Untested</span>

extended_dstat(NumericList, PopulationOrSample) ->

    { Min, Max } = extrema(NumericList),

    [   { sample_size,        length(NumericList)                                 },
        { minimum,            Min                                                 },
        { maximum,            Max                                                 },
        { median,             median(NumericList)                                 },
        { mode,               mode(NumericList)                                   },
        { arithmetic_mean,    arithmetic_mean(NumericList)                        },
        { geometric_mean,     geometric_mean(NumericList)                         },
        { harmonic_mean,      harmonic_mean(NumericList)                          },
        { standard_deviation, standard_deviation(NumericList, PopulationOrSample) },
        { skewness,           skewness(NumericList)                               },
        { kurtosis,           kurtosis(NumericList)                               }
    ].





%% @spec median(List::numericlist()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Takes the median (central) value of a list.  Sorts the input list, then finds and returns the middle value.  ```1> sc:median([1,2,999]).
%% 2'''

%% @see arithmetic_mean/1
%% @see mode/1

%% @since Version 488

median(List) when is_list(List) ->

    SList  = lists:sort(List),
    Length = length(SList),

    case even_or_odd(Length) of

        even ->
            [A,B] = lists:sublist(SList, round(Length/2), 2),
            (A+B)/2;

        odd ->
            lists:nth( round((Length+1)/2), SList )

    end.





%% @spec even_or_odd(Num::integer()) -> even | odd

%% @doc Documentary convenience function that returns the atoms `even' or `odd' for any integer. ```1> sc:even_or_odd(3).
%% odd'''

%% @since Version 489

even_or_odd(Num) when is_integer(Num), Num band 1 == 0 -> even;
even_or_odd(Num) when is_integer(Num)                  -> odd.





%% @spec standard_deviation(Values::numericlist(), Kind::population|sample) -> float()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Measures the standard deviation of the values in the list.  ```1> sc:standard_deviation([1,2,3,4,5],population).
%% 1.4142135623730951
%%
%% 2> sc:standard_deviation([1,2,3,4,5],sample).
%% 1.5811388300841898
%%
%% 3> sc:standard_deviation([2,2,2,2],population).
%% 0.0
%%
%% 4> sc:standard_deviation([2,2,2,2],sample).
%% 0.0'''

%% @since Version 490

standard_deviation(Values, population) when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ sc_math:square(Val-Mean) || Val <- Values ]));



standard_deviation(Values, sample) when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt( lists:sum([ sc_math:square(Val-Mean) || Val <- Values ]) / (length(Values)-1) ).





% thanks to Chile and Kraln for straightening me out on moments and central moments

%% @spec moment(List::list(), N::number()) -> float()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Takes the Nth moment of a list.  The Nth moment of a list is the arithmetic mean of the list items, each taken to the Nth power.  Fractional Ns are well defined
%% and have obscure uses, though most will only ever use this with integer values of N; this function is valid for both.  Not to be confused with {@link central_moment/2}.  {@section Thanks}
%% to Kraln and Chile for straightening me out on moments and central moments.  ```1> sc:moment([1,1,1], 2).
%% 1.0
%%
%% 2> sc:moment([2,2,2], 2).
%% 4.0
%%
%% 3> sc:moment([1,2,3], 2).
%% 4.666666666666667
%%
%% 4> sc:moment([1,2,3], 3).
%% 12.0
%%
%% 5> sc:moment([1,2,3], 3.5).
%% 19.693026767781483'''

%% @since Version 491

moment(List, N) when is_list(List), is_number(N) ->

    arithmetic_mean( [ math:pow(Item, N) || Item <- List ] ).



%% @equiv [ moment(List, N) || N <- [2,3,4] ]

%% @since Version 491

%% @doc <span style="color:orange;font-style:italic">Untested</span>

moments(List) ->

    moments(List, [2,3,4]).



%% @equiv [ moment(List, N) || N <- Moments ]

%% @since Version 491

%% @doc <span style="color:orange;font-style:italic">Untested</span>

moments(List, Moments) when is_list(Moments) ->

    [ moment(List, M) || M <- Moments ].





% thanks to Chile and Kraln for straightening me out on moments and central moments

%% @spec central_moment(List::list(), N::integer()) -> float()

%% @doc <span style="color:red">Buggy</span> <span style="color:orange;font-style:italic">Untested</span> Takes the Nth cetral moment of a list.  The Nth central moment of a list is the arithmetic mean of (the list items each minus the mean of the list, each
%% taken to the Nth power).  In a sense, this is the "normalized" moment.  Fractional Ns are not defined.  Not to be confused with {@link moment/2}.  {@section Thanks} to Kraln and
%% Chile for straightening me out on moments and central moments.  ```1> sc:central_moment([1,1,1], 2).
%% 0.0
%%
%% 2> sc:central_moment([2,2,2], 2).
%% 0.0
%%
%% 3> sc:central_moment([1,2,3], 2).
%% 0.666666666666666
%%
%% 4> sc:central_moment([1,2,3], 3).
%% 0.0'''

%% @since Version 492

central_moment(List, N) when is_list(List), is_integer(N) ->

    ListAMean = arithmetic_mean(List),
    arithmetic_mean( [ math:pow(Item-ListAMean, N) || Item <- List ] ).





%% @equiv [ central_moment(List, N) || N <- [2,3,4] ]

%% @since Version 492

%% @doc <span style="color:orange;font-style:italic">Untested</span>

central_moments(List) ->

    central_moments(List, [2,3,4]).





%% @equiv [ central_moment(List, N) || N <- Moments ]

%% @since Version 492

%% @doc <span style="color:orange;font-style:italic">Untested</span>

central_moments(List, Moments) when is_list(Moments) ->

    [ central_moment(List, M) || M <- Moments ].





%% @equiv central_moment(List, 3)

%% @since Version 493

%% @doc <span style="color:orange;font-style:italic">Untested</span>

skewness(List) ->

    central_moment(List, 3).





%% @equiv central_moment(List, 4)

%% @since Version 494

%% @doc <span style="color:red">Buggy</span> <span style="color:orange;font-style:italic">Untested</span>

%% Wrong! todo comeback

kurtosis(List) ->

    central_moment(List, 4).





% todo comeback

% excess_kurtosis(List) ->





%% @spec histograph(List::list()) -> weightlist()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Takes a histograph count of the items in the list.  Mixed type lists are safe.  Input lists do not need to be sorted.  The histograph is shallow - that is, the histograph of `[ [1,2], [1,2], [2,2] ]' is `[ {[1,2],2}, {[2,2],1} ]', not `[ {1,2}, {2,4} ]'. ```1> sc:histograph([1,2,a,2,b,1,b,1,b,2,a,2,2,1]).
%% [{1,4},{2,5},{a,2},{b,3}]
%%
%% 2> sc:histograph([ sc:rand(10) || X <- lists:seq(1,100000) ]).
%% [{0,10044}, {1,9892}, {2,10009}, {3,10016}, {4,10050}, {5,10113}, {6,9990}, {7,9994}, {8,10004}, {9,9888}]'''

%% @since Version 496

%% @todo add an argument presort to this and other functions to skip the sorting pass

histograph([]) ->

    [];



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





%% @spec mode(List::numericlist()) -> any()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Takes the mode (most common) value(s) of a list, as a list.  If there are more than one value tied for most common, all tied will be returned.  This function is safe for mixed-type lists, and does not perform deep traversal (that is, the mode of `[ [2,2] ]' is `[2,2]', not `2'). ```sc:mode([1,2,1,3,1,4]).
%% [1]
%%
%% 2> sc:mode([ [1,2,3], [2,3,4], [3,4,5], [2,3,4] ]).
%% [[2,3,4]]
%%
%% 3> sc:mode([ a,b, 1, a,b, 2, a,b, 3 ]).
%% [a,b]'''

%% @see arithmetic_mean/1
%% @see median/1

%% @since Version 497

mode([]) ->

    [];



mode(List) when is_list(List) ->

    mode_front(lists:reverse(lists:keysort(2, histograph(List)))).



mode_front([{Item,Freq}|Tail]) ->

    mode_front(Tail, Freq, [Item]).



mode_front([ {Item, Freq} | Tail],  Freq,   Results) ->

    mode_front(Tail, Freq, [Item]++Results);



mode_front([ {_Item,_Freq} |_Tail], _Better, Results) ->

    Results;



mode_front( [], _Freq, Results) ->

    Results.





%% @spec amean_vector_normal(VX::numeric_list()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns the arithmetic mean of the elements of the unit vector for the vector provided.

%% @since Version 497

amean_vector_normal(VX) ->

    arithmetic_mean(sc_vector:normalize(VX)).





%% @spec gmean_vector_normal(VX::numeric_list()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns the geometric mean of the elements of the unit vector for the vector provided.

%% @since Version 498

gmean_vector_normal(VX) ->

    geometric_mean(sc_vector:normalize(VX)).





%% @spec hmean_vector_normal(VX::numeric_list()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns the harmonic mean of the elements of the unit vector for the vector provided.

%% @since Version 499

hmean_vector_normal(VX) ->

    harmonic_mean(sc_vector:normalize(VX)).





% Thanks for some math help on erl-b, erl-c and engset, Vat and Wintermute

% todo incomplete comeback

% erlang_b_distribution(N,A) ->
%
%    Num   = math:pow(A,N) / factorial(N),
%    Denom = lists:sum([ math:pow(A,I) / factorial(I) || I <- lists:seq(0,N) ]),
%
%    Num / Denom.





% todo incomplete comeback

% erlang_c_distribution(N,A) ->
%
%    Num   = (math:pow(A,N) / sc:factorial(N)) * (N/(N-A)),
%
%    Denom = lists:sum([ math:pow(A,I) / factorial(I) || I <- lists:seq(0,N-1) ])
%          + ((math:pow(A,N)/factorial(N))*(N/(N-A))),
%
%    {wait_probability, Num / Denom}.





%% @spec median_absolute_deviation(List::numericlist()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Calculate the median absolute deviation of a {@type numericlist()}. ```1> sc:median_absolute_deviation([1,1,2,2,4,6,9]).
%% 1'''

%% @since Version 501

median_absolute_deviation(List) when is_list(List) ->

    ListMedian = median(List),
    median( [ abs(ListItem - ListMedian) || ListItem <- List ] ).





%% @spec expected_value(List::mixed_weight_list()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns the expected value of infinite selection from a weighted numeric list.  ```1> sc:expected_value([1,2,3,4,5,6]).
%% 3.50000'''
%%
%% <a href="http://www.wolframalpha.com/input/?i=ExpectedValue[+f%2C+{1%2C2%2C3%2C4%2C5%2C6}%2C+f+]">Wolfram Alpha confirms</a>
%%
%% ```2> sc:expected_value([ {1,5}, {10,1} ]).
%% 2.5
%%
%% 3> sc:expected_value([ {-1,37}, {35,1} ]).
%% -5.26316e-2'''

%% @since Version 502

expected_value(List) ->

    expected_value(List, 0, 0).



expected_value([], Sum, Range) ->

    Sum/Range;



expected_value( [ {Value,Probability} | Remainder], Sum, Range) ->

    expected_value(Remainder, Sum+(Value*Probability), Range+Probability);



expected_value( [ UnweightedItem | Remainder], Sum, Range) ->

    expected_value([{UnweightedItem,1}] ++ Remainder, Sum, Range).





%% @spec absolute_difference(A::number(), B::number()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Takes the absolute value of the difference between the two arguments.  Offered mostly to make dependant code clearer. ```1> sc:absolute_difference(1.25, 1).
%% 0.25'''

%% @since Version 504

absolute_difference(A,B) ->

    abs(A-B).





%% @spec root_mean_square(Values::numericlist()) -> float()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Calculates the root mean square of the values in the list.  ```1> sc:root_mean_square([1,2,3,4,5]).
%% 3.3166247903554
%%
%% 2> sc:root_mean_square([2,2,2]).
%% 2.0'''

%% @since Version 505

root_mean_square(List) when is_list(List) ->

    math:sqrt(arithmetic_mean([ Val*Val || Val <- List ])).





%% @spec root_sum_square(VX::vector()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Calculate the magnitude (also known as the root sum square) of a vector.

%% @since Version 506

root_sum_square(VX) when is_list(VX) ->

    math:sqrt(lists:sum([ X*X || X <- VX ]));



root_sum_square(VX) when is_tuple(VX) ->

    root_sum_square(tuple_to_list(VX)).




%% @equiv root_sum_square(VX)

%% @since Version 506

vector_magnitude(VX) ->

    root_sum_square(VX).





%% @spec mod(Base::integer(), Range::integer()) -> integer()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Takes the modulus of an integer by another integer.  Luckily, erlang calls what most languages refer to as modulus by its correct name, remainder (c's `%', erlang's `rem').  Modulus is implemented incorrectly in nearly every language, because chip vendors implement remainder and the wrong name stuck.  The difference is in how the operator reacts to a negative `Base': -10 modulo 3 is 2, whereas -10 rem 3 is -1.  Remainder takes the residue of dividing the base by the lowest (nearest negative infinity) integer N adjacent the real valued divisor; modulo returns the highest, which is less CPU efficient but always provides an answer on [0..Range-1]. ```1> sc:mod(10,3).
%% 1
%%
%% 2> [ sc:mod(X,4) || X <- lists:seq(-10,10) ].
%% [2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2]'''
%%
%% @since Version 507

mod(Base, Range) when is_integer(Base), is_integer(Range) ->

    case Base rem Range of

        X when X < 0 ->
            X + Range;

        Z ->
            Z

    end.





%% @spec square(Input::number()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Squares the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> sc:square(2).
%% 4
%%
%% 2> sc:square(2.5).
%% 6.25'''

%% @since Version 508

square(X) ->

    X*X.





%% @spec cube(Input::number()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Cubes the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> sc:cube(2).
%% 8
%%
%% 2> sc:cube(2.5).
%% 6.25'''

%% @since Version 508

cube(X) ->

    X*X*X.





% comeback todo documentation

%% @since Version 509

%% @doc <span style="color:orange;font-style:italic">Untested</span>

factorial(X) ->

    factorial(X, 1).



%% @private

factorial(0, _Counter) ->

    0;



factorial(1, Counter) ->

    Counter;



factorial(X, Counter) when is_integer(X), X > 1 ->

    factorial(X-1, Counter*X).





%% @since Version 510
%% @equiv ceiling(X)

ceil(X) ->

     ceiling(X).





% todo comeback docs

%% @since Version 510

%% @doc <span style="color:orange;font-style:italic">Untested</span>

ceiling(X) ->

     ceiling_t(trunc(X), trunc(X)-X).



%% @private

ceiling_t(T, Td) when Td < 0 -> T+1;
ceiling_t(T, Td) when Td > 0 -> T;
ceiling_t(T,_Td)             -> T.





% todo comeback docs; point out this isn't erlang:floor because of negative number behavior

%% @since Version 511

%% @doc <span style="color:orange;font-style:italic">Untested</span>

floor(X) ->

     floor_t(trunc(X), trunc(X)-X).



%% @private

floor_t(T, Td) when Td < 0 -> T;
floor_t(T, Td) when Td > 0 -> T-1;
floor_t(T,_Td)             -> T.





% comeback todo docs

%% @since Version 512

%% @doc <span style="color:orange;font-style:italic">Untested</span>

mersenne_prime(Which) -> mersenne_prime_worker(Which, 1).



%% @private

mersenne_prime_worker(0, Current) ->

    Current - 1;



mersenne_prime_worker(Remain, Current) when Remain > 30 ->

    mersenne_prime_worker(Remain-30, Current*1073741824);



mersenne_prime_worker(Remain, Current) ->

    mersenne_prime_worker(Remain-1, Current*2).





% comeback todo docs

%% since Version 513

%% @doc <span style="color:orange;font-style:italic">Untested</span>

factorize(N) when is_integer(N), N > 1 ->

    factorize(N, 2, []);

factorize(1) -> [1];
factorize(0) -> [].





factorize(N, Current, Work) ->

    case Current > math:sqrt(N) of

        true  -> lists:reverse([N] ++ Work);
        false ->
            case N rem Current of
                0 -> factorize(N div Current, Current,   [Current] ++ Work);
                _ -> factorize(N,             Current+1, Work)
            end
    end.





%% @spec is_unique_list(List::list()) -> true | false

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns true if the list is unique; false otherwise.  List uniqueness is defined as whether any member of the list compares equally to any other member; deep list inspection is not performed.  Comparison is type-safe. ```2> sc:is_unique_list([1,2,3]).
%% true
%%
%% 2> sc:is_unique_list([1,2,3,1]).
%% false
%%
%% 3> sc:is_unique_list([1,2,3,{1}]).
%% true
%%
%% 4> sc:is_unique_list([1,2,3,[1]]).
%% true
%%
%% 5> sc:is_unique_list([1,2,3,[1],[1]]).
%% false'''

%% @since Version 514

is_unique_list(List) ->

    length(lists:usort(List)) == length(List).





%% @spec is_sorted_list(List::list()) -> true | false

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns true if the list is sorted; false otherwise.  List sortedness is typesafe, and defined equivalently to how defined by the language and `lists:sort()'. ```1> sc:is_sorted_list([1,2,3]).
%% true
%%
%% 2> sc:is_sorted_list([1,2,3,1]).
%% false
%%
%% 3> sc:is_sorted_list([1,2,3,false]).
%% true
%%
%% 4> sc:is_sorted_list([false,1,2,3]).
%% false'''

%% @since Version 514

is_sorted_list([]) ->

    true;





is_sorted_list([Head|Rem]) ->

    is_sorted_list_worker(Rem, Head).





is_sorted_list_worker([], _Last) ->

    true;





is_sorted_list_worker([Cur|Rem], Last) when Cur >= Last ->

    is_sorted_list_worker(Rem, Cur);





is_sorted_list_worker([_Cur|_Rem], _Last) ->

    false.





% todo comeback docs

%% @since Version 515

%% @doc <span style="color:orange;font-style:italic">Untested</span>

alarm_set(Time, Lambda, Repeat) ->

    { alarm_actor_pid, spawn( fun() -> alarm_head(Time, Lambda, Repeat, construct) end ) }.





% todo comeback docs

%% @since Version 515

%% @doc <span style="color:orange;font-style:italic">Untested</span>

alarm_terminate( { alarm_actor_pid, Pid } ) ->

    Pid ! terminate,
    ok.





%% @since Version 515

alarm_head(Time, Lambda, Repeat, construct) ->

    Head = self(),
    alarm_head(Time, Lambda, Repeat, spawn( fun() -> alarm_trigger(Time, Repeat, Head) end ));





alarm_head(Time, Lambda, Repeat, TriggerPid) ->

    receive

        terminate ->
            TriggerPid ! terminate,
            ok;

        trigger ->
            NextLambda = case is_list(Lambda) of
                true ->
                    [ThisLambda | RemLambda] = Lambda,
                    ThisLambda(),
                    RemLambda;
                false ->
                    Lambda(),
                    Lambda
            end,
            alarm_head(Time, NextLambda, Repeat, TriggerPid);

        UnknownMessage ->
            io:format("Warning: alarm head ~p took unexpected message.  Discarded:~n  ~w~n~n", [self(), UnknownMessage]),
            alarm_head(Time, Lambda, Repeat, TriggerPid)

    end.





%% @since Version 515

alarm_trigger(Time, Repeat, Head) ->

    receive

        terminate ->
            Head ! terminate,
            ok;

        UnknownMessage ->
            io:format("Warning: alarm trigger ~p took unexpected message; this screws with its timing cycle.  Discarded:~n  ~w~n~n", [self(), UnknownMessage]),
            alarm_trigger(Time, Repeat, Head)

    after Time ->

        Head ! trigger,

        case Repeat of
            1         -> Head ! terminate, ok;
            0         -> Head ! terminate, ok;
            no_repeat -> Head ! terminate, ok;
            forever   -> alarm_trigger(Time, forever, Head);
            Count     -> alarm_trigger(Time, Count-1, Head)
        end

    end.





%% @spec centroid(InputList::coord_list()) -> coord()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Calculates the coordinate which represents the per-axis arithmetic mean of a set of points.  Convenient in list comprehensions.  To calculate the centroid of `[1,1]', `[2,3]', you gather the X coordinates `[1,2]', then use their mean `1.5'; then do the same for the Y, `[1,3]' to `2'.  The centroid would thus be `[1.5,2]'.  You may pass any number of coordinates to this function, of any axis count, but they must all be the same axis count.  The return value will be a coordinate with the same axis count.  Negative and real values are fine; imaginary math is not implemented. ```1> sc:centroid([[1]]).
%% [1.0]
%%
%% 2> sc:centroid([[1,1],[2,2]]).
%% [1.5,1.5]
%%
%% 3> sc:centroid([[1,1,1],[2,2,2],[3,3,3]]).
%% [2.0,2.0,2.0]
%%
%% 4> sc:centroid([[1,-1,1.0],[-2,-2,-2],[3,3,3],[4,4,4],[5,5,5]]).
%% [2.2,1.8,2.2]'''

%% @since Version 516

centroid(CoordList) when is_list(CoordList) ->

    [ sc_stats:arithmetic_mean(X) ||
        X <- sc_lists:zip_n(CoordList, to_list)
    ].





% comeback todo docs

%% @since Version 517

%% @doc <span style="color:orange;font-style:italic">Untested</span>

nearest_to(Centers, Point) ->

    { C, _ } = sc_tuple:keymin(2, [ { Center, sc_distance:euclidean(Center, Point) } || Center <- Centers ]),
    C.





% comeback todo docs

%% @since Version 518

%% @doc <span style="color:orange;font-style:italic">Untested</span>

by_distance_raw(Centers, Points) when is_list(Centers), is_list(Points) ->

    [ {Key, [NV || {_NC,NV} <- Near]} || {Key, Near} <- sc_lists:keygroup(1, [ { nearest_to(Centers, Point), Point } || Point <- Points ]) ].





%% @since Version 519

%% @doc <span style="color:orange;font-style:italic">Untested</span>

by_distance(Centers, Points) when is_list(Centers), is_list(Points) ->

    Work = by_distance_raw(Centers, Points),

    Grab = fun(Center) ->
        case lists:keysearch(Center, 1, Work) of
            {value, {Center, Matches}} -> Matches;
            false                      -> []
        end
    end,

    [ Grab(Center) || Center <- Centers ].





% comeback todo to implement

% k_means(KCount, ItemList) when is_integer(KCount), is_list(CoordList) ->





%% @spec module_attribute(Module::atom()) -> AttributeList | { error, no_such_module }

%% @doc <span style="color:orange;font-style:italic">Untested</span> Look up all attributes of a given module.  ```1> sc:get_module_attribute(scutil).
%% [{author,"John Haugeland <stonecypher@gmail.com>"},
%%  {bugtracker,"http://crunchyd.com/forum/project.php?projectid=7"},
%%  {currentsource,"http://crunchyd.com/release/scutil.zip"},
%%  {description,"StoneCypher's utility library."},
%%  {library_requirements,[{testerl,16}]},
%%  {license,[{mit_license,"http://scutil.com/license.html"}]},
%%  {publicforum,"http://crunchyd.com/forum/scutil-discussion/"},
%%  {publicsvn,"svn://crunchyd.com/scutil/"},
%%  {svn_head,"$HeadURL$"},
%%  {svn_id,"$Id$"},
%%  {svn_revision,"$Revision$"},
%%  {testerl_export,[{[],scutil_testsuite}]},
%%  {vsn,[134633400955530778836494569152232539093]},
%%  {webpage,"http://scutil.com/"}]'''

%% @since Version 520

module_attribute(Module) ->

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->
            Attributes;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





%% @spec module_attribute(Module::atom(), Attribute::atom()) -> { value, {Attribute, Value} } | { error, no_such_attribute } | { error, no_such_module }

%% @doc <span style="color:red">Buggy</span> <span style="color:orange;font-style:italic">Untested</span> Look up an Erlang module attribute value by title.  Originally found at <a href="http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html">Mastering Erlang Part 3</a>; subsequently cleaned up and given error reporting.  ```1> sc:module_attribute(scutil, author).
%% "John Haugeland <stonecypher@gmail.com>"
%%
%% 2> sc:module_attribute(scutil, license).
%% [{mit_license,"http://scutil.com/license.html"}]'''{@section Thanks} to Alain O'Dea for pointing out defects in this routine regarding repeated module elements, and available improvements to the provided API.  <a href="http://fullof.bs/reading-module-attributes-in-erlang#comment-475" target="_blank">Mr. O'Dea's insightful advice</a> will be implemented, but that time has not yet come.

%% @since Version 520

module_attribute(Module,Attribute) ->

    % Found at http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html
    % Reformatted for clarity, removed unnessecary framing list
    % Added error handling behavior

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->

            case lists:keysearch(Attribute, 1, Attributes) of

                { value, {Attribute,Value} } ->
                    Value;

                false ->
                    { error, no_such_attribute }

            end;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





% todo comeback docs

%% @since Version 521

%% @doc <span style="color:orange;font-style:italic">Untested</span>

module_feature(Module, Feature) ->

    case beam_lib:chunks(Module, [Feature]) of

        { ok, { Module, [ {Feature,Attributes} ] } } ->
            Attributes;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





%% @spec svn_revision(ModuleName::atom()) -> integer()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Scans a module for an attribute svn_revision, parses it in the format expected from the svn:keyword Revision, and returns the version number as an integer.  To use, add a module attribute to your module as follows: `-svn_revision("$+Revision$).', after removing the plus (if the plus wasn't there, the example would get corrupted when I updated the module `;)').  Then set the svn keyword "Revision" on the file, and check it in.  After that, your version is magically updated every time you check in!  `:D'  The sole argument to this function is the name of the module to be scanned, as an atom. ```1> scutil:scan_svn_revision(testerl).
%% 16'''

%% @since Version 523

svn_revision(Module) ->

    "$Revision: " ++ X = module_attribute(Module, svn_revision),
    [ Head | _Rem ]    = string:tokens(X, " "),

    list_to_integer(Head).





% todo comeback docs

%% @since Version 524

%% @doc <span style="color:orange;font-style:italic">Untested</span>

function_stats(Module) ->

    [ { entrypoints,     entrypoint_count(Module)     },
      { function_labels, function_label_count(Module) },
      { function_points, function_point_count(Module) }
    ].





% todo comeback docs

%% @since Version 525

%% @doc <span style="color:orange;font-style:italic">Untested</span>

function_point_count(Module) ->

    length(function_points(Module)).





% todo comeback docs

%% @since Version 526

%% @doc <span style="color:orange;font-style:italic">Untested</span>

function_label_count(Module) ->

    length(function_labels(Module)).





% todo comeback docs

%% @since version 527

%% @doc <span style="color:orange;font-style:italic">Untested</span>

entrypoint_count(Module) ->

    length(entrypoints(Module)).





% todo comeback docs

%% @since Version 528

%% @doc <span style="color:orange;font-style:italic">Untested</span>

function_labels(Module) ->

    lists:usort(
        [ L ||
            {_,L,_,_} <- abstract_functions(Module)
        ]
    ).





% todo comeback docs

%% @since Version 529

%% @doc <span style="color:orange;font-style:italic">Untested</span>

function_points(Module) ->

    lists:usort(
        [ {L,A} ||
            {_,L,A,_} <- abstract_functions(Module)
        ]
    ).





% todo comeback docs

%% @since Version 530

%% @doc <span style="color:orange;font-style:italic">Untested</span>

entrypoints(Module) ->

    lists:flatten(
        [ [{L,A,[{Kind,Name}||{Kind,_LineNum,Name}<-ThisAcArg],When} || {_,_,ThisAcArg,When,_} <- AbstractClauseList ] || {_,L,A,AbstractClauseList} <- sc:abstract_functions(Module) ]
    ).





% todo comeback docs

%% @since Version 530

%% @doc <span style="color:orange;font-style:italic">Untested</span>

entrypoints(Module, FName) ->

    lists:flatten(
        [ [{L,A,[{Kind,Name}||{Kind,_LineNum,Name}<-ThisAcArg],When} || {_,_,ThisAcArg,When,_} <- AbstractClauseList ] || {_,L,A,AbstractClauseList} <- abstract_functions(Module), L==FName ]
    ).





% todo comeback docs

%% @since version 531

%% @doc <span style="color:orange;font-style:italic">Untested</span>

abstract_functions(Module) ->

    [ {Id, Name, Arity, Code} ||
        {function, Id, Name, Arity, Code} <- abstract(Module, stripped)
    ].





% todo comeback docs

%% @since version 531

%% @doc <span style="color:orange;font-style:italic">Untested</span>

abstract_function(Module, FName) ->

    [ {Id, Name, Arity, Code} ||
        {function, Id, Name, Arity, Code} <- abstract(Module, stripped),
        Name == FName
    ].





% todo comeback docs

%% @since version 532

%% @doc <span style="color:orange;font-style:italic">Untested</span>

abstract_attributes(Module) ->

    [ {Id, Name, Value} ||
        {attribute, Id, Name, Value} <- abstract(Module, stripped)
    ].





%% @since Version 533

%% @doc <span style="color:orange;font-style:italic">Untested</span>

module_atoms(Module) ->

    module_feature(Module, atoms).





% todo comeback docs

%% since Version 534

%% @doc <span style="color:orange;font-style:italic">Untested</span>

key_cluster(_Index, []) ->

    [];


% 177> sc:key_cluster(1,[{1,a},{1,aa},{2,a}]).
% [{1,[{1,a},{1,aa}]},{2,[{2,a}]}]

key_cluster(Index, List) ->

    SortedList = lists:keysort(Index, List),
    [First|_]  = List,
    Current    = element(Index, First),

    key_cluster(Index, SortedList, Current, [], []).



key_cluster(Index, [], _Current, Work, Storage) ->

    % Not happy about this, but too lazy to fix it.

    PossibleResult = lists:reverse([ lists:reverse(Sublist) || Sublist <- ([Work] ++ Storage) ]),

    [RemoveEmptyHeadCons|FixedResult] = PossibleResult,

    FinalResult = case RemoveEmptyHeadCons of
        [] -> FixedResult;
        _  -> PossibleResult
    end,

    LabelItem = fun(Item) -> [X|_] = Item, Label = element(Index, X), { Label, Item } end,

    [ LabelItem(Item) || Item <- FinalResult ];



key_cluster(Index, [WorkItem|Rem], Current, Work, Storage) ->

    case element(Index, WorkItem) of
        Current -> key_cluster(Index, Rem, Current, [WorkItem]++Work, Storage);
        Other   -> key_cluster(Index, Rem, Other,   [WorkItem],       [Work]++Storage)
    end.





% todo comeback docs

% @since Version 535

%% @doc <span style="color:orange;font-style:italic">Untested</span>

distinct_neighbor_pairs(List) ->

    distinct_neighbor_pairs(List, [], make_tuples).





% todo comeback docs

% @since Version 535

%% @doc <span style="color:orange;font-style:italic">Untested</span>

distinct_neighbor_pairs(List, MakeType) ->

    distinct_neighbor_pairs(List, [], MakeType).





distinct_neighbor_pairs([], Work, _Make_type) ->

    lists:reverse(Work);  % should actually only ever be an empty list anyway





distinct_neighbor_pairs([[_LastItemIsNotInAPairByItself]], Work, _Make_type) ->

    lists:reverse(Work);





distinct_neighbor_pairs([[]], Work, _Make_type) ->

    lists:reverse(Work);





distinct_neighbor_pairs([A,B|Rem], Work, make_tuples) ->

    distinct_neighbor_pairs(Rem, [{A,B}] ++ Work, make_tuples);





distinct_neighbor_pairs([A,B|Rem], Work, make_lists) ->

    distinct_neighbor_pairs(Rem, [[A,B]] ++ Work, make_lists).





% todo comeback docs

%% @since Version 536

%% @doc <span style="color:orange;font-style:italic">Untested</span>

all_neighbor_pairs(List) ->

    all_neighbor_pairs(List, make_tuples).



% todo comeback docs

%% @since Version 536

%% @doc <span style="color:orange;font-style:italic">Untested</span>

all_neighbor_pairs(List, WorkType) ->

    all_neighbor_pairs(List, [], WorkType).





all_neighbor_pairs([], Work, _Work_type) ->

    lists:reverse(Work);  % should actually only ever be an empty list anyway





all_neighbor_pairs([[]], Work, _Work_type) ->

    lists:reverse(Work);





all_neighbor_pairs([_LastItemIsNotInAPairByItself], Work, _Work_type) ->

    lists:reverse(Work);





all_neighbor_pairs([A,B|Rem], Work, make_lists) ->

    all_neighbor_pairs([B]++Rem, [[A,B]] ++ Work, make_lists);





all_neighbor_pairs([A,B|Rem], Work, make_tuples) ->

    all_neighbor_pairs([B]++Rem, [{A,B}] ++ Work, make_tuples).





%% @since Version 537

%% @doc <span style="color:orange;font-style:italic">Untested</span>

partition_by_residue(Data, Function) ->

    [
        { GroupId, [   GDatum
                   ||  { _H, GDatum } <- GroupData
                   ]
        }
    ||
        { GroupId, GroupData } <- keygroup( 1, [   { Function(Datum), Datum }
                                               ||  Datum <- Data
                                               ]
                                          )
    ].





% comeback todo docs

%% @since Version 538
%%
%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns the last element of the initial sequence where all items pass the predicate function.  ```1> sc_lists:last_while_pos(fun erlang:is_atom/1, [a,b,c,d,2,f]).
%% 4
%%
%% 2> sc_lists:last_while_pos(fun erlang:is_atom/1, [a,b,c,d,r,f]).
%% 6
%%
%% 3> sc_lists:last_while_pos(fun erlang:is_atom/1, [1,a,b,c,d,r,f]).
%% false'''

last_while_pos(Predicate, List) ->

    last_while_pos(1, Predicate, List, false).





%% @doc <span style="color:orange;font-style:italic">Untested</span>

last_while_pos(Predicate, List, Default) ->

    last_while_pos(1, Predicate, List, Default).





last_while_pos(_N, [],  _Pred, Last) ->

    Last;





last_while_pos(N, [Head|Tail], Pred, Last) ->

    case Pred(Head) of
        true  -> last_while_pos(N+1, Tail, Pred, N);
        false -> Last
    end.





%% @since Version 538

%% @doc <span style="color:orange;font-style:italic">Untested</span>

keygroup(Pos, List) ->

    keygroup(Pos, List, unsorted).





%% @since Version 538

%% @doc <span style="color:orange;font-style:italic">Untested</span>

keygroup(Pos, List, unsorted) when is_list(List) ->

    SList = lists:keysort(Pos,List),
    [F|_] = SList,

    keygroup(Pos, SList, element(Pos,F), [], []);





%% @since Version 538

%% @doc <span style="color:orange;font-style:italic">Untested</span>

keygroup(Pos, List, sorted) when is_list(List) ->

    [F|_] = List,

    keygroup(Pos, List, element(Pos,F), [], []).





keygroup(_Pos, [], WorkKey, Work, Output) ->

    [{WorkKey, Work}] ++ Output;





keygroup(Pos, [Item|Rem], WorkKey, Work, Output) ->

    NewKey = element(Pos, Item),

    case NewKey == WorkKey of

        true  ->
            keygroup(Pos, Rem, WorkKey, [Item]++Work, Output);

        false ->
            keygroup(Pos, Rem, NewKey,  [Item],       [{WorkKey,Work}]++Output)

    end.





% @todo swap argument order

%% @since Version 539
%%
%% @doc <span style="color:orange;font-style:italic">Untested</span> Finds the 1-offset index of the first item in the list which passes the given predicate, or returns false if none pass. ```1> sc_lists:first_pos([a,b,c,d,2,f],fun erlang:is_integer/1).
%% 5
%%
%% 2> sc_lists:first_pos([a,b,c,d,e,f],fun erlang:is_integer/1).
%% false'''

first_pos(List, Predicate) ->

    first_pos(1, List, Predicate, false).





%% @doc <span style="color:orange;font-style:italic">Untested</span> Finds the 1-offset index of the first item in the list which passes the given predicate, or returns a default value if none is found.  See {@link first_pos/2} for details.

first_pos(List, Predicate, Default) ->

    first_pos(1, List, Predicate, Default).





first_pos(_N, [],  _Pred, Default) ->

    Default;





first_pos(N, [Head|Tail], Pred, Default) ->

    case Pred(Head) of
        true  -> N;
        false -> first_pos(N+1, Tail, Pred, Default)
    end.





%% @since Version 541 TODO

%% @doc <span style="color:orange;font-style:italic">Untested</span>

split_at(N, List) ->

    split_at(N, N, List, [], []).





split_at(_N, _BlockN, [], Current, Work) ->

    lists:reverse([lists:reverse(Current)] ++ Work);





split_at(N, 0, Workload, Current, Work) ->

    split_at(N, N, Workload, [], [lists:reverse(Current)] ++ Work);





split_at(N, BN, [Item|Rem], Current, Work) ->

    split_at(N, BN-1, Rem, [Item]++Current, Work).





%% @since Version 543

%% @doc <span style="color:orange;font-style:italic">Untested</span>

is_postfix(Postfix, String) ->

    lists:prefix(lists:reverse(Postfix), lists:reverse(String)).





%% @since Version 544

%% @doc <span style="color:orange;font-style:italic">Untested</span>

reverse_map_filter(Workload, MapFun, FilterFun) ->

    reverse_map_filter(Workload, [], MapFun, FilterFun).





%% @since Version 544

reverse_map_filter([], Work, _MapFun, _FilterFun) ->

    Work;





%% @since Version 544

reverse_map_filter([Item|Rem], Work, MapFun, FilterFun) ->

    Res = MapFun(Item),

    case FilterFun(Res) of
        true  -> reverse_map_filter(Rem, [Res]++Work, MapFun, FilterFun);
        false -> reverse_map_filter(Rem, Work,        MapFun, FilterFun)
    end.





%% @since Version 545

%% @doc <span style="color:orange;font-style:italic">Untested</span>

reverse_filter(Workload, Fun) ->

    reverse_filter(Workload, [], Fun).





%% @since Version 545

reverse_filter([], Work, _Fun) ->

    Work;





%% @since Version 545

reverse_filter([Item|Rem], Work, Fun) ->

    case Fun(Item) of
        true  -> reverse_filter(Rem, [Item]++Work, Fun);
        false -> reverse_filter(Rem, Work,         Fun)
    end.





%% @since Version 546

%% @doc <span style="color:orange;font-style:italic">Untested</span>

reverse_map(Workload, Fun) ->

    reverse_map(Workload, [], Fun).





%% @since Version 546

reverse_map([], Work, _Fun) ->

    Work;





%% @since Version 546

reverse_map([Item|Rem], Work, Fun) ->

    reverse_map(Rem, [Fun(Item)]++Work, Fun).





% todo implement catching tuple { key, reqtype } from list, to auto-convert before return
% todo There may be a crashing bug here for repeated attributes, which are apparently legal, see http://fullof.bs/reading-module-attributes-in-erlang#comment-466
% todo It may help to re-implement this using proplists instead of doing it manually, profile
%% @todo document this

%% @doc <span style="color:orange;font-style:italic">Untested</span>

%% @since Version 546

% interface

elements(Config, Requested)                when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1).

%% @doc <span style="color:orange;font-style:italic">Untested</span>
elements(Config, Requested, KeyIdx)        when is_list(Config), is_list(Requested), is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx);

%% @doc <span style="color:orange;font-style:italic">Untested</span>
elements(Config, Requested, strip)         when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1,      strip).

%% @doc <span style="color:orange;font-style:italic">Untested</span>
elements(Config, Requested, KeyIdx, strip) when is_list(Config), is_list(Requested), is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx, strip).





% implementation

elements_worker(Retlist, _,      [],        _)      -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of

        false ->
            elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx);

        { value, Tuple } ->
            elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx);

        AnythingElse ->
            { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }

    end.





elements_worker(Retlist, _,      [],        _,      strip) -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx, strip) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of

        false ->
            elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx, strip);

        { value, {_,Tuple} } ->
            elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx, strip);

        AnythingElse ->
            { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }

    end.





%% @since Version 547

%% @doc <span style="color:orange;font-style:italic">Untested</span>

differences(List) when is_list(List), length(List) > 2 ->

    [ B-A || {A,B} <- all_neighbor_pairs(List) ].





%% @since Version 547

%% @doc <span style="color:orange;font-style:italic">Untested</span>

first_difference(List) when is_list(List), length(List) > 2 ->

    differences(List).





%% @since Version 547

%% @doc <span style="color:orange;font-style:italic">Untested</span>

second_difference(List) when is_list(List), length(List) > 3 ->

    differences(differences(List)).





%% @since Version 547

%% @doc <span style="color:orange;font-style:italic">Untested</span>

third_difference(List) when is_list(List), length(List) > 4 ->

    differences(differences(differences(List))).





%% @since Version 547

%% @doc <span style="color:orange;font-style:italic">Untested</span>

% todo comeback implement the neato solver from http://www.y-maths.co.uk/sequen2.htm

nth_difference(0, List) ->

    List;





nth_difference(N, List) when is_list(List), length(List) > (N+1), N > 0 ->

    nth_difference(N-1, differences(List)).





% used for side effects, doesn't gather results; appropriate for enormous lists

% comeback

%% @doc <span style="color:orange;font-style:italic">Untested</span>

%% @since Version 547

walk_unique_pairings([], _) ->

    ok;



walk_unique_pairings([A|R], F) when is_function(F) ->

    walk_unique_pairings(A, R, F),
    walk_unique_pairings(R, F).



walk_unique_pairings(_A, [],     _F) ->

    ok;



walk_unique_pairings( A, [Rh|Rr], F) ->

    F(A,Rh),
    walk_unique_pairings(A, Rr, F).





%% @type non_negative_integer() = integer().  A {@type non_negative_integer()} must be equal to or greater than zero.

%% @spec count_of(Item::any(), List::list()) -> non_negative_integer()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Counts the number of instances of Item in List.  ```1> TestData = lists:duplicate(40,[healthy,nonsmoker]) ++ lists:duplicate(10,[healthy,smoker]) ++ lists:duplicate(7,[cancer,nonsmoker]) ++ lists:duplicate(3,[cancer,smoker]).
%% [[healthy,nonsmoker], [healthy,nonsmoker], [healthy|...], [...]|...]
%%
%% 2> sc:count_of([healthy,smoker], TestData).
%% 10
%%
%% 3> sc:count_of([healthy,nonsmoker], TestData).
%% 40'''

%% @since Version 550

count_of(Item, List) ->

    lists:foldl(

        fun(X, Counter) ->

            case X of

                Item ->
                    Counter+1;

                _ ->
                    Counter

            end

        end,

        0,
        List

    ).





%% @equiv every_member_representation(Memberships, no_absence)

%% @doc <span style="color:orange;font-style:italic">Untested</span>

every_member_representation(Memberships) ->

    every_member_representation(Memberships, no_absence).





%% @spec every_member_representation(Memberships::list_of_lists(), AllowAbsence::atom()) -> list_of_lists()

%% @doc <span style="color:orange;font-style:italic">Untested</span> For a list of memberships, return every possible combination of one representative member from each list.  The parameter `AllowAbsence' controls whether memberships may be unrepresented; if unrepresented memberships are possible, then one possible representation becomes the empty list. ```1> scutil:every_member_representation([ [a,b],[1,2,3],[i,ii,iii] ], no_absence).
%% [[a,1,i], [a,1,ii], [a,1,iii], [a,2,i], [a,2,ii], [a,2,iii], [a,3,i], [a,3,ii], [a,3,iii], [b,1,i], [b,1,ii], [b,1,iii], [b,2,i], [b,2,ii], [b,2,iii], [b,3,i], [b,3,ii], [b,3,iii]]
%%
%% 2> scutil:every_member_representation([ [a,b],[1,2],[i,ii] ], allow_absence).
%% [ [], [i], [ii], [1], [1,i], [1,ii], [2], [2,i], [2,ii], [a], [a,i], [a,ii], [a,1], [a,1,i], [a,1,ii], [a,2], [a,2,i], [a,2,ii], [b], [b,i], [b,ii], [b,1], [b,1,i], [b,1,ii], [b,2], [b,2,i], [b,2,ii] ]
%%
%% 3> Format = fun(Person, Place, Weapon) -> "It was " ++ Person ++ " in the " ++ Place ++ " with the " ++ Weapon ++ "!" end.
%% #Fun<erl_eval.18.105910772>
%%
%% 4> { People, Places, Weapons } = { ["Col. Mustard", "Mr. Green"], ["the billiards room", "the kitchen"], ["a lead pipe", "a knife", "a gun"] }.
%% {["Col. Mustard","Mr. Green"],
%%  ["the billiards room","the kitchen"],
%%  ["a lead pipe","a knife","a gun"]}
%%
%% 5> Places.
%% ["the billiards room","the kitchen"]
%%
%% 6> Format("Mrs. Scarlett", "the observatory", "a noose").
%% "It was Mrs. Scarlett in the the observatory with the a noose!"
%%
%% 7> EveryClueOutcome = [ Format(ThisPerson, ThisPlace, ThisWeapon) || ThisPerson <- People, ThisPlace <- Places, ThisWeapon <- Weapons ].
%% ["It was Col. Mustard in the the billiards room with the a lead pipe!",
%%  "It was Col. Mustard in the the billiards room with the a knife!",
%%  "It was Col. Mustard in the the billiards room with the a gun!",
%%  "It was Col. Mustard in the the kitchen with the a lead pipe!",
%%  "It was Col. Mustard in the the kitchen with the a knife!",
%%  "It was Col. Mustard in the the kitchen with the a gun!",
%%  "It was Mr. Green in the the billiards room with the a lead pipe!",
%%  "It was Mr. Green in the the billiards room with the a knife!",
%%  "It was Mr. Green in the the billiards room with the a gun!",
%%  "It was Mr. Green in the the kitchen with the a lead pipe!",
%%  "It was Mr. Green in the the kitchen with the a knife!",
%%  "It was Mr. Green in the the kitchen with the a gun!"]'''
%%
%% @since Version 551

every_member_representation([], _) ->

    [[]];





every_member_representation( [Membership|RemMemberships], no_absence   ) ->

    [ [Member] ++ RemRep ||
        Member <- Membership,
        RemRep <- every_member_representation(RemMemberships, no_absence)
    ];





every_member_representation( [Membership|RemMemberships], allow_absence) ->

    Compact = fun(Member, RemRep) ->

        case Member of

            empty ->
                RemRep;

            {item,X} ->
                [X] ++ RemRep

        end

    end,

    [ Compact(Member, RemRep) ||
        Member <- [empty] ++ [{item,X}||X<-Membership],
        RemRep <- every_member_representation(RemMemberships, allow_absence)
    ].





%% @spec every_flag_representation(Flags::list()) -> list_of_lists()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns every interpretation of the list as a set of boolean flags, including all-off and all-on. ```1> scutil:every_flag_representation([1,2,3,4]).
%% [ [], [4], [3], [3,4], [2], [2,4], [2,3], [2,3,4], [1], [1,4], [1,3], [1,3,4], [1,2], [1,2,4], [1,2,3], [1,2,3,4] ]
%%
%% 2> length(scutil:every_flag_representation(lists:seq(1,16))).
%% 65536
%%
%% 3> SourceOfPowers = scutil:every_flag_representation([magic,technology,evil,alien]).
%% [[],                              % Batman
%%  [alien],                         % Superman
%%  [evil],                          % Darkseid
%%  [evil,alien],                    % Sinestro
%%  [technology],                    % Mister Terrific (Michael Holt)
%%  [technology,alien],              % The Blue Beetle
%%  [technology,evil],               % The OMACs
%%  [technology,evil,alien],         % Braniac
%%  [magic],                         % Shazam
%%  [magic,alien],                   % Green Lantern (Alan Scott)
%%  [magic,evil],                    % Lucifer Morningstar
%%  [magic,evil,alien],              % pre-crisis Star Sapphire
%%  [magic,technology],              % Alexander Luthor Jr.
%%  [magic,technology,alien],        % Mister Miracle
%%  [magic,technology,evil],         % pre-crisis Sinestro
%%  [magic,technology,evil,alien]]   % Granny Goodness'''

%% @since Version 552

every_flag_representation([]) ->

    [[]];




every_flag_representation([Flag|RemFlags]) ->

    [ MaybeFlag ++ Reps ||
        MaybeFlag <- [[],[Flag]],
        Reps      <- every_flag_representation(RemFlags)
    ].





%% @since Version 553

%% @doc <span style="color:orange;font-style:italic">Untested</span>

abstract(Module) ->

    abstract(Module, unstripped).





%% @since Version 553

%% @doc <span style="color:orange;font-style:italic">Untested</span>

abstract(Module, DoStrip) ->

    case module_feature(Module, abstract_code) of

        { raw_abstract_v1, ACode } ->

            case DoStrip of

                stripped ->
                    ACode;

                unstripped ->
                    { raw_abstract_v1, ACode }

            end;

        no_abstract_code ->

            { error, "ScUtil's abstract code functions require that a module be compiled with debug_info enabled, eg 'c(" ++ atom_to_list(Module) ++ ",[debug_info]).'" }

    end.





%% @since Version 556
% modified from http://www.trapexit.org/String_Eval

%% @doc <span style="color:orange;font-style:italic">Untested</span>

eval(S) ->

    eval(S,erl_eval:new_bindings()).





%% @since Version 556
% from http://www.trapexit.org/String_Eval

%% @doc <span style="color:orange;font-style:italic">Untested</span>

eval(S, Environ) ->

    {ok, Scanned,_} = erl_scan:string(S),
    {ok, Parsed}    = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed,Environ).





%% @todo use test data at http://changingminds.org/explanations/research/analysis/kendall.htm

%% @spec kendall_correlation(TupleList::coordlist()) -> { tau, Correlation::number() }

%% @doc <span style="color:orange;font-style:italic">Untested</span> Compute the Kendall Tau Rank Correlation Coefficient of a list of coordinate tuples. ```1> scutil:kendall([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {tau,1.0}
%%
%% 2> scutil:kendall([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {tau,-1.0}
%%
%% 3> scutil:kendall([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {tau,1.0}
%%
%% 4> scutil:kendall([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {tau,1.0}
%%
%% 5> scutil:kendall([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {tau,1.0}'''

%% @since Version 557

kendall_correlation(TupleList) when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    kendall_correlation(A,B).





%% @equiv kendall(lists:zip(List1, List2))

kendall_correlation(List1, _) when length(List1) < 2 ->

    {tau, 0.0};





kendall_correlation(List1, List2) when length(List1) /= length(List2) ->

    {error, "For the Kendall correlation, the input lists must be same length."};





kendall_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {RA,_} = lists:unzip(tied_ordered_ranking(List1)),
    {RB,_} = lists:unzip(tied_ordered_ranking(List2)),

    Ordering = lists:keysort(1, lists:zip(RA,RB)),
    {_,OrdB} = lists:unzip(Ordering),

    N = length(List1),
    P = lists:sum(kendall_right_of(OrdB, [])),

    {tau, -(( (4*P) / (N * (N - 1))) - 1) }.





%% @private

kendall_right_of([], Work) ->

    lists:reverse(Work);



kendall_right_of([F|R], Work) ->

    kendall_right_of(R, [kendall_right_of_item(F,R)]++Work).



kendall_right_of_item(B, Rem) ->

    length([R || R <- Rem, R < B]).





%% @todo use test data at http://geographyfieldwork.com/SpearmansRank.htm

%% @spec spearman_correlation(TupleList::coordlist()) -> { rsquared, Correlation::number() }

%% @doc <span style="color:orange;font-style:italic">Untested</span> Compute the Spearman's Rank Correlation Coefficient of a list of coordinate tuples. ```1> scutil:spearman([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {rsquared,1.0}
%%
%% 2> scutil:spearman([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {rsquared,-1.0}
%%
%% 3> scutil:spearman([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {rsquared,0.5}
%%
%% 4> scutil:spearman([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {rsquared,1.0}
%%
%% 5> scutil:spearman([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {rsquared,1.0}'''

%% @since Version 558

spearman_correlation(TupleList) when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    spearman_correlation(A,B).





%% @equiv spearman_correlation(lists:zip(List1, List2))

spearman_correlation(List1, _) when length(List1) < 2 ->

    {rsquared, 0.0};





spearman_correlation(List1, List2) when length(List1) /= length(List2) ->

    {error, "For the Spearman correlation, the input lists must be the same length."};





spearman_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {TR1,_} = lists:unzip(simple_ranking(List1)),
    {TR2,_} = lists:unzip(simple_ranking(List2)),

    Numerator   = 6 * lists:sum([ square(D1-D2) || {D1,D2} <- lists:zip(TR1,TR2) ]),
    Denominator = math:pow(length(List1),3)-length(List1),

    {rsquared, 1-(Numerator/Denominator) }.





%% @todo use test data at http://changingminds.org/explanations/research/analysis/pearson.htm

%% @spec pearson_correlation(TupleList::coordlist()) -> { r, Correlation::number() }

%% @doc <span style="color:orange;font-style:italic">Untested</span> Compute the Pearson Correlation Coefficient of a list of coordinate tuples. ```1> sc_correlate:pearson([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {r,1.0}
%%
%% 2> sc_correlate:pearson([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {r,-1.0}
%%
%% 3> sc_correlate:pearson([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {r,0.0}
%%
%% 4> sc_correlate:pearson([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {r,1.0}
%%
%% 5> sc_correlate:pearson([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {r,0.9970544855015818}'''
%%
%% @since Version 559

pearson_correlation(TupleList) when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    pearson_correlation(A,B).



%% @equiv pearson(lists:zip(List1, List2))

pearson_correlation(List1, _) when length(List1) < 2 ->

    {r, 0.0};



pearson_correlation(List1, List2) when length(List1) /= length(List2) ->

    {error, "For the Pearson correlation, the input lists must be the same length."};



pearson_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    SumXY = lists:sum([A*B || {A,B} <- lists:zip(List1,List2) ]),   % the sum of the products of each matched pair

    SumX  = lists:sum(List1),
    SumY  = lists:sum(List2),

    SumXX = lists:sum([L*L || L<-List1]),                           % the sums of the squared items
    SumYY = lists:sum([L*L || L<-List2]),

    N     = length(List1),

    case math:sqrt(   ( (N*SumXX)-(SumX*SumX) )   *   ( (N*SumYY)-(SumY*SumY) )   ) of

        0 ->
            {r, 0.0};  % some nasty value sets otherwise cause divide by zero


        0.0 ->
            {r, 0.0};  % eg [ [1,1,1,1,1], [1,1,2,1,2] ]

        Denom ->
          Numer = (N*SumXY) - (SumX * SumY),
          {r, (Numer/Denom)}

    end.





%% @type ranking() = { Ranking::number(), Value::any() }.  Values are usually {@type number()}s, but do not have to be with custom ranking predicates.
%% @type rankinglist() = list().  Members of a {@type rankinglist()} must be {@type ranking()}s.

%% @todo comeback make a simple/2 which takes a sorting predicate
%% @spec simple_ranking(Values::numericlist()) -> rankinglist()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns a ranked ordering of the list without tie rankings.  ```1> sc_rank:simple([10,90,20,80,30,70,40,60,50]).
%% [{1,90}, {2,80}, {3,70}, {4,60}, {5,50}, {6,40}, {7,30}, {8,20}, {9,10}]
%%
%% 2> sc_rank:simple([10,10,10,10]).
%% [{1,10},{2,10},{3,10},{4,10}]'''

%% @since Version 560

simple_ranking(List) when is_list(List) ->

    lists:zip(lists:seq(1,length(List)),lists:reverse(lists:sort(List))).





%% @todo comeback make a tied/2 which takes a sorting predicate
% needs significant refactoring; work is being repeated

%% @spec tied_ranking(Values::numericlist()) -> rankinglist()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns a ranked ordering of the list with tie rankings.  As such, for uniformity, all rankings are floats.  Ties are represented as the centers of ranges. ```1> scutil:tied([10,90,20,80,30,70,40,60,50]).
%% [{1.0,90}, {2.0,80}, {3.0,70}, {4.0,60}, {5.0,50}, {6.0,40}, {7.0,30}, {8.0,20}, {9.0,10}]
%%
%% 2> scutil:tied([100,200,200,300]).
%% [{1.0,300},{2.5,200},{2.5,200},{4.0,100}]'''

%% @since Version 561

tied_ranking(List) ->

    tied_rank_worker(simple_ranking(List), [], no_prev_value).





%% @private

tied_add_prev(Work, {FoundAt, NewValue}) ->

    lists:duplicate( length(FoundAt), {lists:sum(FoundAt)/length(FoundAt), NewValue} ) ++ Work.





%% @private

tied_rank_worker([], Work, PrevValue) ->

    lists:reverse(tied_add_prev(Work, PrevValue));



tied_rank_worker([Item|Remainder], Work, PrevValue) ->

    case PrevValue of

        no_prev_value ->
            {BaseRank,BaseVal} = Item,
            tied_rank_worker(Remainder, Work, {[BaseRank],BaseVal});

        {FoundAt,OldVal} ->

            case Item of

                {Id,OldVal} ->
                    tied_rank_worker(Remainder, Work,                           {[Id]++FoundAt,OldVal});

                {Id,NewVal} ->
                    tied_rank_worker(Remainder, tied_add_prev(Work, PrevValue), {[Id],NewVal})

            end
    end.





%% @todo comeback make a tied/2 which takes a sorting predicate

%% @spec tied_ordered_ranking(Values::numericlist()) -> rankinglist()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns a tied ranked ordering of the list, ordered according to the input ordering rather than the sorted ordering.  As with {@link tied/1}, all rankings are floats, and ties are represented as the centers of ranges. ```1> scutil:ordered([10,90,20,80,30,70,40,60,50]).
%% [{9.0,10}, {1.0,90}, {8.0,20}, {2.0,80}, {7.0,30}, {3.0,70}, {6.0,40}, {4.0,60}, {5.0,50}]
%%
%% 2> scutil:ordered([100,200,200,300]).
%% [{4.0,100},{2.5,200},{2.5,200},{1.0,300}]'''

%% @since Version 562

tied_ordered_ranking(List) when is_list(List) ->

    tied_ordered_ranking(List, tied_ranking(List), []).



tied_ordered_ranking([], [], Work) ->

    lists:reverse(Work);



tied_ordered_ranking([Front|Rem], Ranks, Work) ->

    {value,Item}  = lists:keysearch(Front,2,Ranks),
    {IRank,Front} = Item,
    tied_ordered_ranking(Rem, Ranks--[Item], [{IRank,Front}]++Work).





%% @since Version 564

%% @doc <span style="color:orange;font-style:italic">Untested</span>

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands) ->

    halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, brief).





%% @since Version 564

%% @doc <span style="color:orange;font-style:italic">Untested</span>

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, brief) ->

    { Effort, _ } = halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, complete),
    Effort;





%% @since Version 564

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, complete) ->

    ProgramLength     = TotalOperators    + TotalOperands,
    ProgramVocabulary = DistinctOperators + DistinctOperands,

    Volume            = ProgramLength         * (math:log(ProgramVocabulary)),
    Difficulty        = (DistinctOperators/2) * (TotalOperands/DistinctOperands),

    Effort            = Volume * Difficulty,

    { Effort, [{volume, Volume}, {difficulty, Difficulty}, {program_length, ProgramLength}, {program_vocabulary, ProgramVocabulary}] }.





%% @spec integer_to_radix_list(Number::number(), Radix::tuple()) -> list()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Convert a number to a radix string using a radix list of your specification and any size.  When appropriate, prefer the system provided `erlang:integer_to_list/2'.  Lists are accepted, but converted to tuples before use, so are inefficient.  ```1> sc_convert:integer_to_radix_list(1111, "0123456789abcdef").
%% "457"
%%
%% 2> sc_convert:integer_to_radix_list(1111, "0123456789").
%% "1111"
%%
%% 3> sc_convert:integer_to_radix_list(1234567890, "abcdefghij").
%% "bcdefghija"
%%
%% 4> sc_convert:integer_to_radix_list(12648430, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F}).
%% "C0FFEE"
%%
%% 5> sc_convert:integer_to_radix_list(1234567890, [alpha, beta, gamma, delta, epsilon, zeta, eta, theta, kappa, lambda]).
%% [beta,gamma,delta,epsilon,zeta,eta,theta,kappa,lambda,alpha]'''

%% @since Version 566

integer_to_radix_list(Number, RadixItems) when is_tuple(RadixItems) ->

    Radix = size(RadixItems),
    [ element(Ch+1, RadixItems) || Ch <- downshift_radix([], Number, Radix) ];




integer_to_radix_list(Number, RadixList) when is_list(RadixList) ->

    integer_to_radix_list(Number, list_to_tuple(RadixList)).





%% @since Version 567

%% @doc <span style="color:orange;font-style:italic">Untested</span>

downshift_radix(InStep, 0, _Radix) ->

    InStep;





downshift_radix(InStep, Number, Radix) ->

    downshift_radix([Number rem Radix]++InStep, trunc((Number-(Number rem Radix)) / Radix), Radix ).





% Like binary_to_term, but not so much for binaries
% thanks dizzyd (modified for error reporting)



%% @since Version 568

%% @doc <span style="color:orange;font-style:italic">Untested</span>

list_to_term(List) ->

    case catch erl_scan:string(List) of

        { ok, Tokens, _ } ->

            case erl_parse:parse_term( Tokens ++ [{ dot, 1 }] ) of
                { ok,Term } -> Term;
                Error       -> { error, Error }
            end;

        Error -> { error, Error }

    end.





%% @type io_list() = list().  Every list member of an {@type io_list()} must be a {@type byte()}.

%% @spec io_list_to_hex_string(Input::io_list()) -> hexstring()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Convert an io_list() to a hexstring().  ```1> scutil:io_list_to_hex_string("a").
%% "61"
%%
%% 2> scutil:io_list_to_hex_string("a08n408nbqa").
%% "6130386e3430386e627161"'''

%% @since Version 569

io_list_to_hex_string(Input) when is_list(Input) ->

    io_list_to_hex_string(Input, []).





io_list_to_hex_string([], Work) ->

    lists:reverse(Work);





io_list_to_hex_string([Item|Remainder], Work) when is_integer(Item), Item >= 0, Item =< 255 ->

    [A,B] = byte_to_hex(Item),
    io_list_to_hex_string(Remainder, [B,A]++Work);





io_list_to_hex_string(_, _) ->

    {error, not_an_io_list}.





%% @type nybble() = integer().  A nybble must be an integer in the range 0-15, inclusive.

%% @spec nybble_to_hex(Nyb::nybble()) -> integer()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Convert a nybble() to a hexchar(). ```1> scutil:nybble_to_hex(7).
%% 55
%%
%% 2> scutil:nybble_to_hex(15).
%% 102'''

%% @since Version 570

nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 0,  Nyb < 10 ->

    $0 + Nyb;



nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 10, Nyb < 16 ->

    $a + Nyb - 10.





%% @type byte() = integer().  A byte must be an integer in the range 0-255, inclusive.  (Technically this is an octet, not a byte, but the word byte is extensively misused throughout the erlang documentation and standard library, which makes this an important concession, so we're when-in-Rome-ing.)

%% @spec byte_to_hex(TheByte::byte()) -> hexstring()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Convert a byte() into a hexstring().  The hexstring() result will always be two characters (left padded with zero if necessary). ```1> scutil:byte_to_hex(7).
%% "07"
%%
%% 2> scutil:byte_to_hex(255).
%% "ff"'''

%% @since Version 571

byte_to_hex(TheByte) when is_integer(TheByte), TheByte >= 0, TheByte =< 255 ->

    [ nybble_to_hex(TheByte bsr 4), nybble_to_hex(TheByte band 15) ].





%% @type hexchar() = integer().  Integer must be in the range $0 - $9, the range $a - $f, or the range $A - $F, all inclusive, for inputs; outputs will always use lower case.
%% @type hexstring() = list().  All elements of the list must be of type {@type hexchar()}.

%% @spec hex_to_int(HexChar::hexstring() | hexchar()) -> integer()
%% @doc <span style="color:orange;font-style:italic">Untested</span> Convert a hexstring() or hexchar() into its numeric value. ```1> scutil:hex_to_int("c0ffEE").
%% 12648430
%%
%% 2> scutil:hex_to_int($e).
%% 14
%%
%% 3> scutil:hex_to_int("100").
%% 256'''

%% @since Version 572

hex_to_int(Hex) when is_integer(Hex), Hex >= $0, Hex =< $9 -> Hex - $0;
hex_to_int(Hex) when is_integer(Hex), Hex >= $a, Hex =< $f -> Hex - $a + 10;
hex_to_int(Hex) when is_integer(Hex), Hex >= $A, Hex =< $F -> Hex - $A + 10;

hex_to_int(Hex) when is_list(Hex) ->
    hex_to_int(Hex, 0).

hex_to_int([],          Acc) -> Acc;
hex_to_int([Digit|Rem], Acc) -> hex_to_int(Rem, (Acc bsl 4) + hex_to_int(Digit)).





%% @spec list_to_number(X::list()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Converts a list into a number; integers will be returned if there is no mantissa in the list representation. ```1> scutil:list_to_number("2").
%% 2
%%
%% 2> scutil:list_to_number("2.0").
%% 2.0
%%
%% 3> scutil:list_to_number("2.1").
%% 2.1'''

%% @since Version 574

list_to_number(X) ->

    case catch list_to_float(X) of

        {'EXIT',_} ->
            list_to_integer(X);

        Y ->
            Y

    end.





%% @todo more distance types - manhattan, malahabanois, etc

%% @spec euclidean_distance(Coordinate1::coord(), Coordinate2::coord()) -> number()

%% @doc <span style="color:orange;font-style:italic">Untested</span> Returns the distance between two coordinates in any N-space.  In two dimensions, this is known as the Pythagorean theorem.  The coordinates may be of any positive integer dimensionality (2d, 3d, but no -1d or 2.5d), but both coordinates must be of the same dimensionality.  The coordinates may have real-valued or negative components, but imaginary math is not implemented.  This function tolerates tuple coordinates by converting them to lists; list coordinates are thus slightly faster. ```1> sc:distance([0,0],[1,1]).
%% 1.4142135623730951
%%
%% 2> sc:distance({0,0},[-1,1.0]).
%% 1.4142135623730951
%%
%% 3> sc:distance([0,0,0,0],[1,-1,1,-1]).
%% 2.0'''

%% @since Version 575

euclidean_distance(C1, C2) when is_tuple(C1) ->

    euclidean_distance( tuple_to_list(C1), C2 );



euclidean_distance(C1, C2) when is_tuple(C2) ->

    euclidean_distance( C1, tuple_to_list(C2) );



euclidean_distance(C1, C2) ->

    % squaring makes taking the absolute value to get unsigned magnitude redundant; that's not an omission, it's an optimization
    math:sqrt(
        lists:sum(
            [ sc_math:square(A-B) ||
                {A,B} <- sc_lists:zip_n([C1,C2])
            ]
        )
    ).





%% @todo docs

% % @ s ince Version 576

% merge_settings(S1, S2) when

%     is_list(S1),
%     is_list(S2) ->





%% @spec send_receive(ToWhom::pid()|atom(), What::any()) -> { item, any() }

%% @doc <span style="color:orange;font-style:italic">Untested</span> (Blocking) First send a message to an entity.  Then pop the front of the message queue and return it as `{item,X}'; block.  ```1> scutil:send_receive(self(), message).
%% {item,message}'''

%% @since Version 578

send_receive(ToWhom, What) ->

    ToWhom ! What,

    receive X ->
        { item, X }
    end.





%% @spec send_receive(ToWhom::pid()|atom(), What::any(), HowLong::non_negative_integer()|infinity) -> { item, any() } | nothing_there

%% @doc <span style="color:orange;font-style:italic">Untested</span> (Non-Blocking) First send a message to an entity.  Then pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> scutil:send_receive(self(), message).
%% {item,message}'''

%% @since Version 579

send_receive(ToWhom, What, HowLong) ->

    ToWhom ! What,

    receive X ->
        { item, X }
    after HowLong ->
        nothing_there
    end.





%% @spec send_receive_masked(Mask::any(), ToWhom::pid()|atom(), What::any()) -> { Mask, any() }

%% @doc <span style="color:orange;font-style:italic">Untested</span> (Blocking) First send a message to an entity.  Then pop the first message queue item matching the mask as a 2-tuple, and return it as `{Mask,X}'; block.  ```1> scutil:send_receive(self(), message).
%% {item,message}'''

%% @since Version 580

send_receive_masked(Mask, ToWhom, What) ->

    ToWhom ! What,

    receive { Mask, X } ->
        { Mask, X }
    end.





%% @spec send_receive_masked(Mask::any(), ToWhom::pid()|atom(), What::any(), HowLong::non_negative_integer()|infinity) -> { item, any() } | nothing_there

%% @doc <span style="color:orange;font-style:italic">Untested</span> (Non-Blocking) First send a message to an entity.  Then pop the front of the message queue and return it as `{Mask,X}', or return nothing_there for empty queues; do not block.  ```1> scutil:send_receive(self(), message).
%% {item,message}'''

%% @since Version 581

send_receive_masked(Mask, ToWhom, What, HowLong) ->

    ToWhom ! What,

    receive { Mask, X } ->
        { Mask, X }
    after HowLong ->
        nothing_there
    end.





%% @spec receive_one() -> { item, any() } | nothing_there

%% @doc <span style="color:orange;font-style:italic">Untested</span> Pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> scutil:receive_one().
%% nothing_there
%%
%% 2> self() ! message.
%% message
%%
%% 3> scutil:receive_one().
%% {item,message}
%%
%% 4> scutil:receive_one().
%% nothing_there'''

%% @since Version 582

receive_one() ->

    receive (X) ->
        { item, X }
    after 0 ->
        nothing_there
    end.





%% @since Version 586

%% @doc <span style="color:orange;font-style:italic">Untested</span>

union(L) ->

    lists:usort(lists:append(L)).





%% @since Version 586

%% @equiv union( [L1, L2] )

%% @doc <span style="color:orange;font-style:italic">Untested</span>

union(L1, L2) ->

    union([L1,L2]).





%% @since Version 586

%% @equiv union( [L1, L2, L3] )

%% @doc <span style="color:orange;font-style:italic">Untested</span>

union(L1, L2, L3) ->

    union([L1,L2,L3]).





%% @since Version 586

%% @equiv union( [L1, L2, L3, L4] )

%% @doc <span style="color:orange;font-style:italic">Untested</span>

union(L1, L2, L3, L4) ->

    union([L1,L2,L3,L4]).
