
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





%% @doc This is the 2011 revamp of <a href="http://scutil.com/" target="_blank">scutil</a>'s erlang library.
%%
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
    index_of_first/2,
    count_x/2,
    zip_n/1,
    expand_labels/1,
    combinations/2,
    flag_sets/1,
    list_product/1,
    sanitize_tokens/2,
    naive_bayes_likelihood/4,
    caspers_jones_estimate/1,
    range_scale/1,
    even_or_odd/1,

    instant_runoff_vote/1,

    dstat/2,

    median/1,

    moment/2,
      moments/1,
      moments/2,

    central_moment/2,
      central_moments/1,
      central_moments/2,

    arithmetic_mean/1,
      geometric_mean/1,
      harmonic_mean/1,
      weighted_arithmetic_mean/1,

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
%% @doc Runs the test suite in verbose form.  Also responds to [verbose] to be more familiar to eunit devs. ```1> sc:test(verbose).
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

%% @doc Computes a zip on any sized group of lists, rather than just two or three as offered by the lists module.```1> sc:zip_n([ [1,2,3], [a,b,c], [i,ii,iii] ]).
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

%% @doc Remove unacceptable elements from an input list, as defined by another list or a filter function.  Common reasons for sanitization include reducing arbitrary or bulk data to key format (such as using an original filename and new size to generate a new filename or database key) and removing malformed items from a list before processing. ```1> scutil:sanitize_tokens("ae0z4nb'wc-04bn ze0e 0;4ci ;e0o5rn;", "ace").
%% "aeceece"
%%
%% 2> Classifier = fun(apple) -> true; (banana) -> true; (cherry) -> true; (date) -> true; (elderberry) -> true; (_) -> false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 3> scutil:sanitize_tokens([apple, boat, cherry, dog, elderberry], Classifier).
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

%% @doc Calculates digital line bandwidth over timescales in converted units. ```1>'''
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

caspers_jones_estimate(FunctionPoints) when

    is_integer(FunctionPoints),
    FunctionPoints >= 0 ->


    [   { estimated_defects,   math:pow(FunctionPoints, 1.25) },
        { unit_test_baseline,  math:pow(FunctionPoints, 1.2)  },
        { months_to_implement, math:pow(FunctionPoints, 0.4)  },
        { staff_to_implement,  FunctionPoints/150             }
    ].





%% @spec naive_bayes_likelihood(FeatureEvident::non_negative_integer(), FeatureTotal::positive_integer(), NonFeatureEvident::non_negative_integer(), NonFeatureTotal::positive_integer()) -> Result::list()

%% @doc Calculates the contributing difference probability, feature likelihood and non-feature likelihood of an event
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

%% @doc Get the scale of a same-sign numeric range.  Gives nonsense results for non-numeric lists, or for lists which have both positive and negative members.  For a numeric list [4,5,6,12], the scale of the range 4..12 is 3:1, which is represented as 3.0 . ```1> sc:range_scale([3, 4, 5, 6]).
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

%% @doc Estimates the zipf baseline from a score and a rank position. ```1> sc:zipf_position_estimate(120, 3).
%% 360'''

%% @since Version 480

zipf_position_estimate(Score, Rank) ->

    Score * Rank.





%% @spec zipf_estimate_list(PosNumericList::positive_numeric_list()) -> positive_numeric_list()

%% @doc Estimates the zipf baseline from each number in a numeric list. ```1> sc:zipf_estimate_list([ 120, 60, 40, 30, 24, 20 ]).
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

%% @doc todo ```1> sc:zipf_nearness([ 120, 60, 40, 30, 24, 20 ]).
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

%% @doc Take the arithmetic mean (often called the average) of a list of numbers. ```1> sc:arithmetic_mean([1,2,3,4,5]).
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

%% @doc Take the geometric mean of a list of numbers. ```1> scutil:geometric_mean([1,2,3,4,5]).
%% 2.6051710846973517''' The naive approach ```geometric_mean(List) -> math:pow(scutil:list_product(List), 1/length(List)).''' is not used because it accumulates error very quickly, and is as such unsuited to huge lists.

%% @see arithmetic_mean/1
%% @see harmonic_mean/1
%% @see gmean_vector_normal/1

%% @since Version 482

geometric_mean([]) ->

    0.0;



geometric_mean(List) when is_list(List) ->

    math:exp(scutil:arithmetic_mean([math:log(X)||X<-List])).





%% @spec harmonic_mean(InputList::numericlist()) -> float()

%% @doc Take the harmonic mean of a list of numbers. ```1> scutil:harmonic_mean([1,2,3,4,5]).
%% 2.18978102189781'''

%% @see arithmetic_mean/1
%% @see geometric_mean/1
%% @see hmean_vector_normal/1

%% @since Version 483

harmonic_mean([]) ->

    0.0;



harmonic_mean(List) when is_list(List) ->

    length(List) / lists:sum([ 1/X || X<-List ]).





%% @spec weighted_arithmetic_mean(InputList::weightlist()) -> float()

%% @doc Take the weighted arithmetic mean of the input values. ```1> scutil:weighted_arithmetic_mean([ {8,1}, {3,4}, {16,1} ]).
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

%% @doc Performs an instant runoff vote.  http://en.wikipedia.org/wiki/Instant-runoff_voting ```1>'''

%% @since Version 485

instant_runoff_vote(ListOfVoteLists) ->

    instant_runoff_vote(ListOfVoteLists, []).



instant_runoff_vote(ListOfVoteLists, Exclude) ->

    FilteredVotes = [ VoteList || VoteList <- ListOfVoteLists, VoteList =/= [] ],
    FilteredVotes.





% inspired by J dstat from some blog, http://bbot.org/blog/archives/2011/03/17/on_being_surprised_by_a_programming_language/
% herp derp

% Hilariously, J seems to be assuming pop or sample.

%% @since Version 487

dstat(NumericList, PopulationOrSample) ->

    { Min, Max } = extrema(NumericList),

    {   { sample_size,        length(NumericList)                                 },
        { minimum,            Min                                                 },
        { maximum,            Max                                                 },
        { median,             median(NumericList)                                 },
        { mean,               arithmetic_mean(NumericList)                        },
        { standard_deviation, standard_deviation(NumericList, PopulationOrSample) },
        { skewness,           skewness(NumericList)                               },
        { kurtosis,           kurtosis(NumericList)                               }
    }.




%% @spec median(List::numericlist()) -> number()

%% @doc Takes the median (central) value of a list.  Sorts the input list, then finds and returns the middle value.  ```1> sc:median([1,2,999]).
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

%% @doc Measures the standard deviation of the values in the list.  ```1> sc:standard_deviation([1,2,3,4,5],population).
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

%% @doc Takes the Nth moment of a list.  The Nth moment of a list is the arithmetic mean of the list items, each taken to the Nth power.  Fractional Ns are well defined
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

    scutil:arithmetic_mean( [ math:pow(Item, N) || Item <- List ] ).



%% @equiv [ moment(List, N) || N <- [2,3,4] ]

%% @since Version 491

moments(List) ->

    moments(List, [2,3,4]).



%% @equiv [ moment(List, N) || N <- Moments ]

%% @since Version 491

moments(List, Moments) when is_list(Moments) ->

    [ moment(List, M) || M <- Moments ].





% thanks to Chile and Kraln for straightening me out on moments and central moments

%% @spec central_moment(List::list(), N::integer()) -> float()

%% @doc Takes the Nth cetral moment of a list.  The Nth central moment of a list is the arithmetic mean of (the list items each minus the mean of the list, each
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

    ListAMean = scutil:arithmetic_mean(List),
    scutil:arithmetic_mean( [ math:pow(Item-ListAMean, N) || Item <- List ] ).



%% @equiv [ central_moment(List, N) || N <- [2,3,4] ]

%% @since Version 492

central_moments(List) ->

    central_moments(List, [2,3,4]).



%% @equiv [ central_moment(List, N) || N <- Moments ]

%% @since Version 492

central_moments(List, Moments) when is_list(Moments) ->

    [ central_moment(List, M) || M <- Moments ].
