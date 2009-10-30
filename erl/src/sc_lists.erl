
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





-module(sc_lists).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("List manipulation and handling routines.").

-testerl_export( { [], sc_list_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    extrema/1,
    key_duplicate/1,
    list_rotate/2,
    index_of_first/2,
    rotate_to_first/2,
    rotate_to_last/2,
    minmax/1,
    every_flag_representation/1,
    every_member_representation/1,
    count_of/2,

    list_intersection/2,
      list_intersection/3,

    zip_n/1,
      zip_n/2,

    combinations/2,

    expand_label/1,
    expand_labels/1,

    permute/1,
      permute/2,

    shared_keys/1,
      shared_keys/2,
      shared_keys/3,

    all_unique_pairings/1,
    walk_unique_pairings/2,
    list_product/1,
    sanitize_tokens/2,

    elements/2,
      elements/3,
      elements/4,

    reverse_map/2,
    reverse_filter/2,
    reverse_map_filter/3,
    postfix/2,

    keygroup/2,
      keygroup/3,

    split_at/2,

    first_pos/2,
      first_pos/3,

    last_while_pos/2,
      last_while_pos/3,

    partition_n/2,

    all_neighbor_pairs/1,
    distinct_neighbor_pairs/1

] ).




%% @since Version 221

% was `scutil:extrema_of/1'
extrema(List) ->

    [First | _] = List,

    lists:foldl(

        fun(Next,T) ->

            io:format("~w ~w~n", [T,Next]),
            {Hi, Lo} = T,

            Lo2 = if
                Next < Lo ->
                    Next;
                true ->
                    Lo
            end,

            Hi2 = if
                Next > Hi ->
                    Next;
                true ->
                    Hi
            end,

            {Hi2, Lo2}

        end,

        {First,First},
        List
    ).





%% @since Version 200

key_duplicate(KeyList) ->

    lists:flatten( [ lists:duplicate(Key, Value) || {Key,Value} <- KeyList ] ).





%% @since Version 168

list_rotate(0, List) ->

    List;



list_rotate(By, List) when By =< (-(length(List))) ->

    list_rotate(By rem length(List), List);



list_rotate(By, List) when By < 0 ->

    list_rotate(length(List) + By, List);



list_rotate(By, List) when By >= length(List) ->

    list_rotate(By rem length(List), List);



list_rotate(By, List) ->

    { Front, Rear } = lists:split(By, List),
    Rear ++ Front.





%% @since Version 169

index_of_first(Item, List) ->

    index_of_first(Item, List, 1).



index_of_first(_Item, [], _Pos) ->

    undefined;



index_of_first(Item, [Item|_ListRem], Pos) ->

    Pos;



index_of_first(Item, [_OtherItem|ListRem], Pos) ->

    index_of_first(Item, ListRem, Pos+1).





%% @since Version 170

rotate_to_first(Item, List) ->

    list_rotate(index_of_first(Item, List)-1, List).





%% @since Version 170

rotate_to_last(Item, List) ->

    list_rotate(index_of_first(Item, List), List).





%% @since Version 129
%% @todo is this the same as extrma?

minmax( [FirstItem | RestOfList] ) ->

    minmax(RestOfList, FirstItem, FirstItem).



% can never be both smaller than min and larger than max, so this is safe
minmax( [], Min, Max) ->

    {Min,Max};



minmax( [ThisItem|RestOfList], Min, Max) when ThisItem < Min ->

    minmax(RestOfList, ThisItem, Max);



minmax( [ThisItem|RestOfList], Min, Max) when ThisItem > Max ->

    minmax(RestOfList, Min, ThisItem);


minmax( [_ThsItem|RestOfList], Min, Max) ->

    minmax(RestOfList, Min,      Max).





%% @spec every_flag_representation(Flags::list()) -> list_of_lists()

%% @doc {@section Lists} Returns every interpretation of the list as a set of boolean flags, including all-off and all-on. ```1> scutil:every_flag_representation([1,2,3,4]).
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

%% @since Version 126

every_flag_representation([]) ->
    
    [[]];




every_flag_representation([Flag|RemFlags]) ->
    
    [ MaybeFlag ++ Reps ||
        MaybeFlag <- [[],[Flag]],
        Reps      <- every_flag_representation(RemFlags)
    ].





%% @equiv every_member_representation(Memberships, no_absence)

every_member_representation(Memberships) -> 

    every_member_representation(Memberships, no_absence).



%% @spec every_member_representation(Memberships::list_of_lists(), AllowAbsence::atom()) -> list_of_lists()

% % @doc {@section Lists} For a list of memberships, return every possible combination of one representative member from each list.  The parameter `AllowAbsence' controls whether memberships may be unrepresented; if unrepresented memberships are possible, then one possible representation becomes the empty list. ```1> scutil:every_member_representation([ [a,b],[1,2,3],[i,ii,iii] ], no_absence).
% % [[a,1,i], [a,1,ii], [a,1,iii], [a,2,i], [a,2,ii], [a,2,iii], [a,3,i], [a,3,ii], [a,3,iii], [b,1,i], [b,1,ii], [b,1,iii], [b,2,i], [b,2,ii], [b,2,iii], [b,3,i], [b,3,ii], [b,3,iii]]
% %
% % 2> scutil:every_member_representation([ [a,b],[1,2],[i,ii] ], allow_absence).
% % [ [], [i], [ii], [1], [1,i], [1,ii], [2], [2,i], [2,ii], [a], [a,i], [a,ii], [a,1], [a,1,i], [a,1,ii], [a,2], [a,2,i], [a,2,ii], [b], [b,i], [b,ii], [b,1], [b,1,i], [b,1,ii], [b,2], [b,2,i], [b,2,ii] ]'''
% %
% % 3> Format = fun(Person, Place, Weapon) -> "It was " ++ Person ++ " in the " ++ Place ++ " with the " ++ Weapon ++ "!" end.
% % #Fun<erl_eval.18.105910772>
% %
% % 4> { People, Places, Weapons } = { ["Col. Mustard", "Mr. Green"], ["the billiards room", "the kitchen"], ["a lead pipe", "a knife", "a gun"] }.
% % {["Col. Mustard","Mr. Green"],
% %  ["the billiards room","the kitchen"],
% %  ["a lead pipe","a knife","a gun"]}
% %
% % 5> Places.
% % ["the billiards room","the kitchen"]
% %
% % 6> Format("Mrs. Scarlett", "the observatory", "a noose").
% % "It was Mrs. Scarlett in the the observatory with the a noose!"
% %
% % 7> EveryClueOutcome = [ Format(ThisPerson, ThisPlace, ThisWeapon) || ThisPerson <- People, ThisPlace <- Places, ThisWeapon <- Weapons ].
% % ["It was Col. Mustard in the the billiards room with the a lead pipe!",
% %  "It was Col. Mustard in the the billiards room with the a knife!",
% %  "It was Col. Mustard in the the billiards room with the a gun!",
% %  "It was Col. Mustard in the the kitchen with the a lead pipe!",
% %  "It was Col. Mustard in the the kitchen with the a knife!",
% %  "It was Col. Mustard in the the kitchen with the a gun!",
% %  "It was Mr. Green in the the billiards room with the a lead pipe!",
% %  "It was Mr. Green in the the billiards room with the a knife!",
% %  "It was Mr. Green in the the billiards room with the a gun!",
% %  "It was Mr. Green in the the kitchen with the a lead pipe!",
% %  "It was Mr. Green in the the kitchen with the a knife!",
% %  "It was Mr. Green in the the kitchen with the a gun!"]
% %
%% @since Version 126

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





%% @type non_negative_integer() = integer().  A {@type non_negative_integer()} must be equal to or greater than zero.

%% @spec count_of(Item::any(), List::list()) -> non_negative_integer()

%% @doc Counts the number of instances of Item in List.  ```1> TestData = lists:duplicate(40,[healthy,nonsmoker]) ++ lists:duplicate(10,[healthy,smoker]) ++ lists:duplicate(7,[cancer,nonsmoker]) ++ lists:duplicate(3,[cancer,smoker]).
%% [[healthy,nonsmoker], [healthy,nonsmoker], [healthy|...], [...]|...]
%%
%% 2> scutil:count_of([healthy,smoker], TestData).
%% 10
%%
%% 3> scutil:count_of([healthy,nonsmoker], TestData).
%% 40'''

%% @since Version 117

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





%% @equiv list_intersection(List1, List2, unsorted)

list_intersection(List1, List2) ->

    list_intersection(List1, List2, unsorted).



%% @spec list_intersection(List1::list(), List2::list(), IsSorted::atom()) -> list()

%% @doc Efficiently computes the intersection of two lists.  The third parameter, which is optional and defaults to `unsorted', is either the atom `sorted' or `unsorted'.  If `sorted' is used, the function will sort both inputs before proceeding, as it requires sorted lists; as such, if you already know your lists to be sorted, passing `unsorted' will save some time.  The return list will be reverse sorted. ```1> scutil:list_intersection([1,2,3,4,5,2,3,10,15,25,30,40,45,55],[1,3,5,5,5,15,20,30,35,40,50,55]).
%% [55,40,30,15,5,3,1]
%%
%% 2> scutil:list_intersection([1],[2]).
%% []''' {@section Thanks} to Ayrnieu for catching a defect in the initial implementation.

%% @since Version 120

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





%% @spec zip_n(Ls::list()) -> list_of_tuples()
%% @equiv zip_n(Ls, to_tuple)

zip_n(Ls) ->

    zip_n(Ls, to_tuple).



%% @spec zip_n(Ls::list(), ResultType::atom()) -> list_of_tuples()

%% @doc {@section List} Computes a zip on any sized group of lists, rather than just two or three as offered by the lists module.
%%
%% This is actually more efficient than one might expect at first glance.  I ran a benchmark of 100,000 transformations of a list of lists into a list of tuples using {@link benchmark/3} and {@link multi_do/4} against both zip_n and the library function zip3; the library function won at 150 seconds to 175, which is a far smaller difference than I expected.```1> Testy = [ [1,2,3], [1,2,3], [1,2,3] ].
%% [[1,2,3],[1,2,3],[1,2,3]]
%%
%% 2> scutil:benchmark(scutil, multi_do, [100000, scutil, zip_n, [Testy]]).
%% {174.95563, [[{1,1,1},{2,2,2},{3,3,3}], [{1,1,1},{2,2,2},{3,3,3}], ... }
%%
%% 3> scutil:benchmark(scutil, multi_do, [100000, lists, zip3, Testy]).
%% {149.605, [[{1,1,1},{2,2,2},{3,3,3}], [{1,1,1},{2,2,2},{3,3,3}], ... }'''
%%
%% {@section Thanks} Thanks to Vladimir Sessikov for contributing this to and thus allowing conscription from <a href="http://www.erlang.org/ml-archive/erlang-questions/200207/msg00066.html">the mailing list</a>.

%% @since Version 108

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

%% @spec combinations(Items::list(), OutputItemSize::positive_integer()) -> list_of_lists()

%% @doc {@section List} Provides a list of every unique combination of input terms, order-ignorant; contrast {@link permute/2}.  Permutations are all unique combinations of a set of tokens; the 2-permutations of `[a,b,c]' for example are `[a,b]', `[a,c]' and `[b,c]'.  Note the absence of other orderings, such as `[b,a]', which are provided by {@link permute/2}.  Combinations are taken of a smaller count of tokens than the main set.  Combinations are not ordered, but this implementation happens to provide answers in the same order as the input list.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> scutil:combinations([a,b,c,d],2).
%% [[a,b],[a,c],[a,d],[b,c],[b,d],[c,d]]
%%
%% 2> scutil:combinations(["dave","kate","pat"],2).
%% [["dave","kate"],["dave","pat"],["kate","pat"]]
%%
%% 3> scutil:combinations([fast, strong, smart, lucky], 2).
%% [[fast,strong], [fast,smart], [fast,lucky], [strong,smart], [strong,lucky], [smart,lucky]]''' {@section Thanks} to Alisdair Sullivan for this implementation, which has been slightly but not significantly modified since receipt.

%% @since Version 89

combinations(Items, 1) when is_list(Items) -> 

    Items;



combinations([], _N) ->

   [];



combinations(Items, N) when is_list(Items), is_integer(N), N > 0 ->

    [ lists:flatten(lists:append( [lists:nth(I, Items)], [J] )) ||
      I <- lists:seq(1, length(Items)),
      J <- combinations( lists:nthtail(I, Items), (N-1) )
    ].





%% @since Version 51

expand_label({Label,List}) when is_list(List) ->

     [ {Label,L} || L<-List ];



expand_label({Label,Item}) ->

    {Label, Item}.



expand_labels(List) when is_list(List) ->

    lists:flatten( [ expand_label(X) || X <- List ] ).





%% @equiv permute(List, length(List))

permute(List) -> 

    permute(List, length(List)).
    


%% @type positive_integer() = integer().  Positive integer must be greater than zero.

%% @spec permute(List::list(), Depth::positive_integer()) -> list()

%% @doc {@section Utility} Calculate either the full or the depth-limited permutations of a list, order sensitive; contrast {@link combinations/2}.  Permutations are all valid orderings of a set of tokens; the permutations of `[a,b]' for example are `[a,b]' and `[b,a]'.  Depth limitation means the permutations of a smaller count of tokens from the main set; the 2-limited permutations of `[a,b,c]' for example are `[a,b]', `[a,c]', `[b,a]', `[b,c]', `[c,a]' and `[c,b]'.  Permutations are not ordered.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> scutil:permute(["dave","kate","pat"]).
%% [{"pat","kate","dave"}, {"kate","pat","dave"}, {"pat","dave","kate"}, {"dave","pat","kate"}, {"kate","dave","pat"}, {"dave","kate","pat"}]
%%
%% 2> scutil:permute([fast, strong, smart, lucky], 2).
%% [{strong,fast}, {smart,fast}, {lucky,fast}, {fast,strong}, {smart,strong}, {lucky,strong}, {fast,smart}, {strong,smart}, {lucky,smart}, {fast,lucky}, {strong,lucky}, {smart,lucky}]'''

%% @since Version 17

permute(List, 1) when is_list(List) -> 

    [ [T] || 
        T <- List 
    ];



permute(List, Depth) when is_list(List), is_integer(Depth) ->

    [ [T]++R ||
        T <- List,
        R <- permute(List--[T], Depth-1)
    ].





% Create sorted list X of 3-ary tuples {K,Ai,Bi} from sorted lists A, B of 2ary {K,Ai}/{K,Bi} tuples, where key K appears in both A and B

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





% collects results; do not use for huge lists

%% @deprecated Use {@link combinations/2} instead.

%% @spec all_unique_pairings(List::list()) -> tuple_list()

%% @doc {@section List} Generate every unique pair of elements from a list; deprecated in favor of {@link combinations/2}.  ```1> scutil:all_unique_pairings([a,b,c]).
%% [{b,c},{a,b},{a,c}]'''

%% @since Version 31

all_unique_pairings(A) when is_list(A) -> 

    all_unique_pairings(A,[]).
    


all_unique_pairings([], Work) -> 

    Work;
    
    

all_unique_pairings([Ai|Ar], Work) -> 

     all_unique_pairings(Ar, [{Ai,Ari}||Ari<-Ar] ++ Work).





% used for side effects, doesn't gather results; appropriate for enormous lists

% comeback

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





%% @spec list_product(A::numericlist()) -> number()

%% @doc {@section Math} Takes the product of all numbers in the list.  Offered mostly to make dependant code clearer. ```1> scutil:list_product([1,2,5.4]).
%% 10.8'''

%% @since Version 39

list_product(List) when is_list(List) ->

    list_product(List, 1).



list_product([], Counter) ->

    Counter;



list_product([Head|Tail], Counter) ->

    list_product(Tail, Counter*Head).





%% @type filterfunction() = function().  Filter functions are 1ary binary predicates - they accept an argument and return either true or false.
%% @type sanitizer() = list() | filterfunction().  Sanitizers are used by {@link sanitize_tokens/2} for input sanitization; they define what parts of an input list are valid, and the remainder are removed.  Sanitizers may either be a list of acceptable elements or a filter function.

%% @spec sanitize_tokens(InputList::list(), Allowed::sanitizer()) -> list()

%% @doc {@section List} Remove unacceptable elements from an input list, as defined by another list or a filter function.  Common reasons for sanitization include reducing arbitrary or bulk data to key format (such as using an original filename and new size to generate a new filename or database key) and removing malformed items from a list before processing. ```1> scutil:sanitize_tokens("ae0z4nb'wc-04bn ze0e 0;4ci ;e0o5rn;", "ace").
%% "aeceece"
%%
%% 2> Classifier = fun(apple) -> true; (banana) -> true; (cherry) -> true; (date) -> true; (elderberry) -> true; (_) -> false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 3> scutil:sanitize_tokens([apple, boat, cherry, dog, elderberry], Classifier).
%% [apple,cherry,elderberry]'''

%% @see sanitize_filename/1

%% @since Version 31

sanitize_tokens(List, Allowed) when is_list(List), is_function(Allowed) ->

    lists:filter(Allowed, List);



sanitize_tokens(List, Allowed) when is_list(List), is_list(Allowed) ->

    lists:filter(fun(X) -> lists:member(X,Allowed) end, List).





%% @todo TODO

% key_split(KeyId, TupleList)           when is_list(TupleList) -> key_split(KeyId, TupleList,                       unsorted).
% key_split(KeyId, TupleList, unsorted) when is_list(TupleList) -> key_split(KeyId, lists:keysort(KeyId, TupleList), sorted);
% key_split(KeyId, TupleList, sorted)   when is_list(TupleList) ->

% key_minimum(
% key_maximum(




% todo invert this so that it returns {currentcount, fun, result} so that it can be continued
% generate(0, _) -> [];
% generate(N, Fun) when is_integer(N) andalso N > 0 andalso is_function(Fun) -> [Fun()] ++ generate(N-1,Fun).





% todo implement catching tuple { key, reqtype } from list, to auto-convert before return
% todo There may be a crashing bug here for repeated attributes, which are apparently legal, see http://fullof.bs/reading-module-attributes-in-erlang#comment-466
% todo It may help to re-implement this using proplists instead of doing it manually, profile
%% @todo document this

% interface

elements(Config, Requested)                when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1).
elements(Config, Requested, KeyIdx)        when is_list(Config), is_list(Requested), is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx);

elements(Config, Requested, strip)         when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1,      strip).
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





%% @since Version 334

reverse_map(Workload, Fun) ->

    reverse_map(Workload, [], Fun).



%% @since Version 334

reverse_map([], Work, _Fun) ->

    Work;



%% @since Version 334

reverse_map([Item|Rem], Work, Fun) ->

    reverse_map(Rem, [Fun(Item)]++Work, Fun).





%% @since Version 334

reverse_filter(Workload, Fun) ->

    reverse_filter(Workload, [], Fun).



%% @since Version 334

reverse_filter([], Work, _Fun) ->

    Work;



%% @since Version 334

reverse_filter([Item|Rem], Work, Fun) ->

    case Fun(Item) of
        true  -> reverse_filter(Rem, [Item]++Work, Fun);
        false -> reverse_filter(Rem, Work,         Fun)
    end.





%% @since Version 334

reverse_map_filter(Workload, MapFun, FilterFun) ->

    reverse_map_filter(Workload, [], MapFun, FilterFun).



%% @since Version 334

reverse_map_filter([], Work, _MapFun, _FilterFun) ->

    Work;



%% @since Version 334

reverse_map_filter([Item|Rem], Work, MapFun, FilterFun) ->

    Res = MapFun(Item),

    case FilterFun(Res) of
        true  -> reverse_map_filter(Rem, [Res]++Work, MapFun, FilterFun);
        false -> reverse_map_filter(Rem, Work,        MapFun, FilterFun)
    end.





%% @since Version 334

postfix(Postfix, String) ->

    lists:prefix(lists:reverse(Postfix), lists:reverse(String)).





%% @since Version 345

keygroup(Pos, List) ->

    keygroup(Pos, List, unsorted).





%% @since Version 345

keygroup(Pos, List, unsorted) when is_list(List) ->

    SList = lists:keysort(Pos,List),
    [F|_] = SList,

    keygroup(Pos, SList, element(Pos,F), [], []);





%% @since Version 345

keygroup(Pos, List, sorted) when is_list(List) ->

    [F|_] = List,

    keygroup(Pos, List, element(Pos,F), [], []).





%% @since Version 345

keygroup(_Pos, [], WorkKey, Work, Output) ->

    [{WorkKey, Work}] ++ Output;





%% @since Version 345

keygroup(Pos, [Item|Rem], WorkKey, Work, Output) ->

    NewKey = element(Pos, Item),

    case NewKey == WorkKey of

        true  ->
            keygroup(Pos, Rem, WorkKey, [Item]++Work, Output);

        false ->
            keygroup(Pos, Rem, NewKey,  [Item],       [{WorkKey,Work}]++Output)

    end.





%% @since Version 346 TODO

split_at(N, List) ->

    split_at(N, N, List, [], []).





split_at(_N, _BlockN, [], Current, Work) ->

    lists:reverse([lists:reverse(Current)] ++ Work);





split_at(N, 0, Workload, Current, Work) ->

    split_at(N, N, Workload, [], [lists:reverse(Current)] ++ Work);





split_at(N, BN, [Item|Rem], Current, Work) ->

    split_at(N, BN-1, Rem, [Item]++Current, Work).





%% @Version Since 381
%
% 1> sc_lists:first_pos([a,b,c,d,2,f],fun erlang:is_integer/1).
% 5
% 
% 2> sc_lists:first_pos([a,b,c,d,e,f],fun erlang:is_integer/1).
% false

first_pos(List, Predicate) ->

    first_pos(1, List, Predicate, false).



first_pos(List, Predicate, Default) ->

    first_pos(1, List, Predicate, Default).



first_pos(_N, [],  _Pred, Default) ->

    Default;



first_pos(N, [Head|Tail], Pred, Default) ->

    case Pred(Head) of
        true  -> N;
        false -> first_pos(N+1, Tail, Pred, Default)
    end.





%% @Version Since 381
%
% 1> sc_lists:last_while_pos([a,b,c,d,2,f],fun erlang:is_atom/1).
% 4
%
% 2> sc_lists:last_while_pos([a,b,c,d,r,f],fun erlang:is_atom/1).
% 6
%
% 3> sc_lists:last_while_pos([1,a,b,c,d,r,f],fun erlang:is_atom/1).
% false

last_while_pos(List, Predicate) ->

    last_while_pos(1, List, Predicate, false).



last_while_pos(List, Predicate, Default) ->

    last_while_pos(1, List, Predicate, Default).



last_while_pos(_N, [],  _Pred, Last) ->

    Last;



last_while_pos(N, [Head|Tail], Pred, Last) ->

    case Pred(Head) of
        true  -> last_while_pos(N+1, Tail, Pred, N);
        false -> Last
    end.





% @Since Version 388

partition_n(Data, Function) ->

    [ { GroupId, [ GDatum || { _H, GDatum } <- GroupData ] } || { GroupId, GroupData } <- keygroup( 1, [ { Function(Datum), Datum } || Datum <- Data ] ) ].





% @since Version 417

all_neighbor_pairs(List) ->

    all_neighbor_pairs(List, []).



all_neighbor_pairs([[]], Work) ->

    lists:reverse(Work);



all_neighbor_pairs([A,B|Rem], Work) ->

    all_neighbor_pairs(Rem, [{A,B}] ++ Work).





% @since Version 418

distinct_neighbor_pairs(List) ->

    distinct_neighbor_pairs(List, []).



distinct_neighbor_pairs([], Work) ->

    [];



distinct_neighbor_pairs([[_LastItemIsNotInAPairByItself]], Work) ->

    lists:reverse(Work);



distinct_neighbor_pairs([[]], Work) ->

    lists:reverse(Work);



distinct_neighbor_pairs([A,B|Rem], Work) ->

    distinct_neighbor_pairs(Rem, [{A,B}] ++ Work).
