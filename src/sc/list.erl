
-module(sc.list).

-export( [
    extrema/1
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





% Like binary_to_term, but not so much for binaries
% thanks dizzyd (modified for error reporting)

%% @since Version 216

list_to_term(List) ->

    case catch erl_scan:string(List) of

        { ok, Tokens, _ } ->

            case erl_parse:parse_term( Tokens ++ [{ dot, 1 }] ) of
                { ok,Term } -> Term;
                Error       -> { error, Error }
            end;

        Error -> { error, Error }

    end.





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

rotate_first_to_end(Item, List) ->

    list_rotate(index_of_first(Item, List), List).





%% @since Version 129
%% @todo is this the same as extrma?

minmax( [FirstItem|RestOfList]) ->

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





%% @spec shuffle(List::list()) -> list()

%% @doc {@section Random} Return a list with the original list's shallow members in a random order.  Deep lists are not shuffled; `[ [a,b,c], [d,e,f], [g,h,i] ]' will never produce sublist reorderings (`[b,c,a]') or list mixing (`[b,g,e]'), only reordering of the three top level lists.  The output list will always be the same length as the input list.  Repeated items and mixed types in input lists are safe. ```1> scutil:shuffle(lists:seq(1,9)).
%% [8,4,7,9,5,2,6,1,3]
%%
%% 2> {TheFaces, TheSuits} = {  [ace] ++ lists:seq(2,10) ++ [jack,queen,king],  [hearts,spades,clubs,diamonds]  }
%% {[ace,jack,queen,king,2,3,4,5,6,7,8,9,10],
%%  [hearts,spades,clubs,diamonds]}
%%
%% 3> Deck = scutil:shuffle([ {Face,Suit} || Face <- TheFaces, Suit <- TheSuits ]).
%% [ {6,spades}, {7,hearts}, {8,clubs}, {queen,spades}, {6,diamonds}, {ace,...}, {...} | ...]
%%
%% 4> scutil:shuffle([ duck,duck,duck,duck, goose ]).
%% [duck,goose,duck,duck,duck]'''
%%
%% <i>Originally found at <a href="http://wiki.trapexit.org/index.php/RandomShuffle">http://wiki.trapexit.org/index.php/RandomShuffle</a>; refactored for clarity, and unnecessary repeat nesting behavior removed.</i>

%% @since Version 8

shuffle(List) ->

   WeightedAndShuffled = lists:map(
       fun(Item) -> { random:uniform(), Item } end,
       List
   ),

   { _, SortedAndDeweighted } = lists:unzip(lists:keysort(1, WeightedAndShuffled)),

   SortedAndDeweighted.





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
