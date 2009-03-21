
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
