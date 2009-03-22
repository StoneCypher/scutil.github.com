
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <p></p>


%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-03-15 13:47:01 -0600 (Sun, 15 Mar 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





%% @type gridsize() = coord2() | integer().  Coordinates are the width and height of a (1,1) originated grid; as such, coordinates are of the range [1,X] , [1,Y] inclusive, and returned in the form {A,B}.  The integer form implies a square grid.
%% @type coord() = tuple().  Every member of a {@type coord()} is a {@type number()}.  Represents a coordinate, which may imply a sized cartesian space.  Many functions expect integer coordinates; the type does not require them.  This type does not define member count.  If your function requires a specific count of members, name it, as in a {@type coord2()} or {@type coord3()}.
%% @type coordlist() = list().  All members of a {@type coordlist()} must be {@type coord()}s.  All member coordinates must be of the same size, though this type does not define what that size is.  If your function requires a specific count of members, name it, as in a {@type coord2list()} or {@type coord3list()}.
%% @type coord2() = { number(), number() }.  Represents a coordinate, which may imply a sized rectangle.  Many functions expect integer coordinates; the type does not require them.
%% @type coord2list() = list().  All members of a {@type coord2list()} must be {@type coord2()}s.
%% @type coord3() = { number(), number(), number() }.  Represents a coordinate, which may imply a sized 3d box region.  Many functions expect integer coordinates; the type does not require them.
%% @type coord3list() = list().  All members of a {@type coord3list()} must be {@type coord3()}s.

%% @spec grid_scatter(Count::integer(), Size::gridsize()) -> coordlist()

%% @doc {@section Random} Return a Count-length list of non-repeating coordinates in a grid of specified size; useful for feature generation.

%% @todo @comeback give code examples (edoc was failing here?)

%% @since Version 42

grid_scatter(0, []) -> []; % skips a lot of work



grid_scatter(Count, {SizeX, SizeY}) ->

    scutil:random_from(Count, [ {X,Y} || X <- lists:seq(1,SizeX), Y <- lists:seq(1,SizeY) ]);



grid_scatter(Count, Size) ->

    grid_scatter(Count, {Size, Size}).





%% @spec srand() -> { ok, { seeded, Seed } }

%% @doc {@section Random} <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the clock, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want to know what seed was used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> scutil:srand().
%% {ok,{seeded,{1227,902172,685000}}}
%%
%% 2> scutil:srand().
%% {ok,{seeded,{1227,902173,231000}}}'''

%% @since Version 5
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

srand() ->

    {A,B,C} = erlang:now(),
    srand(A,B,C).





%% @spec srand(A::integer(), B::integer(), C::integer()) -> { ok, { seeded, Seed } }
%% @doc {@section Random} <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the three integer seed you provide, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want set what seed is used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> scutil:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}
%%
%% 2> scutil:srand().
%% {ok,{seeded,{1227,902568,604600}}}
%%
%% 3> scutil:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}'''

%% @since Version 5
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

srand(A,B,C) ->

    RandomGeneratorPid = spawn(?MODULE, random_generator, [A,B,C]),

    case whereis(scutil_rand_source) of

        undefined ->
            ok;

        _Defined ->
            unregister(scutil_rand_source)  % todo fixme leak : this should notify the old rand_source that it is being discarded

    end,

    register(scutil_rand_source, RandomGeneratorPid),
    { ok, { seeded, {A,B,C} } }.





%% @private

random_generator(SeedA, SeedB, SeedC) ->

    random:seed(SeedA, SeedB, SeedC),
    random_generator().





%% @private

random_generator() ->

    receive

        terminate ->
            { ok, terminated };

        [Return, Range] ->
            Val = random:uniform(Range),
            Return ! Val,
            random_generator();

        _  ->
            random_generator()

    end.





%% @spec rand(Range::integer()) -> integer()

%% @doc {@section Random} Returns a pseudorandom integer on the range `[0 - (Range-1)]' inclusive. ```1> scutil:rand(100).
%% 9
%%
%% 2> [ scutil:rand(100) || X <- lists:seq(1,10) ].
%% [12,27,99,86,20,96,28,36,28,15]
%%
%% 3> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,992}, {1,990}, {2,992}, {3,1033}, {4,1017}, {5,1003}, {6,996}, {7,1024}, {8,969}, {9,984}]
%%
%% 4> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,1028}, {1,979}, {2,934}, {3,970}, {4,1035}, {5,1007}, {6,986}, {7,1012}, {8,1052}, {9,997}]'''

%% @since Version 5

rand(Range) ->

    case whereis(scutil_rand_source) of

        undefined ->
            srand(),
            rand(Range);

        _ ->

            scutil_rand_source ! [ self(), Range ],
            receive RandVal -> RandVal - 1 end

    end.





%% @equiv random_from(1, List, no_remainder)
%% @since Version 6

random_from(List) ->

    [X] = random_from(1, List, no_remainder), X.



%% @equiv random_from(N, List, no_remainder)
%% @since Version 6

random_from(N, List) ->

    random_from(N, List, no_remainder).



%% @spec random_from(N::integer(), List::list(), WantRemainder::want_remainder()) -> list()

%% @doc {@section Random} Take N non-repeating random elements from a list in undefined order.  If the atom `remainder' is passed in as the third argument, the unused portion of the source list will be returned as the second member of a 2ary tuple with the results; the default is no_remainder, which only returns the result set.  Mixed type input lists are perfectly safe, and membership for random selection is shallow (ie, `[ [1,2], [3,4] ]' as an input list would only generate outputs of lists, never integers.)```1> scutil:random_from([monday,tuesday,wednesday,thursday,friday]).
%% friday
%%
%% 2> scutil:random_from(4, lists:seq(1,20)).
%% [6,3,15,12]
%%
%% 3> scutil:random_from(3, [warrior, mage, cleric, thief, paladin, ranger, bard]).
%% [cleric,warrior,ranger]
%%
%% 4> scutil:random_from(6, [mixed, [1,2,3], 4, {five,5}, 3, 67.2, <<"Hello">>, 8]).
%% [[1,2,3],{five,5},4,mixed,<<"Hello">>,67.2]
%%
%% 5> {Team1, Team2} = scutil:random_from(3, [alice,bob,cathy,dave,edward,fawn], remainder).
%% {[cathy,fawn,dave],[bob,edward,alice]}
%%
%% 6> Team1.
%% [cathy,fawn,dave]
%%
%% 7> Where_Food = fun() -> scutil:random_from([deli, fastfood, chinese, mexican, steakhouse, bistro, greek, indian, thai, sushi]) end.
%% #Fun<erl_eval.20.67289768>
%%
%% 8> Where_Food().
%% thai'''

%% @since Version 6

random_from(N, List, no_remainder) ->

    {R,_} = random_from(N,List,remainder), R;



random_from(N, List, remainder) ->

    lists:split(N,shuffle(List)).





%% @type weightedvalue() = { Value::any(), Weight::number() }.  Used by functions like weighted_arithmetic_mean/1 and random_from_weighted/1, weightedvalue()s represent a value with an associated importance or "weight".
%% @type weightlist() = list().  All members of weightlists must be weightedvalue()s.

%% @spec random_from_weighted(InputList::weightlist()) -> any()

%% @doc {@section Random} Take a random single item from a list with weighted probabilities.  Probabilities may be any numeric type, and may be any non-negative value (items with zero probability will be omitted).  Input is a `weightlist()', which is a list in the form `[{Item,Probability}, {I2,P2}, ...]'. There is no requirement to normalize probabilities to any range, though probabilities normalized to ranges will still work as expected. ```1> scutil:random_from([ {quad,4}, {double,2}, {single,1} ]).
%% quad
%%
%% 2> [ scutil:random_from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,10) ].
%% [single,quad,quad,double,quad,double,quad,quad,quad,double]
%%
%% 3> scutil:histograph([ scutil:random_from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,777777) ]).
%% [{double,222200},{quad,444165},{single,111412}]'''
%% @since Version 10

% InputList is [ {Item,Weight}, {Item,Weight}, ... ]

random_from_weighted(InputList) when is_list(InputList) ->

    RandomLimit = rand(lists:sum([ Weight || {_,Weight} <- InputList ])),  % the random cap is equal to the sum of all the weights
    random_from_weighted_worker(InputList, RandomLimit).                   % call the worker with the original list and the cap



% if the list is empty, the cap for randomness was calculated wrongly, and as such the random point is too high

random_from_weighted_worker([], _) ->

    { error, limit_miscalculation };



% but if the list has reasonable contents and the limit is a pos-or-0 integer

random_from_weighted_worker(InputList, Limit) when is_list(InputList), is_integer(Limit), Limit >= 0 ->

    [ {Item,Weight} | Remainder ] = InputList,   % break off the input list's head as {I,W} and keep the rest as Remainder

    case Weight =< Limit of                                             % if the weight is less than or equal to the limit,

        true  ->
            random_from_weighted_worker(Remainder, Limit-Weight);       % recurse the next item with a decremented weight

        false ->
            Item                                                        % if not, this item is the one we want

    end.
