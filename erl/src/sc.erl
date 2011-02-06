
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

    flag_sets/1,
    member_sets/1,
      member_sets/2,

    rotate_list/2,
      rotate_to_first/2,
      rotate_to_last/2,

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





