
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since September 14, 2007

%% @doc Extended quickcheck generators.  Kept separate because I don't want to require eQC to compile scutil, and because eQC
%% conflicts eval/1,2 and shuffle/1.  The list of QuickCheck generators could use substantial extension, and, well, here it is.
%% These are essentially entirely used by `sc_tests.erl'.
%%
%% @end





-module(sc_eqc).





-export([

    pos_integer/0,
    non_neg_integer/0,
    misc_type/0,
    real_char/0,
    rand_elements/1,
    time12/0,
    time24/0

]).





-include_lib("eqc/include/eqc.hrl").





-svn_revision("$Revision$").





%% @since Version 660

%% @doc eqc's char() generates byte()s, not char()s.  The difference came when Erlang moved to support unicode.

real_char() ->

    choose(0, 16#10ffff).





%% @since Version 655

%% @doc Generator to produce non-negative integers, with otherwise identical behavior to `eqc_gen:int()'.

non_neg_integer() ->

    ?SUCHTHAT(I, int(), I > 0).





%% @since Version 655

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.

pos_integer() ->

    ?SUCHTHAT(I, int(), I >= 0).





%% @since Version 657

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.

misc_type() ->

    %% add float, binary, bitstring, ?pid?, etc

    case sc:random_from([ int, iolist, unicodestring, list, tuple ]) of

        int ->
            int();

        iolist ->
            list(eqc_gen:char());

        unicodestring ->
            list(real_char());

        list ->
            list(misc_type(shallow));

        tuple ->
            list_to_tuple(list(misc_type(shallow)))

    end.





%% @since Version 657

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.

misc_type(shallow) ->

    %% add float, binary, bitstring, ?pid?, etc

    case sc:random_from([ int, iolist, unicodestring, list, tuple ]) of

        int ->
            int();

        iolist ->
            list(eqc_gen:char());

        unicodestring ->
            list(real_char());

        list ->
            [];

        tuple ->
            {}

    end.





%% @since Version 661

%% @doc Produces a list of zero or more elements taken from `ElementList', out of order, possibly repeating.  Scales list size on test depth from using list() macro.  ```1> eqc_gen:sample(sc_eqc:rand_elements([1,2,3,4,5])).
%% []
%% [1,5,2]
%% [3,4]
%% [3,2,4]
%% [2,2,5]
%% []
%% [3,3]
%% [4,3,1,3,5,3]
%% [3]
%% [2,5,4]
%% [1,4,5,4]
%% ok
%%
%% 2> eqc_gen:sample(sc_eqc:rand_elements([1,2,3,4,5])).
%% [3]
%% []
%% [5,4]
%% [4,3,3,5]
%% [1,2]
%% [5,2]
%% [4,1,3,4,2]
%% [5,2]
%% [3,4,4,2]
%% [4,4,2,3]
%% [2,4,2,5,4,4]
%% ok'''

rand_elements(ElementList) ->

    ?LET(L,list(elements(ElementList)),L).





%% @since Version 662

%% @doc Produces a human-readable time as a 12-hour 4-tuple {h:int(),m:int(),s:int(),am|pm}.  Hours are on the interval [1..12] inclusive.  ```1> eqc_gen:sample(sc_eqc:time12()).
%% {3,41,52,am}
%% {8,48,3,am}
%% {9,11,38,pm}
%% {1,49,6,am}
%% {7,21,33,pm}
%% {10,44,50,pm}
%% {2,37,20,pm}
%% {3,18,59,am}
%% {8,46,42,pm}
%% {12,47,35,pm}
%% {2,0,41,am}
%% ok
%%
%% 2> eqc_gen:sample(sc_eqc:time12()).
%% {4,55,46,am}
%% {12,8,22,pm}
%% {12,44,15,pm}
%% {6,59,28,am}
%% {4,36,0,pm}
%% {2,33,10,pm}
%% {9,7,22,pm}
%% {10,19,10,am}
%% {9,57,33,am}
%% {3,31,8,am}
%% {7,34,23,am}
%% ok'''

time12() ->

    { eqc_gen:choose(1,12), eqc_gen:choose(0,59), eqc_gen:choose(0,59), eqc_gen:elements([am,pm]) }.





%% @since Version 663

%% @doc Produces a human-readable time as a 24-hour 3-tuple {h:int(),m:int(),s:int()}.  Hours are on the interval [0..23] inclusive.  ```9> c("/projects/scutil/erl/src/sc_eqc.erl").
%% {ok,sc_eqc}
%% 340> eqc_gen:sample(sc_eqc:time24()).
%% {4,26,35}
%% {20,20,45}
%% {12,32,55}
%% {14,48,23}
%% {9,44,16}
%% {17,34,31}
%% {5,18,50}
%% {18,0,1}
%% {6,33,51}
%% {23,41,59}
%% {20,57,0}
%% ok
%%
%% 341> eqc_gen:sample(sc_eqc:time24()).
%% {1,49,3}
%% {20,44,19}
%% {0,13,25}
%% {18,59,25}
%% {10,16,17}
%% {21,40,6}
%% {9,37,13}
%% {12,52,5}
%% {3,15,29}
%% {1,55,3}
%% {13,53,31}
%% ok'''

time24() ->

    { eqc_gen:choose(0,23), eqc_gen:choose(0,59), eqc_gen:choose(0,59) }.
