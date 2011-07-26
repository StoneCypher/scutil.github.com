
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

    rand_elements/1,

    pos_integer/0,
    non_neg_integer/0,

    misc_type/0,

    unicode_codepoint/0,

    time12/0,
    time24/0,

    roygbiv/0,
    roygbp/0,
    roygbpwb/0,
    rgb_color/0,
    websafe_216/0,

    playing_card/0,
    playing_card/1

]).





-include_lib("eqc/include/eqc.hrl").





-svn_revision("$Revision$").





%% @since Version 660

%% @doc This generates random unicode codepoints (eqc's `char()' generates `byte()'s, as legacy from before Erlang spoke unicode).  ```1> eqc_gen:sample(sc_eqc:unicode_codepoint()).
%% 587555
%% 824312
%% 1039897
%% 520984
%% 1059424
%% 82091
%% 325780
%% 932253
%% 584998
%% 830263
%% 29761
%% ok
%%
%% 2> eqc_gen:sample(sc_eqc:unicode_codepoint()).
%% 750151
%% 165286
%% 495247
%% 242058
%% 614341
%% 1013687
%% 141668
%% 460131
%% 72752
%% 398389
%% 106443
%% ok'''

unicode_codepoint() ->

    choose(0, 16#10ffff).





%% @since Version 655

%% @doc Generator to produce non-negative integers, with otherwise identical behavior to `eqc_gen:int()'.  Inherits range scaling behavior from use of `int()' macro.  May produce zeros - if you don't want zeros, use `pos_integer()' instead.  ```1> eqc_gen:sample(sc_eqc:non_neg_integer()).
%% 10
%% 9
%% 6
%% 12
%% 12
%% 9
%% 14
%% 9
%% 2
%% 10
%% 10
%% ok
%%
%% 2> eqc_gen:sample(sc_eqc:non_neg_integer()).
%% 11
%% 12
%% 0
%% 4
%% 13
%% 8
%% 14
%% 0
%% 14
%% 9
%% 10
%% ok'''

non_neg_integer() ->

    ?SUCHTHAT(I, int(), I >= 0).





%% @since Version 655

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.  Inherits range scaling behavior from use of `int()' macro.  Will not produce zeros - if you want zeros, use `non_neg_integer()' instead.  ```1> eqc_gen:sample(sc_eqc:pos_integer()).
%% 9
%% 1
%% 9
%% 11
%% 5
%% 6
%% 14
%% 6
%% 3
%% 8
%% 8
%% ok
%%
%% 2> eqc_gen:sample(sc_eqc:pos_integer()).
%% 9
%% 10
%% 12
%% 3
%% 8
%% 10
%% 13
%% 5
%% 15
%% 17
%% 3
%% ok'''

pos_integer() ->

    ?SUCHTHAT(I, int(), I > 0).





%% @since Version 657

%% @doc Generator to produce randomly typed, randomly valued output.  General approach found in http://www.trapexit.org/Recursive_Genereators#Generators_for_data_type .

misc_type() ->

    %% add float, binary, bitstring, ?pid?, etc

    case sc:random_from([ int, iolist, unicode_string, list, tuple ]) of

        int ->
            int();

        iolist ->
            list(eqc_gen:char());

        unicode_string ->
            list(unicode_codepoint());

        list ->
            list(misc_type(shallow));

        tuple ->
            list_to_tuple(list(misc_type(shallow)))

    end.





%% @since Version 657

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.

misc_type(shallow) ->

    %% add float, binary, bitstring, ?pid?, etc

    case sc:random_from([ int, iolist, unicode_string, list, tuple ]) of

        int ->
            int();

        iolist ->
            list(eqc_gen:char());

        unicode_string ->
            list(unicode_codepoint());

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

%% @doc Produces a human-readable time as a 12-hour 4-tuple `{h:int(),m:int(),s:int(),am|pm}'.  Hours are on the interval [1..12] inclusive.  ```1> eqc_gen:sample(sc_eqc:time12()).
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

%% @doc Produces a human-readable time as a 24-hour 3-tuple `{h:int(),m:int(),s:int()}'.  Hours are on the interval [0..23] inclusive.  ```9> c("/projects/scutil/erl/src/sc_eqc.erl").
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





%% @since Version 666

roygbiv() ->

    eqc_gen:elements([red, orange, yellow, green, blue, indigo, violet]).





%% @since Version 667

roygbp() ->

    eqc_gen:elements([red, orange, yellow, green, blue, purple]).





%% @since Version 667

roygbpwb() ->

    eqc_gen:elements([red, orange, yellow, green, blue, purple, white, black]).





%% @since Version 668

websafe_216() ->

    [ eqc_gen:elements([$0, $3, $6, $9, $c, $f]), eqc_gen:elements([$0, $3, $6, $9, $c, $f]), eqc_gen:elements([$0, $3, $6, $9, $c, $f]) ].





%% @since Version 670

rgb_color() ->

    [ eqc_gen:elements(lists:seq($0,$9)++lists:seq($a,$f)), eqc_gen:elements(lists:seq($0,$9)++lists:seq($a,$f)),
      eqc_gen:elements(lists:seq($0,$9)++lists:seq($a,$f)), eqc_gen:elements(lists:seq($0,$9)++lists:seq($a,$f)),
      eqc_gen:elements(lists:seq($0,$9)++lists:seq($a,$f)), eqc_gen:elements(lists:seq($0,$9)++lists:seq($a,$f)) ].





%% @since Version 669

%% @equiv playing_card(with_jokers)

%% @doc Equivalent to calling with the argument `with_jokers'.  ```1> eqc_gen:sample(sc_eqc:playing_card()).
%% {8,spades}
%% {small,joker}
%% {5,hearts}
%% {7,clubs}
%% {jack,clubs}
%% {king,clubs}
%% {ace,hearts}
%% {jack,hearts}
%% {jack,clubs}
%% {5,hearts}
%% {2,diamonds}
%% ok'''

playing_card() ->

    playing_card(with_jokers).





%% @since Version 669

%% @doc Generates a random US-style playing card, with or without jokers depending on the argument passed.  ```1> eqc_gen:sample(sc_eqc:playing_card(with_jokers)).
%% {10,spades}
%% {small,joker}
%% {10,spades}
%% {king,spades}
%% {small,joker}
%% {king,spades}
%% {9,hearts}
%% {small,joker}
%% {ace,clubs}
%% {ace,diamonds}
%% {6,spades}
%% ok
%%
%% eqc_gen:sample(sc_eqc:playing_card(without_jokers)).
%% {7,spades}
%% {3,hearts}
%% {jack,hearts}
%% {5,clubs}
%% {jack,spades}
%% {queen,spades}
%% {10,diamonds}
%% {7,diamonds}
%% {queen,diamonds}
%% {9,hearts}
%% {10,diamonds}
%% '''



playing_card(without_jokers) ->

    eqc_gen:elements([ {Face,Suit} || Face <- [ace]++lists:seq(2,10)++[jack,queen,king], Suit <- [hearts,clubs,spades,diamonds] ]);





%% @since Version 669

playing_card(with_jokers) ->

    eqc_gen:elements([ {Face,Suit} || Face <- [ace]++lists:seq(2,10)++[jack,queen,king], Suit <- [hearts,clubs,spades,diamonds] ] ++ [{large,joker}, {small,joker}]).





% % @ since Version 665 todo

% % @ doc Produces a human-readable time as a 24-hour 3-tuple `{h:int(),m:int(),s:int()}'.  Hours are on the interval [0..23] inclusive.  ``` '''

% date_short_atom_us() ->

%     { eqc_gen:elements(sc:months_as_short_atoms()), eqc_gen:choose(0,59), eqc_gen:choose(0,59) }.

% needs to take into account length of month, and whether a given month/year is a leap year
