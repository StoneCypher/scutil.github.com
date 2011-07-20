
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
      real_char/0

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
