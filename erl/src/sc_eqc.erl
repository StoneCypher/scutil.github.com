
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

    pos_int/0,
      non_neg_int/0,
      misc_type/0

]).





-include_lib("eqc/include/eqc.hrl").





-svn_revision("$Revision$").





%% @since Version 655

%% @doc Generator to produce non-negative integers, with otherwise identical behavior to `eqc_gen:int()'.

non_neg_int() ->

    ?SUCHTHAT(I, int(), I > 0).





%% @since Version 655

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.

pos_int() ->

    ?SUCHTHAT(I, int(), I >= 0).





%% @since Version 655

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.

misc_type() ->

    %% add float, binary, bitstring, ?pid?, etc

    case sc:random_from([ int, string, list, tuple ]) of

        int ->
            int();

        string ->
            list(int());

        list ->
            list(misc_type(shallow));

        tuple ->
            {misc_type(shallow)}

    end.





%% @since Version 655

%% @doc Generator to produce positive integers, with otherwise identical behavior to `eqc_gen:int()'.

misc_type(shallow) ->

    %% add float, binary, bitstring, ?pid?, etc

    case sc:random_from([ int, string, list, tuple ]) of

        int ->
            int();

        string ->
            list(int());

        list ->
            [];

        tuple ->
            {}

    end.
