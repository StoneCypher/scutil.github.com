
-module(sc_eqc).





-export([

    pos_int/0,
      non_neg_int/0

]).





-include_lib("eqc/include/eqc.hrl").





-svn_revision("$Revision$").





%% @doc Extended quickcheck generators.  Kept separate because I don't want to require eQC to compile scutil, and because eQC
%% conflicts eval/1,2 and shuffle/1.  The list of QuickCheck generators could use substantial extension, and, well, here it is.
%%
%% @end





%% @since Version 655

non_neg_int() ->

    ?SUCHTHAT(I, int(), I > 0).





%% @since Version 655

pos_int() ->

    ?SUCHTHAT(I, int(), I >= 0).
