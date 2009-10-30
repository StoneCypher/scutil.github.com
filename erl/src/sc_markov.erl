
-module(sc_markov).





-export([

%    list_to_chain/1,
    list_to_chain_pattern/1,

%    weights_for/2,
%    next_for/2,

%    gen_from/1

]).





% matrix should take, for length, either a positive integer or a function which takes the current
% length, the current letter queue and the length histo and returns a length to matrix over





% @since Version 425

list_to_chain_pattern(List) ->

    Bases = sc_lists:key_cluster(1, [ { A,B,I } || { [A,B], I } <- sc_stats:histograph(sc_lists:all_neighbor_pairs(List, make_lists)) ]),
    RawProplist = [ { [Label], [ {[B],I} || {_A,B,I} <- InnerList ] } || { Label, InnerList } <- Bases ],
    { sc_markov_chain_pattern, dict:from_list(RawProplist) }.





% @since Version 426

weights_for(Item, {sc_markov_chain_pattern, Pattern}) ->

    dict:fetch(Item, Pattern).
