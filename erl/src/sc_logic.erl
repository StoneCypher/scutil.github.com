
-module(sc_logic).





-export([

    power_set/1

]).





%% @since Version

power_set(L) when is_list(L) ->

    Size = length(L),
    lists:append( [[[]]] ++ [ sc_lists:combinations(L,Sz) || Sz <- lists:seq(1,Size) ] ).
