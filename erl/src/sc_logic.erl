
-module(sc_logic).





-export([

    power_set/1,

    union/1,
      union/2,

    intersection/2

]).





%% @since Version 435

power_set(L) when is_list(L) ->

    Size = length(L),
    lists:append( [[[]]] ++ [ sc_lists:combinations(L,Sz) || Sz <- lists:seq(1,Size) ] ).





%% @since Version 436

union(L) ->

    lists:usort(lists:append(L)).



%% @equiv union( [L1, L2] )

union(L1, L2) ->

    union([L1,L2]).

% todo test require that sort(union(powerset(A))) == sort(A)





%% @equiv sc_lists:list_intersection/2
% @todo make a version for any number of lists

intersection(L1, L2) ->

    sc_lists:intersection(L1,L2).
