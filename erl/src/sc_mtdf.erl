
-module(sc_mtdf).

-export([
    mtdf/3
]).





%% Since 442

% Starting with a traditional version in pseudocode, will refine in SVN passes

mtdf(Root, F, D) ->

    mtdf(Root, F, D, pos_infinity, neg_infinity, F).





%% Since 444

mtdf(Root, F, D, LowerBound, UpperBound, G) ->

    case less_than(LowerBound, UpperBound) of

        true ->

            if G = lowerBound then
                Beta = G + 1
            else
                Beta = G
    
            G = alphabeta_wm(Root, Beta-1, Beta, D)
    
            if G < Beta then
                UpperBound = G
            else
                LowerBound = G

        false ->
            G

    end.
