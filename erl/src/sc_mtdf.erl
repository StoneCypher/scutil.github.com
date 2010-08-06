
-module(sc_mtdf).

-export([
    mtdf/3
]).





%% Since 442

% Starting with a traditional version in pseudocode, will refine in SVN passes

mtdf(Root, F, D) ->

    FirstG     = F,
    UpperBound = pos_infinity,
    LowerBound = neg_infinity,

    while LowerBound < UpperBound
    
        if g = lowerBound then
            beta = g+1
        else
            beta = g
        
        g = alphabeta_wm(root, beta-1, beta, d)

        if g < b then
            upperbound = g
        else
            lowerbound = g
        
    end while
    
    return g
