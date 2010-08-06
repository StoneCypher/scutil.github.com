
-module(sc_mtdf).

-export([
    mtdf/3
]).





%% Since 444

less_than(neg_infinity, neg_infinity) -> false;
less_than(neg_infinity, _)            -> true;
less_than(pos_infinity, pos_infinity) -> false;
less_than(_,            pos_infinity) -> true;
less_than(A,            B)            -> A < B.





%% Since 442

% Starting with a traditional version in pseudocode, will refine in SVN passes

mtdf(Root, F, D) ->

    mtdf(Root, F, D, pos_infinity, neg_infinity, F).





%% Since 443

mtdf(Root, F, D, LowerBound, UpperBound, G) ->

    case less_than(LowerBound, UpperBound) of

        true ->

            Beta = case G == LowerBound of
                true  -> G+1;
                false -> G
            end,

            NewG = alphabeta_wm(Root, Beta-1, Beta, D)

            { NewUB, NewLB } = case NewG < Beta of
                true  -> { NewG,       LowerBound };
                false -> { UpperBound, NewG       }
            end,

            mtdf(Root, F, D, NewLB, NewUB, NewG);

        false ->
            G

    end.
