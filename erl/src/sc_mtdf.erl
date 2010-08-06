
-module(sc_mtdf).

-export([
    mtdf/3
]).





% Since 445

alphabeta_wm(Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother) -> todo.

    case get(Node) of

        undefined ->

        { cached, Lower, Upper, Last } -> if

            Lower >= Beta  -> Lower;
            Upper =< Alpha -> Upper;

            true ->
                alphabeta_wm_2(Node, max(Alpha, Lower), min(Beta, Upper), Depth, Eval, FirstChild, NextBrother);

    end.





%% Since 447

whittle_down(Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother) ->

    whittle_down_2(neg_infinity, Alpha, FirstChild(Node), Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother).

%% Since 448

whittle_down_2(G, A, _C,        _Node, _Alpha,  Beta, _Depth, _Eval, _FirstChild, _NextBrother) when G < Beta -> G;
whittle_down_2(G, A,  no_child, _Node, _Alpha, _Beta, _Depth, _Eval, _FirstChild, _NextBrother)               -> G;
whittle_down_2(G, A,  C,         Node,  Alpha,  Beta,  Depth,  Eval,  FirstChild,  NextBrother)               ->

    NewG = lmax(G, alphabeta_wm(C, A, Beta, Depth-1, Eval, FirstChild, NextBrother)),
    whittle_down_2(G2, lmax(A,G), NextBrother(C), Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother).






%% Since 448

whittle_up(Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother) ->

    whittle_up_2(pos_infinity, Beta, FirstChild(Node), Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother).

%% Since 449

whittle_up_2(G, _B, _C,        _Node, _Alpha, _Beta, _Depth, _Eval, _FirstChild, _NextBrother) when G > Alpha -> G;
whittle_up_2(G, _B,  no_child, _Node, _Alpha, _Beta, _Depth, _Eval, _FirstChild, _NextBrother)                -> G;
whittle_up_2(G,  B,  C,         Node,  Alpha,  Beta,  Depth,  Eval,  FirstChild,  NextBrother)                ->

    NewG = lmin(g, alphabeta_wm(C, Alpha, B, Depth-1, Eval, FirstChild, NextBrother)),
    whittle_up_2(NewG, lmin(B,G), NextBrother(C), Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother).





%% Since 446

alphabeta_wm_2(Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother) -> todo.

    G = case D of
        0 -> Eval(Node);
        _ -> case is_maxnode(Node) of
            true  -> whittle_down(Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother);
            false -> whittle_up(  Node, Alpha, Beta, Depth, Eval, FirstChild, NextBrother)
        end
    end,

    % Traditional transposition table storing of bounds */
    % Fail low result implies an upper bound */
    if g <= alpha then Node.upperbound := g; store Node.upperbound;
    % Found an accurate minimax value - will not occur if called with zero window */
    if g >  alpha and g < beta then

          Node.lowerbound := g; Node.upperbound := g; store Node.lowerbound, Node.upperbound;

    % Fail high result implies a lower bound */
    if g >= beta then Node.lowerbound := g; store Node.lowerbound;
    return g;





%% Since 444

less_than(neg_infinity, neg_infinity) -> false;
less_than(neg_infinity, _)            -> true;
less_than(pos_infinity, pos_infinity) -> false;
less_than(_,            pos_infinity) -> true;
less_than(A,            B)            -> A < B.





%% Since 442

% Starting with a traditional version in pseudocode, will refine in SVN passes

mtdf(Root, F, D, Eval) ->

    mtdf(Root, F, D, pos_infinity, neg_infinity, F).





%% Since 443

mtdf(Root, F, D, Eval, LowerBound, UpperBound, G) ->

    case less_than(LowerBound, UpperBound) of

        true ->

            Beta = case G == LowerBound of
                true  -> G+1;
                false -> G
            end,

            NewG = alphabeta_wm(Root, Beta-1, Beta, D, Eval)

            { NewUB, NewLB } = case NewG < Beta of
                true  -> { NewG,       LowerBound };
                false -> { UpperBound, NewG       }
            end,

            mtdf(Root, F, D, Eval, NewLB, NewUB, NewG);

        false ->
            G

    end.
