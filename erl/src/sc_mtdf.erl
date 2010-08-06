
-module(sc_mtdf).

-export([
    mtdf/3
]).





% Since 445

alphabeta_wm(Node, Alpha, Beta, Depth, Eval) -> todo.

    case get(Node) of

        undefined ->

        { cached, Lower, Upper, Last } -> if

            Lower >= Beta  -> Lower;
            Upper =< Alpha -> Upper;

            true ->
                alphabeta_wm_2(Node, max(Alpha, Lower), min(Beta, Upper), Depth, Eval);

    end.





%% Since 446

alphabeta_wm_2(Node, Alpha, Beta, Depth, Eval) -> todo.

    G = case D of
        0 -> Eval(Node);
        _ -> case is_maxnode(Node) of
            true  -> whittle_down(Node, Alpha, Beta, Depth);
            false -> whittle_up(  Node, Alpha, Beta, Depth)
        end
    end,

    if d == 0 then g := evaluate(n); % leaf node
    else if Node == MAXNODE then

          g := -INFINITY; a := alpha; % save original alpha value */
          c := firstchild(Node);
          while (g < beta) and (c != NOCHILD) do
                g := max(g, AlphaBetaWithMemory(c, a, beta, d - 1));
                a := max(a, g);
                c := nextbrother(c);

    else % n is a MINNODE */

          g := +INFINITY; b := beta; % save original beta value */
          c := firstchild(Node);
          while (g > alpha) and (c != NOCHILD) do
                g := min(g, AlphaBetaWithMemory(c, alpha, b, d - 1));
                b := min(b, g);
                c := nextbrother(c);

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
