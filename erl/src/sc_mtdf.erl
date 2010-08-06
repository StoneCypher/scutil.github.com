
-module(sc_mtdf).

-export([
    mtdf/3
]).





% Since 445

alphabeta_wm(Root, Alpha, Beta, Depth) -> todo.

    if retrieve(n) == OK then % Transposition table lookup

          if n.lowerbound >= beta then return n.lowerbound;
          if n.upperbound <= alpha then return n.upperbound;
          alpha := max(alpha, n.lowerbound);
          beta := min(beta, n.upperbound);

    if d == 0 then g := evaluate(n); % leaf node
    else if n == MAXNODE then

          g := -INFINITY; a := alpha; % save original alpha value */
          c := firstchild(n);
          while (g < beta) and (c != NOCHILD) do
                g := max(g, AlphaBetaWithMemory(c, a, beta, d - 1));
                a := max(a, g);
                c := nextbrother(c);

    else % n is a MINNODE */

          g := +INFINITY; b := beta; % save original beta value */
          c := firstchild(n);
          while (g > alpha) and (c != NOCHILD) do
                g := min(g, AlphaBetaWithMemory(c, alpha, b, d - 1));
                b := min(b, g);
                c := nextbrother(c);

    % Traditional transposition table storing of bounds */
    % Fail low result implies an upper bound */
    if g <= alpha then n.upperbound := g; store n.upperbound;
    % Found an accurate minimax value - will not occur if called with zero window */
    if g >  alpha and g < beta then

          n.lowerbound := g; n.upperbound := g; store n.lowerbound, n.upperbound;

    % Fail high result implies a lower bound */
    if g >= beta then n.lowerbound := g; store n.lowerbound;
    return g;





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
