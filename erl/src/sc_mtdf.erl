
-module(sc_mtdf).

-export([

    mtdf/7,

    lmin/2,
      lmax/2

]).





%% Since 453

undefined_node(LastSought) ->

    { neg_infinity, pos_infinity, LastSought }.





% Since 445

alphabeta_wm(Id, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought) ->

    case get({board, Id}) of

        undefined ->

           alphabeta_wm_2(Id, undefined_node(LastSought), Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought);

        { Lower, Upper, Last } -> if

            Lower >= Beta  -> Lower;
            Upper =< Alpha -> Upper;

            true ->
                alphabeta_wm_2(Id, {Lower, Upper, Last}, lmax(Alpha, Lower), lmin(Beta, Upper), Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought)

        end

    end.





%% Since 451

lmin(A,B) ->

    case less_than(A,B) of
        true  -> A;
        false -> B
    end.





%% Since 452

lmax(A,B) ->

    case less_than(A,B) of
        true  -> B;
        false -> A
    end.





%% Since 447

whittle_down(Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought) ->

    whittle_down_2(neg_infinity, Alpha, FirstChild(Node), Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought).

%% Since 448

whittle_down_2(G, _A, _C,        _Node, _Alpha,  Beta, _Depth, _Eval, _IsMaxNode, _FirstChild, _NextBrother, _LastSought) when G < Beta -> G;
whittle_down_2(G, _A,  no_child, _Node, _Alpha, _Beta, _Depth, _Eval, _IsMaxNode, _FirstChild, _NextBrother, _LastSought)               -> G;
whittle_down_2(G,  A,  C,         Node,  Alpha,  Beta,  Depth,  Eval,  IsMaxNode,  FirstChild,  NextBrother,  LastSought)               ->

    NewG = lmax(G, alphabeta_wm(C, A, Beta, Depth-1, Eval, IsMaxNode, FirstChild, NextBrother, LastSought)),
    whittle_down_2(NewG, lmax(A,G), NextBrother(C), Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought).






%% Since 448

whittle_up(Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought) ->

    whittle_up_2(pos_infinity, Beta, FirstChild(Node), Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought).

%% Since 449

whittle_up_2(G, _B, _C,        _Node,  Alpha, _Beta, _Depth, _Eval, _IsMaxNode, _FirstChild, _NextBrother, _LastSought) when G > Alpha -> G;
whittle_up_2(G, _B,  no_child, _Node, _Alpha, _Beta, _Depth, _Eval, _IsMaxNode, _FirstChild, _NextBrother, _LastSought)                -> G;
whittle_up_2(G,  B,  C,         Node,  Alpha,  Beta,  Depth,  Eval,  IsMaxNode,  FirstChild,  NextBrother,  LastSought)                ->

    NewG = lmin(g, alphabeta_wm(C, Alpha, B, Depth-1, Eval, IsMaxNode, FirstChild, NextBrother, LastSought)),
    whittle_up_2(NewG, lmin(B,G), NextBrother(C), Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought).





%% Since 446

alphabeta_wm_2(Id, Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought) ->

    G = case Depth of
        0 -> Eval(Node);
        _ -> case IsMaxNode(Node) of
            true  -> whittle_down(Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought);
            false -> whittle_up(  Node, Alpha, Beta, Depth, Eval, IsMaxNode, FirstChild, NextBrother, LastSought)
        end
    end,

    % Traditional transposition table storing of bounds */

    if
        % Fail low result implies an upper bound */
        G =< Alpha ->
            { Lower, _Upper, _Last } = Node,
            put({board,Id}, { Lower, G, LastSought });

        % Found an accurate minimax value - will not occur if called with zero window */
        ((G > Alpha) and (G < Beta)) ->
            { _Lower, _Upper, _Last } = Node,
            put({board,Id}, { G, G, LastSought });

        % Fail high result implies a lower bound */
        G >= Beta ->
           { _Lower, Upper, _Last } = Node,
           put({board,Id}, { G, Upper, LastSought })

        % no default action

    end,

    G.





%% Since 444

less_than(neg_infinity, neg_infinity) -> false;
less_than(neg_infinity, _)            -> true;
less_than(pos_infinity, pos_infinity) -> false;
less_than(_,            pos_infinity) -> true;
less_than(A,            B)            -> A < B.





%% Since 442

% Starting with a traditional version in pseudocode, will refine in SVN passes

mtdf(Root, F, D, Eval, IsMaxNode, FirstChild, NextBrother) ->

    mtdf(Root, F, D, Eval, IsMaxNode, FirstChild, NextBrother, pos_infinity, neg_infinity, F).





%% Since 443

mtdf(Root, F, D, Eval, IsMaxNode, FirstChild, NextBrother, LowerBound, UpperBound, G) ->

    case less_than(LowerBound, UpperBound) of

        true ->

            Beta = case G == LowerBound of
                true  -> G+1;
                false -> G
            end,

            NewG = alphabeta_wm(Root, Beta-1, Beta, D, Eval, IsMaxNode, FirstChild, NextBrother),

            { NewUB, NewLB } = case NewG < Beta of
                true  -> { NewG,       LowerBound };
                false -> { UpperBound, NewG       }
            end,

            mtdf(Root, F, D, Eval, IsMaxNode, FirstChild, NextBrother, NewLB, NewUB, NewG);

        false ->
            G

    end.
