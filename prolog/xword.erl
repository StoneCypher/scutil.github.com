
-module(xword).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-twitter({"JohnHaugeland", "http://twitter.com/JohnHaugeland"}).
-twitter({"ScUtil", "http://twitter.com/ScUtil"}).
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").
-svn_date("$Date$").

-description("").

-todo([unit_tests,stochastic_tests,lib_test_rig_integrate,docs,doc_examples,doc_extraction]).





-testerl_export( { [], xword_testsuite } ).





-library_requirements([
    {scutil,  161},
    {testerl, 66}
]).





-export([

    gen/1,

    row/2,
    rows/1,

    col/2,
    cols/1,

    line_to_words/1,

    rows_to_words/1,
    cols_to_words/1,
    grid_to_words/1,

    grid_to_decomposed_words/1,
    decomposition_to_clause/4,

    grid_to_word_locs/1,

    word_to_clause/1,
    words_to_clauses/1,

    decompose_to_mesh/1,
    grid_to_mesh/1,
    mesh_to_clauses/1,
    grid_to_clauses/1,
    puzzle_to_clauses/1,

    random_clauses/0,
      random_clauses/1,

    negation_triangle/1,

    to_prolog/1,
      to_prolog/2,
      to_prolog/3

]).





pattern_to_grid(Pattern) ->

%    % force all rows to be the same length
%    case sc_stats:histograph([ length(Row) || Row <- Pattern ]) of
%        [{_,_}] ->

    GridBase = list_to_tuple( [list_to_tuple(I) || I <- string:tokens(string:strip(string:strip(Pattern, both, $\r), both, $\n),"\r\n") ] ),
    Height   = size(GridBase),
    Width    = size(element(1,GridBase)),
    { GridBase, Width, Height }.





row(N, {Grid,_,_}) ->

    element(N, Grid).



rows({Grid,W,H}) ->

    [ {I, tuple_to_list(row(I, {Grid,W,H})) } || I <- lists:seq(1,H) ].





col(N, {Grid,_,_}) ->

    [ element(N, Row) || Row <- tuple_to_list(Grid) ].



cols({Grid,W,H}) ->

    [ { I, col(I, {Grid,W,H}) } || I <- lists:seq(1,W) ].





word_to_clause(Word) ->

    "word" ++ integer_to_list(length(Word)) ++ "(" ++ Word ++ ", " ++ sc_string:implode(", ", Word) ++ ").\r\n".





words_to_clauses(Words) ->

    lists:flatten([ word_to_clause(Word) || Word <- Words ]).





line_to_words(Line) ->

    Base = sc_string:explode("#", Line),
    line_to_words(Base, 1, []).



line_to_words([], _Offset, Work) ->

    lists:reverse(Work);


line_to_words([[] | Rem], Offset, Work) ->

    line_to_words(Rem, Offset+1, Work);


line_to_words([[_OneChar] | Rem], Offset, Work) ->

    line_to_words(Rem, Offset+2, Work);


line_to_words([Item | Rem], Offset, Work) ->

    line_to_words(Rem, Offset+length(Item)+1, [{Offset, Item}] ++ Work).





rows_to_words(PGrid) ->

    lists:flatten([ [ {{C,N},across,W} || {C,W} <- line_to_words(R) ] || {N,R} <- rows(PGrid) ]).





cols_to_words(PGrid) ->

    lists:flatten([ [ {{N,R},down,W} || {R,W} <- line_to_words(C)] || {N,C} <- cols(PGrid) ]).





grid_to_word_locs(PGrid) ->

    lists:flatten(rows_to_words(PGrid) ++ cols_to_words(PGrid)).





clause_word_length({_,_,S}) ->

    length(S).





diagonal_sort_on_loc( {_,{X1,Y1},_}, {_,{X2,Y2},_} ) ->

    (X1+Y1) < (X2+Y2).





better_clause_ordering(Positions) ->

    Segments   = sc_lists:partition_n(Positions, fun clause_word_length/1),
    SortedSegs = lists:append([ lists:sort(fun diagonal_sort_on_loc/2, Segment) || { _SegID, Segment } <- Segments ]),

    SortedSegs.





grid_to_decomposed_words(PGrid) ->

    WordLocs      = grid_to_word_locs(PGrid),
    RawLocs       = lists:keysort(2,lists:usort([ Loc || { Loc, _, _ } <- WordLocs ])),
    Positions     = lists:zip(RawLocs, lists:seq(1,length(RawLocs))),

    WordPositions = [ { { proplists:get_value(Loc, Positions), Dir}, Loc, Word } || { Loc, Dir, Word } <- WordLocs ],

    { better_clause_ordering(WordPositions), Positions }.





grid_to_words(PGrid) ->

    {WordLocs, Locs} = grid_to_decomposed_words(PGrid),
    Words = [ {Id, Word} || { Id, _, Word } <- WordLocs ],

    RLocs = [ {Id, Pos} || {Pos, Id} <- Locs ],

    { Words, RLocs }.





label_for_id_dir(Id, across) -> "A" ++ integer_to_list(Id);
label_for_id_dir(Id, down)   -> "D" ++ integer_to_list(Id).





varname( {X,Y} ) -> lists:flatten(io_lib:format("VV_~4.10.0b_~4.10.0b", [X,Y])).





decomposition_to_clause( {Id, Direction}, Loc, Word, BothDirCells ) ->
    decomposition_to_clause( {Id, Direction}, Loc, Word, BothDirCells, [] ).



decomposition_to_clause({Id, Direction}, _Loc, [], _BothDirCells, Work ) ->
    { label_for_id_dir(Id, Direction),
      lists:flatten( "word" ++
                     integer_to_list(length(Work)) ++
                     "(" ++
                     label_for_id_dir(Id, Direction) ++
                     ", " ++
                     sc_string:implode(", ", lists:reverse(Work)) ++
                     ")"
                   ) };



decomposition_to_clause( {Id, across}, {X,Y}, [$. | RemWord], BothDirCells, Work ) ->

    case lists:member({X,Y}, BothDirCells) of
        true ->
            decomposition_to_clause( {Id, across}, {X+1,Y}, RemWord, BothDirCells, [varname({X,Y})] ++ Work );
        false ->
            decomposition_to_clause( {Id, across}, {X+1,Y}, RemWord, BothDirCells, ["           _"] ++ Work )
    end;



decomposition_to_clause( {Id, down}, {X,Y}, [$. | RemWord], BothDirCells, Work ) ->

    case lists:member({X,Y}, BothDirCells) of
        true ->
            decomposition_to_clause( {Id, down}, {X,Y+1}, RemWord, BothDirCells, [varname({X,Y})] ++ Work );
        false ->
            decomposition_to_clause( {Id, down}, {X,Y+1}, RemWord, BothDirCells, ["           _"] ++ Work )
    end;



decomposition_to_clause( {Id, across}, {X,Y}, [Fixed | RemWord], BothDirCells, Work ) ->

    decomposition_to_clause( {Id, across}, {X+1,Y}, RemWord, BothDirCells, [[Fixed]] ++ Work );



decomposition_to_clause( {Id, down}, {X,Y}, [Fixed | RemWord], BothDirCells, Work ) ->

    decomposition_to_clause( {Id, down}, {X,Y+1}, RemWord, BothDirCells, [[Fixed]] ++ Work ).





decompose_to_mesh({Decompose, _Labels}) ->

    { Across, Down } = lists:partition( fun( { {_,across}, _, _ } ) -> true; (_) -> false end, Decompose ),

    HCells = lists:flatten([ [{X+Wo,Y} || Wo <- lists:seq(0, length(W)-1)] || {_, {X,Y}, W} <- Across ]),
    VCells = lists:flatten([ [{X,Y+Wo} || Wo <- lists:seq(0, length(W)-1)] || {_, {X,Y}, W} <- Down ]),

    BothDirs = sc_lists:list_intersection(HCells, VCells),

    { Decompose, BothDirs }.





grid_to_mesh(PGrid) ->

    decompose_to_mesh(grid_to_decomposed_words(PGrid)).





mesh_to_clauses( { Decompositions, BothDirCells } ) ->

    [ decomposition_to_clause(Label, Loc, Word, BothDirCells) || {Label, Loc, Word} <- Decompositions ].





grid_to_clauses(PGrid) ->

    mesh_to_clauses(grid_to_mesh(PGrid)).





puzzle_to_clauses(Puzzle) ->

    grid_to_clauses(gen(Puzzle)).





random_clauses() ->
    random_clauses(xword_words:wordlist()).


random_clauses(Words) ->
    [ xword:word_to_clause(W) || W <- sc_random:shuffle(Words) ].





negation_triangle(List) ->

    [ L1 ++ " \= " ++ L2 || L1 <- List, L2 <- List, L1 < L2 ].





to_prolog(Puzzle) ->

    to_prolog(Puzzle, xword_words:wordlist()).



to_prolog(Puzzle, Wordlist) ->

    Clauses     = puzzle_to_clauses(Puzzle),
    BaseClauses = [ BC || {_,BC} <- Clauses ],
    Labels      = [ La || {La,_} <- Clauses ],

    io:format("make(~s).~n~n", [sc_string:implode(",", Labels)]),

    lists:flatten([ "% make(",
                    sc_string:implode(",", Labels),
                    ")\r\n\r\n",
                    random_clauses(Wordlist),
                    "\r\n\r\n\r\n\r\nmake(",
                    sc_string:implode(", ", Labels),
                    ") :-\r\n\r\n    ",
                    sc_string:implode(",\r\n    ", BaseClauses),
                    ",\r\n\r\n    ",
                    sc_string:implode(",\r\n    ", negation_triangle(Labels)),
                    ".\r\n"
                  ]).



to_prolog(Puzzle, Wordlist, File) ->

    io:format("~n~nadd_to_path('~s').~n[~s].~n", [filename:dirname(File), filename:rootname(filename:basename(File))]),

    file:write_file(File, to_prolog(Puzzle, Wordlist)).





gen(Pattern) when is_list(Pattern) ->

    pattern_to_grid(Pattern).
