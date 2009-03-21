




%% @since Version 171

columns(RowCount, List) -> 

    columns(List, lists:duplicate(RowCount, []), 0).



columns( [], Output, Unrotate) -> list_rotate(Unrotate, [ lists:reverse(Column) || Column <- Output]);



columns( [Item|ListRem], [Output|OutRem], Unrotate) ->

    columns(ListRem, OutRem++[[Item]++Output], Unrotate-1).





%% @since Version 173

columnated_rows(ColumnCount, List) ->

    columns(ceiling(length(List) / ColumnCount), List).





%% @since Version 204

first_or_nothing( [H|T] ) ->

    {H,T};



first_or_nothing( [] ) ->

    { [], [] }.





%% @todo needs spec, doc, since

first_row(Columns) ->

     first_row(Columns, [], []).



%% @private

first_row( [], OutCols, Work) ->

    { lists:reverse(Work), lists:reverse(OutCols) };



first_row( [ThisCol|RemCols], OutCols, Work) ->

    {Item,ColRem} = first_or_nothing(ThisCol),
    first_row(RemCols, [ColRem]++OutCols, [Item]++Work).





%% @equiv columnate(List, 2, 3)
%% @since Version 204

columnate(List) ->

    columnate(List, []).



%% @since Version 204

columnate(List, Options) ->

    Settings = lists:ukeymerge(1, Options, [{align, center}, {columns, 2}, {margin, 3}] ),

    [ColumnCount, Margin, Align] = [ proplists:get_value(X, Settings) || X <- [columns,margin,align] ],

    Columns = scutil:columns(ColumnCount, List),

    MinWidths = [ lists:max( [length(lists:flatten(io_lib:format("~w",[Item]))) || Item <- Col]) || Col <- Columns ],

    DoAlign = fun(Item, Width) ->

        case Align of
            left   -> string:left(   lists:flatten( io_lib:format("~w",[Item]) ), Width);
            center -> string:centre( lists:flatten( io_lib:format("~w",[Item]) ), Width);
            centre -> string:centre( lists:flatten( io_lib:format("~w",[Item]) ), Width);
            right  -> string:right(  lists:flatten( io_lib:format("~w",[Item]) ), Width)
        end

    end,

    Aligned = [ [ DoAlign(Item, Width) || Item <- Col]  || {Width, Col} <- lists:zip(MinWidths, Columns) ],

    Format = implode( lists:duplicate(Margin, $ ), [ "~" ++ integer_to_list(Width) ++ "s" || Width <- MinWidths ] ),

    columnate_each_row(Aligned, Format, []).





%% @since Version 205

columnated_text(List, Options) ->

    implode("\r\n", columnate(List, Options)).





%% @since Version 204

columnate_each_row( [ [] | _ ], _Format, Output) ->

    lists:reverse(Output);



columnate_each_row(Columns, Format, Output) ->

    {ThisRow, RemRows} = first_row(Columns),
    ThisOut = lists:flatten(io_lib:format(Format, ThisRow)),
    columnate_each_row(RemRows, Format, [ThisOut]++Output).
