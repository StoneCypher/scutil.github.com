
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 259 $
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-03-15 13:47:01 -0600 (Sun, 15 Mar 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc.columns).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: columns.erl 259 2009-03-23 00:40:13Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/src/sc/columns.erl $").
-svn_revision("$Revision: 259 $").

-description("Text columnation routines").

-testerl_export( { [], sc_columns_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    columnated_text/2,
    columnated_rows/2,

    columnate/1,
      columnate/2,

    columns/2,
    first_row/1

] ).





%% @since Version 171

columns(RowCount, List) ->

    columns(List, .lists:duplicate(RowCount, []), 0).



columns( [], Output, Unrotate) -> 

    .sc.list:rotate(
        Unrotate, 
        [ .lists:reverse(Column) ||
            Column <- Output
        ]
    );



columns( [Item|ListRem], [Output|OutRem], Unrotate) ->

    columns(ListRem, OutRem ++ [[Item]++Output], Unrotate-1).





%% @since Version 173

columnated_rows(ColumnCount, List) ->

    columns(.sc.math:ceiling(length(List) / ColumnCount), List).





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

    { .lists:reverse(Work), .lists:reverse(OutCols) };



first_row( [ThisCol|RemCols], OutCols, Work) ->

    {Item,ColRem} = first_or_nothing(ThisCol),
    first_row(RemCols, [ColRem]++OutCols, [Item]++Work).





%% @equiv columnate(List, 2, 3)
%% @since Version 204

columnate(List) ->

    columnate(List, []).



%% @since Version 204

columnate(List, Options) ->

    Settings = .lists:ukeymerge(1, Options, [{align, center}, {columns, 2}, {margin, 3}] ),

    [ColumnCount, Margin, Align] = [ .proplists:get_value(X, Settings) || X <- [columns,margin,align] ],

    Columns = columns(ColumnCount, List),

    MinWidths = [ .lists:max( [length(.lists:flatten(.io_lib:format("~w",[Item]))) || Item <- Col]) || Col <- Columns ],

    DoAlign = fun(Item, Width) ->

        case Align of
            left   -> .string:left(   .lists:flatten( .io_lib:format("~w",[Item]) ), Width);
            center -> .string:centre( .lists:flatten( .io_lib:format("~w",[Item]) ), Width);
            centre -> .string:centre( .lists:flatten( .io_lib:format("~w",[Item]) ), Width);
            right  -> .string:right(  .lists:flatten( .io_lib:format("~w",[Item]) ), Width)
        end

    end,

    Aligned = [ [ DoAlign(Item, Width) || Item <- Col]  || {Width, Col} <- .lists:zip(MinWidths, Columns) ],

    Format = .sc.list:implode( .lists:duplicate(Margin, $ ), [ "~" ++ integer_to_list(Width) ++ "s" || Width <- MinWidths ] ),

    columnate_each_row(Aligned, Format, []).





%% @since Version 205

columnated_text(List, Options) ->

    .sc.list:implode("\r\n", columnate(List, Options)).





%% @since Version 204

columnate_each_row( [ [] | _ ], _Format, Output) ->

    .lists:reverse(Output);



columnate_each_row(Columns, Format, Output) ->

    {ThisRow, RemRows} = first_row(Columns),
    ThisOut = .lists:flatten(.io_lib:format(Format, ThisRow)),
    columnate_each_row(RemRows, Format, [ThisOut]++Output).
