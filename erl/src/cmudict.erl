
-module(cmudict).

-author("John Haugeland - stonecypher@gmail.com").
-webpage(none).
-twitter("JohnHaugeland").
-license( {mit_license, "http://scutil.com/license.html"} ).  % proprietary or { licensename, "url" } eg "http://fullof.bs/standard_license.html"

-publicsvn(none).
-bugtracker(none).
-publicforum(none).

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("").





-testerl_export( { [], "" } ).   %  no_testsuite or { [], foo_testsuite }
-library_requirements( [] ).





-export( [
   file_to_plist/1,
   to_plist/1
] ).





chop_term([]) ->

    skip;


chop_term(";;;" ++ _Comment) ->

    skip;


chop_term(Term) ->

    [Word, Pronunciation] = sc_string:explode("  ", Term),
    Phonemes              = [ string:to_lower(X) || X <- sc_string:explode(" ", Pronunciation) ],
    {string:to_lower(Word), Phonemes}.





walk_list([], Work, Output) ->

    Map = fun(X) ->
        chop_term(lists:reverse(X))
    end,

    Filter = fun
       ( skip ) -> false;
       ( _Any ) -> true
    end,

    sc_lists:reverse_map_filter([Work] ++ Output, Map, Filter);


walk_list("\r\n" ++ LData, Work, Output) ->

    walk_list(LData, [], [Work]++Output);


walk_list([NewChar | LData], Work, Output) ->

    walk_list(LData, [NewChar]++Work, Output).





file_to_plist(File) ->

    {ok, Data} = file:read_file(File),
    to_plist(Data).





to_plist(BData) when is_binary(BData) ->
   to_plist(binary_to_list(BData));



to_plist(LData) when is_list(LData) ->
    walk_list(LData, [], []).
