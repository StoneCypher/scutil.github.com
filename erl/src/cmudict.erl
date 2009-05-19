
-module(cmudict).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-twitter({"JohnHaugeland", "http://twitter.com/JohnHaugeland"}).
-twitter({"ScUtil", "http://twitter.com/ScUtil"}).
-license( {mit_license, "http://scutil.com/license.html"} ).  % proprietary or { licensename, "url" } eg "http://fullof.bs/standard_license.html"

-publicsvn(none).
-bugtracker(none).
-publicforum(none).

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Routines to work with the CMU Pronouncing Dictionary").





-testerl_export( { [], "" } ).   %  no_testsuite or { [], foo_testsuite }
-library_requirements( [] ).





-export( [
   file_to_plist/1,
   to_plist/1
] ).





%% @since Version 379

chop_term([]) ->

    skip;


chop_term(";;;" ++ _Comment) ->

    skip;


chop_term(Term) ->

    [Word, Pronunciation] = sc_string:explode("  ", Term),
    Phonemes              = [ string:to_lower(X) || X <- sc_string:explode(" ", Pronunciation) ],
    {string:to_lower(Word), Phonemes}.





%% @since Version 379

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





%% @since Version 379

file_to_plist(File) ->

    {ok, Data} = file:read_file(File),
    to_plist(Data).





%% @since Version 379

to_plist(BData) when is_binary(BData) ->
   to_plist(binary_to_list(BData));



%% @since Version 379

to_plist(LData) when is_list(LData) ->
    walk_list(LData, [], []).
