
-module(sc.module).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=7").
-publicforum("http://crunchyd.com/forum/scutil-discussion/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("StoneCypher's utility library.").

-testerl_export( { [], scutil_testsuite } ).

-library_requirements( [] ).





-export( [

    get_module_feature/2,       % needs tests

    get_module_attribute/1,     % needs tests
    get_module_attribute/2,     % needs tests

    abstract_attributes/1,      % needs tests

    abstract_function/2,        % needs tests
    abstract_functions/1,       % needs tests

    atoms/1,             % needs tests


    entrypoints/1,         % needs tests
    entrypoints/2,         % needs tests


    function_labels/1,     % needs tests
    function_points/1      % needs tests

    entrypoint_count/1,         % needs tests
    function_label_count/1,     % needs tests
    function_point_count/1,     % needs tests

    function_stats/1,           % needs tests

] ).





%% @since Version 130

% was scutil:module_atoms/1

atoms(Module) ->

    get_module_feature(Module, atoms).





%% @since version 138

% was `scutil:abstract_attributes/1'
abstract_attributes(Module) ->

    [ {Id, Name, Value} ||
        {attribute, Id, Name, Value} <- abstract_code(Module, stripped)
    ].





%% @since version 138

% was `scutil:abstract_functions/1'
abstract_functions(Module) ->

    [ {Id, Name, Arity, Code} ||
        {function, Id, Name, Arity, Code} <- abstract_code(Module, stripped)
    ].





%% @since version 138

% was `scutil:abstract_function/2'
abstract_function(Module, FName) ->

    [ {Id, Name, Arity, Code} ||
        {function, Id, Name, Arity, Code} <- abstract_code(Module, stripped),
        Name == FName
    ].





%% @since Version 140

% was `scutil:list_entrypoints/1'
entrypoints(Module) ->

    lists:flatten(
        [ [{L,A,[{Kind,Name}||{Kind,_LineNum,Name}<-ThisAcArg],When} || {_,_,ThisAcArg,When,_} <- AbstractClauseList ] || {_,L,A,AbstractClauseList} <- scutil:abstract_functions(Module) ]
    ).





%% @since Version 140

% was `scutil:list_entrypoints/2'
entrypoints(Module, FName) ->

    lists:flatten(
        [ [{L,A,[{Kind,Name}||{Kind,_LineNum,Name}<-ThisAcArg],When} || {_,_,ThisAcArg,When,_} <- AbstractClauseList ] || {_,L,A,AbstractClauseList} <- scutil:abstract_functions(Module), L==FName ]
    ).





%% @since Version 140

% was `scutil:list_function_points/1'
function_points(Module) ->

    lists:usort(
        [ {L,A} ||
            {_,L,A,_} <- scutil:abstract_functions(Module)
        ]
    ).





%% @since Version 140

% was `scutil:list_function_labels/1'
function_labels(Module) ->

    lists:usort(
        [ L ||
            {_,L,_,_} <- scutil:abstract_functions(Module)
        ]
    ).





%% @since Version 140

% was `scutil:list_function_points/1'
function_points(Module) ->

    lists:usort(
        [ {L,A} ||
            {_,L,A,_} <- scutil:abstract_functions(Module)
        ]
    ).





%% @since Version 140

% was `scutil:list_function_labels/1'
function_labels(Module) ->

    lists:usort(
        [ L ||
            {_,L,_,_} <- scutil:abstract_functions(Module)
        ]
    ).





%% @since version 138

entrypoint_count(Module) ->

    length(list_entrypoints(Module)).





%% @since Version 140

function_label_count(Module) ->

    length(list_function_labels(Module)).





%% @since Version 140

function_point_count(Module) ->

    length(list_function_points(Module)).





%% @since Version 140

function_stats(Module) -> 

    [ { entrypoints,     entrypoint_count(Module)     },
      { function_labels, function_label_count(Module) },
      { function_points, function_point_count(Module) }
    ].
