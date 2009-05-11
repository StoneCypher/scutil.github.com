
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date$</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc_module).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Routines for working with modules directly, after compile.").

-testerl_export( { [], sc_module_testsuite } ).

-library_requirements( [
] ).





-export( [

    feature/2,                  % needs tests

    attribute/1,                % needs tests
    attribute/2,                % needs tests

    abstract_attributes/1,      % needs tests

    abstract_function/2,        % needs tests
    abstract_functions/1,       % needs tests

    atoms/1,                    % needs tests

    entrypoints/1,              % needs tests
    entrypoints/2,              % needs tests

    function_labels/1,          % needs tests
    function_points/1,          % needs tests

    entrypoint_count/1,         % needs tests

    function_label_count/1,     % needs tests
    function_point_count/1,     % needs tests

    function_stats/1,           % needs tests

    svn_revision/1,             % needs tests
    has_function/2              % needs tests

] ).





%% @since Version 130

% was scutil:module_atoms/1

atoms(Module) ->

    feature(Module, atoms).





%% @since version 138

% was `scutil:abstract_attributes/1'
abstract_attributes(Module) ->

    [ {Id, Name, Value} ||
        {attribute, Id, Name, Value} <- sc_code:abstract(Module, stripped)
    ].





%% @since version 138

% was `scutil:abstract_functions/1'
abstract_functions(Module) ->

    [ {Id, Name, Arity, Code} ||
        {function, Id, Name, Arity, Code} <- sc_code:abstract(Module, stripped)
    ].





%% @since version 138

% was `scutil:abstract_function/2'
abstract_function(Module, FName) ->

    [ {Id, Name, Arity, Code} ||
        {function, Id, Name, Arity, Code} <- sc_code:abstract(Module, stripped),
        Name == FName
    ].





%% @since Version 140

% was `scutil:list_entrypoints/1'
entrypoints(Module) ->

    .lists:flatten(
        [ [{L,A,[{Kind,Name}||{Kind,_LineNum,Name}<-ThisAcArg],When} || {_,_,ThisAcArg,When,_} <- AbstractClauseList ] || {_,L,A,AbstractClauseList} <- sc_code:abstract_functions(Module) ]
    ).





%% @since Version 140

% was `scutil:list_entrypoints/2'
entrypoints(Module, FName) ->

    lists:flatten(
        [ [{L,A,[{Kind,Name}||{Kind,_LineNum,Name}<-ThisAcArg],When} || {_,_,ThisAcArg,When,_} <- AbstractClauseList ] || {_,L,A,AbstractClauseList} <- sc_code:abstract_functions(Module), L==FName ]
    ).





%% @since Version 140

% was `scutil:list_function_points/1'
function_points(Module) ->

    lists:usort(
        [ {L,A} ||
            {_,L,A,_} <- sc_code:abstract_functions(Module)
        ]
    ).





%% @since Version 140

% was `scutil:list_function_labels/1'
function_labels(Module) ->

    lists:usort(
        [ L ||
            {_,L,_,_} <- sc_code:abstract_functions(Module)
        ]
    ).





%% @since version 138

entrypoint_count(Module) ->

    length(entrypoints(Module)).





%% @since Version 140

function_label_count(Module) ->

    length(function_labels(Module)).





%% @since Version 140

function_point_count(Module) ->

    length(function_points(Module)).





%% @since Version 140

function_stats(Module) ->

    [ { entrypoints,     entrypoint_count(Module)     },
      { function_labels, function_label_count(Module) },
      { function_points, function_point_count(Module) }
    ].





%% @spec scan_svn_revision(ModuleName::atom()) -> integer()

%% @doc {@section Utility} Scans a module for an attribute svn_revision, parses it in the format expected from the svn:keyword Revision, and returns the version number as an integer.  To use, add a module attribute to your module as follows: `-svn_revision("$+Revision$).', after removing the plus (if the plus wasn't there, the example would get corrupted when I updated the module `;)').  Then set the svn keyword "Revision" on the file, and check it in.  After that, your version is magically updated every time you check in!  `:D'  The sole argument to this function is the name of the module to be scanned, as an atom. ```1> scutil:scan_svn_revision(testerl).
%% 16'''

%% @since Version 44

svn_revision(Module) ->

    "$Revision: " ++ X = attribute(Module, svn_revision),
    [ Head | _Rem ]    = string:tokens(X, " "),

    list_to_integer(Head).





%% @spec module_has_function(Module::atom(), Function::atom()) -> boolean()

%% @doc TODO

%% @since Version 84

% was scutil:module_has_function/2

has_function(Module, Function) ->

    scutil:deprecate("module_has_function() is deprecated in favor of erlang:function_exported/3"),
    lists:keymember(Function, 1, apply(Module, module_info, [exports])).





%% @since Version 127

% was scutil:get_module_feature/2

feature(Module, Feature) ->

    case beam_lib:chunks(Module, [Feature]) of

        { ok, { Module, [ {Feature,Attributes} ] } } ->
            Attributes;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





%% @spec get_module_attribute(Module::atom()) -> AttributeList | { error, no_such_module }

%% @doc {@section Utility} Look up all attributes of a given module.  ```1> scutil:get_module_attribute(scutil).
%% [{author,"John Haugeland <stonecypher@gmail.com>"},
%%  {bugtracker,"http://crunchyd.com/forum/project.php?projectid=7"},
%%  {currentsource,"http://crunchyd.com/release/scutil.zip"},
%%  {description,"StoneCypher's utility library."},
%%  {library_requirements,[{testerl,16}]},
%%  {license,[{mit_license,"http://scutil.com/license.html"}]},
%%  {publicforum,"http://crunchyd.com/forum/scutil-discussion/"},
%%  {publicsvn,"svn://crunchyd.com/scutil/"},
%%  {svn_head,"$HeadURL$"},
%%  {svn_id,"$Id$"},
%%  {svn_revision,"$Revision$"},
%%  {testerl_export,[{[],scutil_testsuite}]},
%%  {vsn,[134633400955530778836494569152232539093]},
%%  {webpage,"http://scutil.com/"}]'''

%% @since Version 129

% was scutil:get_module_attribute/1

attribute(Module) ->

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->
            Attributes;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.

%% @spec get_module_attribute(Module::atom(), Attribute::atom()) -> { value, {Attribute, Value} } | { error, no_such_attribute } | { error, no_such_module }

%% @doc {@section Utility} <span style="color:red">Buggy</span> Look up an Erlang module attribute value by title.  Originally found at <a href="http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html">Mastering Erlang Part 3</a>; subsequently cleaned up and given error reporting.  ```1> scutil:get_module_attribute(scutil, author).
%% "John Haugeland <stonecypher@gmail.com>"
%%
%% 2> scutil:get_module_attribute(scutil, license).
%% [{mit_license,"http://scutil.com/license.html"}]'''{@section Thanks} to Alain O'Dea for pointing out defects in this routine regarding repeated module elements, and available improvements to the provided API.  <a href="http://fullof.bs/reading-module-attributes-in-erlang#comment-475" target="_blank">Mr. O'Dea's insightful advice</a> will be implemented, but that time has not yet come.

%% @since Version 23

% was scutil:get_module_attribute/2

attribute(Module,Attribute) ->

    % Found at http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html
    % Reformatted for clarity, removed unnessecary framing list
    % Added error handling behavior

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->

            case lists:keysearch(Attribute, 1, Attributes) of

                { value, {Attribute,Value} } ->
                    Value;

                false ->
                    { error, no_such_attribute }

            end;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.
