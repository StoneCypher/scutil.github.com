
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <p></p>


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





%% @spec scan_svn_revision(ModuleName::atom()) -> integer()

%% @doc {@section Utility} Scans a module for an attribute svn_revision, parses it in the format expected from the svn:keyword Revision, and returns the version number as an integer.  To use, add a module attribute to your module as follows: `-svn_revision("$+Revision$).', after removing the plus (if the plus wasn't there, the example would get corrupted when I updated the module `;)').  Then set the svn keyword "Revision" on the file, and check it in.  After that, your version is magically updated every time you check in!  `:D'  The sole argument to this function is the name of the module to be scanned, as an atom. ```1> scutil:scan_svn_revision(testerl).
%% 16'''

%% @since Version 44

scan_svn_revision(Module) ->

    "$Revision: " ++ X = get_module_attribute(Module, svn_revision),
    [ Head | _Rem ]    = string:tokens(X, " "),

    list_to_integer(Head).





%% @spec module_has_function(Module::atom(), Function::atom()) -> boolean()

%% @doc TODO

%% @since Version 84

module_has_function(Module, Function) ->

    scutil:deprecate("module_has_function() is deprecated in favor of erlang:function_exported/3"),
    lists:keymember(Function, 1, apply(Module, module_info, [exports])).





%% @type typelabel() = [ integer | float | list | tuple | binary | bitstring | boolean | function | pid | port | reference | atom | unknown ].  Used by type_of(), this is just any single item from the list of erlang's primitive types, or the atom <tt>unknown</tt>.

%% @spec type_of(Argument::any()) -> typelabel()

%% @doc {@section Utility} Fetch the type of the argument.  Valid for any term.  Fails before erlang 12, due to use of `is_bitstring()' . ```1> scutil:type_of(1).
%% integer
%%
%% 2> scutil:type_of({hello,world}).
%% tuple'''

%% @since Version 14

type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before erlang 12
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;

type_of(_X)                     -> unknown.





%% @since Version 127

get_module_feature(Module, Feature) ->

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

get_module_attribute(Module) ->

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

get_module_attribute(Module,Attribute) ->

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
