
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since September 14, 2007

%% @doc <p>ScUtil is StoneCypher's Utility Library, a collection of various routines of a variety of topics: {@section Conversion},
%%         {@section Descriptive}, {@section List}, {@section Math}, {@section Parallelism}, {@section Random}, {@section Regex},
%%         {@section Serialism}, {@section Statistics}, {@section String} and {@section Utility} routines.  This file has aggregated
%%         dozens of useful miscellaneous routines which I'm releasing to the public in good faith.  There's no particular direction to
%%         this library; any time I write a routine that I tend to use in a lot of situations, which isn't already meaningfully
%%         classifiable into one of my other libraries, I throw it in here.  This has ended up creating a range of unrelated
%%         functionality on which most of my other libraries depend heavily.  Have fun digging around.
%%      </p>
%% == Conversion ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link hex_to_int/1}, {@link byte_to_hex/1}, {@link nybble_to_hex/1}, {@link io_list_to_hex/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == Documentary ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link even_or_odd/1}, {@link absolute_difference/2}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == List ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link shuffle/1}, {@link sanitize_tokens/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == Math ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == Parallelism ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == Random ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link grid_scatter/1}, {@link srand/0}, {@link srand/3}, {@link rand/1}, {@link random_from/1}, {@link random_from/2}, {@link random_from/3}, {@link random_from_weighted/1}, {@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == Regex ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link regex_read_matches/2}, {@link regex_read_matches/3}, {@link a/1}</dd>
%% </dl>
%% == Serialism ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link multi_do/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == Statistics ==
%% <dl>
%%   <dt>Means</dt>
%%   <dd>{@link arithmetic_mean/1}, {@link geometric_mean/1}, {@link harmonic_mean/1}, {@link weighted_arithmetic_mean/1}, {@link arithmetic_mean/1}</dd>
%%   <dt>Descriptive</dt>
%%   <dd>{@link median/1}, {@link mode/1}</dd>
%%   <dt>Normals</dt>
%%   <dd>{@link amean_vector_normal/1}, {@link gmean_vector_normal/1}, {@link hmean_vector_normal/1}</dd>
%% </dl>
%% == String ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link sanitize_filename/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == Utility ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link type_of/1}, {@link get_module_attribute/2}, {@link receive_one/0}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%% == z ==
%% <dl>
%%   <dt></dt>
%%   <dd>{@link a/1}, {@link a/1}, {@link a/1}</dd>
%% </dl>
%%
%%      <p>ScUtil is MIT license, because <a href="http://WhyIHateTheGPL.com/">the author feels very strongly against the GPL</a>.</p>
%%
%% @end

%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Direct link to zip archive</span><a href="http://crunchyd.com/release/scutil.zip">Current version</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Public SVN at</span><a href="svn://crunchyd.com/scutil/">svn://crunchyd.com/scutil/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Discussion forum at</span><a href="http://crunchyd.com/forum/scutil-discussion/">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Bugtracker at</span><a href="http://crunchyd.com/forum/project.php?projectid=7">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:20em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date$</tt></span>
%% @reference <span style="margin-top:1em;padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:20em">Test sets require min. version 16</span><a href="http://testerl.com/">TestErl</a></span>

%% @todo add @see cross-references between related functions





-module(scutil).

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
-library_requirements( [ {testerl,16} ] ).





-export( [

    type_of/1,
    get_module_attribute/2,
    byte_to_hex/1, nybble_to_hex/1, io_list_to_hex/1, % needs tests
    regex_read_matches/2, regex_read_matches/3, regex_read_matches/4, % needs tests
    multi_do/3, multi_do/4, % needs tests
    elements/2, elements/3, elements/4, % needs tests
    sanitize_tokens/2,
    sanitize_filename/1, % needs tests
    random_generator/3, srand/0, srand/3, rand/1, random_from/1, random_from/2, random_from/3, random_from_weighted/1, % needs tests
    grid_scatter/2, % needs tests
    list_product/1, % needs tests
    even_or_odd/1, % needs tests
    histograph/1, % needs tests
    median/1, % needs tests
    mode/1, % needs tests
    arithmetic_mean/1, geometric_mean/1, harmonic_mean/1, weighted_arithmetic_mean/1,  % needs tests
    absolute_difference/2, % needs tests
    std_deviation/1, % needs tests
    root_mean_square/1, % needs tests
    moment/2, moments/1, moments/2, % needs tests
    central_moment/2, central_moments/1, central_moments/2, % needs tests
%    weighted_geometric_mean/1,

    dot_product/2, cross_product/2, % needs tests
    vector_magnitude/1, % needs tests
    qsp_average/2, % needs tests
    normalize_vector/1, % needs tests
    amean_vector_normal/1, gmean_vector_normal/1, hmean_vector_normal/1, % needs tests

    ranks_of/1, % needs tests
    tied_ranks_of/1, % needs tests
    ordered_ranks_of/1, % needs tests
    pearson_correlation/1,  pearson_correlation/2, % needs tests
    spearman_correlation/1, spearman_correlation/2, % needs tests
    kendall_correlation/1,  kendall_correlation/2, % needs tests
    skewness/1, % needs tests
    kurtosis/1, % needs tests
    to_lines/1, % needs tests

    receive_one/0, % needs tests

    in_both_lists/1, in_both_lists/2, % needs tests
    all_unique_pairings/1, % needs tests
    walk_unique_pairings/2, % needs tests
    list_to_number/1, % needs tests
    counter/1, inc_counter/1, inc_counter/2, dec_counter/1, dec_counter/2, reset_counter/1, counter_process/0, % needs tests
    start_register_if_not_running/4, % needs tests
    wait_until_terminate/0, wait_until_terminate/1, % needs tests
    module_has_function/2, % needs tests

    call_after/2, call_after/3, call_after/4, call_after_worker/4, % needs tests
    shuffle/1, % needs tests

    permute/1, permute/2, % needs tests

    has_bit/2, count_bits/1, % needs tests
    diff_timestamp/2, % needs tests

    expand_label/1, expand_labels/1, % needs tests

    benchmark/3, % needs tests

    hex_to_int/1, % needs tests

    erlang_b_distribution/2, % needs tests
    erlang_c_distribution/2, % needs tests

    implode/2, % needs tests

    mod/2, % needs tests

    scan_svn_revision/1 % needs tests

] ).





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





%% @spec get_module_attribute(Module::atom(), Attribute::atom()) -> { value, {Attribute, Value} } | { error, no_such_attribute } | { error, no_such_module }

%% @doc {@section Utility} Look up an Erlang module attribute value by title.  Originally found at <a href="http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html">Mastering Erlang Part 3</a>; subsequently cleaned up and given error reporting.  ```1> scutil:get_module_attribute(scutil, author).
%% "John Haugeland <stonecypher@gmail.com>"
%%
%% 2> scutil:get_module_attribute(scutil, license).
%% [{mit_license,"http://scutil.com/license.html"}]'''

%% @since Version 23

%% @todo  Implement <a href="http://concise-software.blogspot.com/">Alain O'Dea</a>'s improvements from <a href="http://fullof.bs/reading-module-attributes-in-erlang#comment-475">this blog comment</a>
%% @todo  Add Alain O'Dea to doc attribute

get_module_attribute(Module,Attribute) ->

    % Found at http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html
    % Reformatted for clarity, removed unnessecary framing list
    % Added error handling behavior

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->
            case lists:keysearch(Attribute, 1, Attributes) of
                { value, {Attribute,Value} }   -> Value;
                false                          -> { error, no_such_attribute }
            end;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





%% @type hexchar() = integer().  Integer must be in the range $0 - $9, the range $a - $f, or the range $A - $F, all inclusive, for inputs; outputs will always use lower case.
%% @type hexstring() = list().  All elements of the list must be of type hexchar() .

%% @spec hex_to_int(HexChar::hexstring() | hexchar()) -> integer()
%% @doc {@section Conversion} Convert a hexstring() or hexchar() into its numeric value. ```1> scutil:hex_to_int("c0ffEE").
%% 12648430
%%
%% 2> scutil:hex_to_int($e).
%% 14
%%
%% 3> scutil:hex_to_int("100").
%% 256'''

%% @since Version 18

hex_to_int(Hex) when is_integer(Hex), Hex >= $0, Hex =< $9 -> Hex - $0;
hex_to_int(Hex) when is_integer(Hex), Hex >= $a, Hex =< $f -> Hex - $a + 10;
hex_to_int(Hex) when is_integer(Hex), Hex >= $A, Hex =< $F -> Hex - $A + 10;

hex_to_int(Hex) when is_list(Hex) -> hex_to_int(Hex, 0).

hex_to_int([],          Acc) -> Acc;
hex_to_int([Digit|Rem], Acc) -> hex_to_int(Rem, (Acc bsl 4) + hex_to_int(Digit)).





%% @type byte() = integer().  A byte must be an integer in the range 0-255, inclusive.  (Technically this is an octet, not a byte, but the word byte is extensively misused throughout the erlang documentation and standard library, which makes this an important concession, so we're when-in-Rome-ing.)

%% @spec byte_to_hex(TheByte::byte()) -> hexstring()

%% @doc {@section Conversion} Convert a byte() into a hexstring().  The hexstring() result will always be two characters (left padded with zero if necessary). ```1> scutil:byte_to_hex(7).
%% "07"
%%
%% 2> scutil:byte_to_hex(255).
%% "ff"'''

%% @since Version 20

byte_to_hex(TheByte) when is_integer(TheByte), TheByte >= 0, TheByte =< 255 -> [ nybble_to_hex(TheByte bsr 4), nybble_to_hex(TheByte band 15) ].





%% @type nybble() = integer().  A nybble must be an integer in the range 0-15, inclusive.

%% @spec nybble_to_hex(Nyb::nybble()) -> integer()

%% @doc {@section Conversion} Convert a nybble() to a hexchar(). ```1> scutil:nybble_to_hex(7).
%% 55
%%
%% 2> scutil:nybble_to_hex(15).
%% 102'''

%% @since Version 19

nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 0,  Nyb < 10 -> $0 + Nyb;
nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 10, Nyb < 16 -> $a + Nyb - 10.





%% @type io_list() = list().  Every list member of an io_list must be a byte().

%% @spec io_list_to_hex(Input::io_list()) -> hexstring()

%% @doc {@section Conversion} Convert an io_list() to a hexstring().  ```1> scutil:io_list_to_hex("a").
%% "61"
%%
%% 2> scutil:io_list_to_hex("a08n408nbqa").
%% "6130386e3430386e627161"'''

%% @since Version 19

io_list_to_hex(Input) when is_list(Input)                                            -> io_list_to_hex(Input, []).

io_list_to_hex([],               Work)                                               -> lists:reverse(Work);
io_list_to_hex([Item|Remainder], Work) when is_integer(Item), Item >= 0, Item =< 255 -> [A,B] = byte_to_hex(Item), io_list_to_hex(Remainder, [B,A]++Work);
io_list_to_hex(_,                _)                                                  -> {error, not_an_io_list}.





%% @equiv multi_do(C,M,F,[])
%% @since Version 38
multi_do(C, Module, Func)             -> multi_do(C, Module, Func, [],   []).

%% @spec multi_do(Count::integer(), Module::atom(), Function::atom(), Args::list()) -> list()

%% @doc {@section Serialism} Take an iteration count, a module name, a function name and an argument list, and repeatedly apply the argument list to the module/function, count times.  This is primarily useful with nondeterministic functions whose result might change despite identical arguments, such as functions with random behavior; for example, this function is invoked to implement stochastic testing in <a href="http://testerl.com/">TestErl</a>. ```1> scutil:multi_do(10, scutil, rand, [100]).
%% [9,94,4,82,77,44,89,19,45,92]
%%
%% 2> scutil:multi_do(10, scutil, rand, [10000]).
%% [2377,2559,1713,8489,4468,3261,3344,3751,380,2525]'''

%% @since Version 38
multi_do(C, Module, Func, Args)       -> multi_do(C, Module, Func, Args, []).

multi_do(0,_Module,_Func,_Args, Work) -> Work;
multi_do(I, Module, Func, Args, Work) -> multi_do(I-1, Module, Func, Args, Work ++ [apply(Module, Func, Args)]).





%% @equiv regex_read_matches(String, Reg, {0,0})
%% @since Version 41
regex_read_matches(String, Reg) -> regex_read_matches(String, Reg, {0,0}).

%% @equiv regex_read_matches(String, Reg, {TrimFront,TrimLength})
%% @since Version 41
regex_read_matches(String, Reg, TrimFront, TrimLength) -> regex_read_matches(String, Reg, {TrimFront, TrimLength}).

%% @spec regex_read_matches(String::string(), Reg::string(), { TrimFront::integer(), TrimLength::integer() }) -> list() | { error, E }

%% @doc {@section Regex} Take a string and a regular expression (and optionally an offset and length to trim to in each result), and return a list of all matches.  For a trim length of {A,B}, the first A and last B characters of each result will be removed.```1> scutil:regex_read_matches("0j2  4g5  8t9", "[0-9](.)[0-9]").
%% ["0j2","4g5","8t9"]
%%
%% 2> scutil:regex_read_matches("0j2  4g5  8t9", "[0-9](.)[0-9]", {1,1}).
%% ["j","g","t"]
%%
%% 3> scutil:regex_read_matches("0j2  4g5  8t9", "[0-9](.)[0-9]", 1, 1).
%% ["j","g","t"]'''
%%
%% Why provide the equivalent syntaxes (_, _, {A,B}) and (_, _, A,B) ?  Without the tuple is more natural to many, but with the tuple is far more convenient for database-driven behavior, as well as the internal implementation.  I frequently find myself using both forms, and so every time I simplify I find myself wrapping the non-removed form back into the removed form.  Does it violate the simplest interface principle?  Yeah, but in this case it's a boon, IMO.  As such, keeping both forms.

%% @since Version 41
regex_read_matches(String, Reg, {TrimFront, TrimLength}) ->

    case regexp:matches(String, Reg) of
        { match, Matches } -> [ string:substr(String, Start+TrimFront, End-(TrimLength+1)) || {Start,End} <- Matches ];
        { error, E }       -> { error, E }
    end.





%% @type gridsize() = coord() | integer().  Coordinates are the width and height of a (1,1) originated grid; as such, coordinates are of the range [1,X] , [1,Y] inclusive, and returned in the form {A,B}.  The integer form implies a square grid.
%% @type coord() = { number(), number() }.  Represents a coordinate, which may imply a sized rectangle.  Many functions expect integer coordinates; the type does not require them.
%% @type coordlist() = list().  All members of a coordlist() must be coord()s.

%% @spec grid_scatter(Count::integer(), Size::gridsize()) -> coordlist()

%% @doc {@section Random} Return a Count-length list of non-repeating coordinates in a grid of specified size; useful for feature generation.

%% @todo @comeback give code examples (edoc was failing here?)

%% @since Version 42

grid_scatter(0, []) -> []; % skips a lot of work

grid_scatter(Count, {SizeX, SizeY}) -> scutil:random_from(Count, [ {X,Y} || X <- lists:seq(1,SizeX), Y <- lists:seq(1,SizeY) ]);
grid_scatter(Count, Size)           -> grid_scatter(Count, {Size, Size}).





%% @spec srand() -> { ok, { seeded, Seed } }

%% @doc {@section Random} <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the clock, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want to know what seed was used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> scutil:srand().
%% {ok,{seeded,{1227,902172,685000}}}
%%
%% 2> scutil:srand().
%% {ok,{seeded,{1227,902173,231000}}}'''

%% @since Version 5
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

srand() ->

    {A,B,C} = erlang:now(),
    srand(A,B,C).





%% @spec srand(A::integer(), B::integer(), C::integer()) -> { ok, { seeded, Seed } }
%% @doc {@section Random} <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the three integer seed you provide, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want set what seed is used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> scutil:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}
%%
%% 2> scutil:srand().
%% {ok,{seeded,{1227,902568,604600}}}
%%
%% 3> scutil:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}'''

%% @since Version 5
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

srand(A,B,C) ->

    RandomGeneratorPid = spawn(?MODULE, random_generator, [A,B,C]),

    case whereis(scutil_rand_source) of
        undefined -> ok;
        _Defined  -> unregister(scutil_rand_source)  % todo fixme leak : this should notify the old rand_source that it is being discarded
    end,

    register(scutil_rand_source, RandomGeneratorPid),
    { ok, { seeded, {A,B,C} } }.





%% @private

random_generator(SeedA, SeedB, SeedC) ->

    random:seed(SeedA, SeedB, SeedC),
    random_generator().





%% @private

random_generator() ->

    receive

        terminate ->
            { ok, terminated };

        [Return, Range] ->
            Val = random:uniform(Range),
            Return ! Val,
            random_generator();

        _  ->
            random_generator()

    end.





%% @spec rand(Range::integer()) -> integer()

%% @doc {@section Random} Returns a pseudorandom integer on the range `[0 - (Range-1)]' inclusive. ```1> scutil:rand(100).
%% 9
%%
%% 2> [ scutil:rand(100) || X <- lists:seq(1,10) ].
%% [12,27,99,86,20,96,28,36,28,15]
%%
%% 3> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,992}, {1,990}, {2,992}, {3,1033}, {4,1017}, {5,1003}, {6,996}, {7,1024}, {8,969}, {9,984}]
%%
%% 4> scutil:histograph([ scutil:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,1028}, {1,979}, {2,934}, {3,970}, {4,1035}, {5,1007}, {6,986}, {7,1012}, {8,1052}, {9,997}]'''

%% @since Version 5

rand(Range) ->

    case whereis(scutil_rand_source) of

        undefined ->
            srand(),
            rand(Range);

        _ ->

            scutil_rand_source ! [ self(), Range ],
            receive RandVal -> RandVal - 1 end

    end.





%% @equiv random_from(1, List, no_remainder)
%% @since Version 6
random_from(   List) -> [X] = random_from(1, List, no_remainder), X.

%% @equiv random_from(N, List, no_remainder)
%% @since Version 6
random_from(N, List) -> random_from(N, List, no_remainder).

%% @spec random_from(N::integer(), List::list(), WantRemainder::want_remainder()) -> list()

%% @doc {@section Random} Take N non-repeating random elements from a list in undefined order.  If the atom `remainder' is passed in as the third argument, the unused portion of the source list will be returned as the second member of a 2ary tuple with the results; the default is no_remainder, which only returns the result set.  Mixed type input lists are perfectly safe, and membership for random selection is shallow (ie, `[ [1,2], [3,4] ]' as an input list would only generate outputs of lists, never integers.)```1> scutil:random_from([monday,tuesday,wednesday,thursday,friday]).
%% friday
%%
%% 2> scutil:random_from(4, lists:seq(1,20)).
%% [6,3,15,12]
%%
%% 3> scutil:random_from(3, [warrior, mage, cleric, thief, paladin, ranger, bard]).
%% [cleric,warrior,ranger]
%%
%% 4> scutil:random_from(6, [mixed, [1,2,3], 4, {five,5}, 3, 67.2, <<"Hello">>, 8]).
%% [[1,2,3],{five,5},4,mixed,<<"Hello">>,67.2]
%%
%% 5> {Team1, Team2} = scutil:random_from(3, [alice,bob,cathy,dave,edward,fawn], remainder).
%% {[cathy,fawn,dave],[bob,edward,alice]}
%%
%% 6> Team1.
%% [cathy,fawn,dave]
%%
%% 7> Where_Food = fun() -> scutil:random_from([deli, fastfood, chinese, mexican, steakhouse, bistro, greek, indian, thai, sushi]) end.
%% #Fun<erl_eval.20.67289768>
%%
%% 8> Where_Food().
%% thai'''

%% @since Version 6
random_from(N, List, no_remainder) -> {R,_} = random_from(N,List,remainder), R;
random_from(N, List, remainder)    -> lists:split(N,shuffle(List)).





%% @type weightedvalue() = { Value::any(), Weight::number() }.  Used by functions like weighted_arithmetic_mean/1 and random_from_weighted/1, weightedvalue()s represent a value with an associated importance or "weight".
%% @type weightlist() = list().  All members of weightlists must be weightedvalue()s.

%% @spec random_from_weighted(InputList::weightlist()) -> any()

%% @doc {@section Random} Take a random single item from a list with weighted probabilities.  Probabilities may be any numeric type, and may be any non-negative value (items with zero probability will be omitted).  Input is a `weightlist()', which is a list in the form `[{Item,Probability}, {I2,P2}, ...]'. There is no requirement to normalize probabilities to any range, though probabilities normalized to ranges will still work as expected. ```1> scutil:random_from([ {quad,4}, {double,2}, {single,1} ]).
%% quad
%%
%% 2> [ scutil:random_from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,10) ].
%% [single,quad,quad,double,quad,double,quad,quad,quad,double]
%%
%% 3> scutil:histograph([ scutil:random_from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,777777) ]).
%% [{double,222200},{quad,444165},{single,111412}]'''
%% @since Version 10

% InputList is [ {Item,Weight}, {Item,Weight}, ... ]
random_from_weighted(InputList) when is_list(InputList) ->
    RandomLimit = rand(lists:sum([ Weight || {_,Weight} <- InputList ])),  % the random cap is equal to the sum of all the weights
    random_from_weighted_worker(InputList, RandomLimit).                   % call the worker with the original list and the cap

% if the list is empty, the cap for randomness was calculated wrongly, and as such the random point is too high
random_from_weighted_worker([], _) -> { error, limit_miscalculation };

% but if the list has reasonable contents and the limit is a pos-or-0 integer
random_from_weighted_worker(InputList, Limit) when is_list(InputList), is_integer(Limit), Limit >= 0 ->
    [ {Item,Weight} | Remainder ] = InputList,   % break off the input list's head as {I,W} and keep the rest as Remainder
    case Weight =< Limit of                                             % if the weight is less than or equal to the limit,
        true  -> random_from_weighted_worker(Remainder, Limit-Weight);  % recurse the next item with a decremented weight
        false -> Item                                                   % if not, this item is the one we want
    end.





% todo implement catching tuple { key, reqtype } from list, to auto-convert before return
% todo There may be a crashing bug here for repeated attributes, which are apparently legal, see http://fullof.bs/reading-module-attributes-in-erlang#comment-466
% todo It may help to re-implement this using proplists instead of doing it manually, profile
%% @todo document this

% interface

elements(Config, Requested)                when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1).
elements(Config, Requested, KeyIdx)        when is_list(Config), is_list(Requested), is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx);

elements(Config, Requested, strip)         when is_list(Config), is_list(Requested)                     -> elements_worker([], Config, Requested, 1,      strip).
elements(Config, Requested, KeyIdx, strip) when is_list(Config), is_list(Requested), is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx, strip).

% implementation

elements_worker(Retlist, _,      [],        _)      -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of
        false            -> elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx);
        { value, Tuple } -> elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx);
        AnythingElse     -> { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }
    end.

elements_worker(Retlist, _,      [],        _,      strip) -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx, strip) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of
        false                -> elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx, strip);
        { value, {_,Tuple} } -> elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx, strip);
        AnythingElse         -> { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }
    end.





%% @type filterfunction() = function().  Filter functions are 1ary binary predicates - they accept an argument and return either true or false.
%% @type sanitizer() = list() | filterfunction().  Sanitizers are for input sanitization; they define what parts of an input list are valid, and the remainder are removed.  Sanitizers may either be a list of acceptable elements or a filter function.

%% @spec sanitize_tokens(InputList::list(), Allowed::sanitizer()) -> list()

%% @doc {@section List} Remove unacceptable elements from an input list, as defined by another list or a filter function.  Common reasons for sanitization include reducing arbitrary or bulk data to key format (such as using an original filename and new size to generate a new filename or database key) and removing malformed items from a list before processing. ```1> scutil:sanitize_tokens("ae0z4nb'wc-04bn ze0e 0;4ci ;e0o5rn;", "ace").
%% "aeceece"
%%
%% 2> Classifier = fun(apple) -> true; (banana) -> true; (cherry) -> true; (date) -> true; (elderberry) -> true; (_) -> false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 3> scutil:sanitize_tokens([apple, boat, cherry, dog, elderberry], Classifier).
%% [apple,cherry,elderberry]'''

%% @see sanitize_filename/1

%% @since Version 31

sanitize_tokens(List, Allowed) when is_list(List), is_function(Allowed) -> lists:filter(Allowed, List);
sanitize_tokens(List, Allowed) when is_list(List), is_list(Allowed)     -> lists:filter(fun(X) -> lists:member(X,Allowed) end, List).





%% @spec sanitize_filename(Filename::string()) -> string()

%% @doc {@section String} Sanitize an arbitrary string to be appropriate for Windows and Unix filesystems, and URLs. ```1> scutil:sanitize_filename("\h/e~l%lo! w^o@r#l*d.").
%% "helloworld"'''

%% @see sanitize_tokens/2

%% @since Version 31

sanitize_filename(Filename) -> sanitize_tokens(Filename, lists:seq($a,$z)++lists:seq($A,$Z)++lists:seq($0,$9)++"-_()[]").





%% @todo finish me

% batch_reduce(Workload, Function) ->
%
%     [ spawn(Function, LoadItem) || LoadItem <- Workload ],
%     reduce_receive(length(Workload, [])).
%
% reduce_receive(0,           Work) -> Work;
% reduce_receive(AnswerCount, Work) -> receive X -> reduce_receive(AnswerCount-1, [X]++Work).





%% @todo finish me

% distributed_batch_reduce(Workload, Function, Nodes) -> distributed_batch_reduce_handout(Workload, Function, Nodes, [], 0).

% distributed_batch_reduce_handout([],            _Function, _Nodes,              Work, 0)        -> Work;                                                                                                                                   % nothing left in the work queue, count of output outstanding is 0, work's done
% distributed_batch_reduce_handout([],             Function, _Nodes,              Work, CountOut) -> { item, {_Source, Result } } = scutil:receive_one(), distributed_batch_reduce_handout([],       Function,  [],       [Result]++Work, CountOut-1); % there's no work left in the queue, but stuff outstanding from child nodes
% distributed_batch_reduce_handout(Workload,       Function,  [],                 Work, CountOut) -> { item, { Source, Result } } = scutil:receive_one(), distributed_batch_reduce_handout(Workload, Function,  [Source], [Result]++Work, CountOut-1); % no nodes available, wait for a receive, queue the result and add the node back to the available list
% distributed_batch_reduce_handout([Item|WorkRem], {Mod,Fun}, [ThisNode|NodeRem], Work, CountOut) -> spawn(ThisNode, Mod, Fun, Item),           distributed_batch_reduce_handout(WorkRem,  {Mod,Fun}, NodeRem,  Work,           CountOut+1). % work and nodes available; dispatch some work, increment the work out counter and recurse





%% @todo finish me

% dissimilar_charset(english, lowercase) -> "abcdefghjklmnopqrstuwxyz";
% dissimilar_charset(english, mixedcase) -> "abcdefghjklmnopqrstuwxyzABDEFGHRT";
% dissimilar_charset(english, alphanum)  -> "abcdefghjklmnopqrstuwxyzABDEFGHRT34679".

% similarize_charset   a10OZ2B8 -> aloozzBB





%% @spec receive_one() -> { item, any() } | nothing_there

%% @doc {@section Utility} Pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> scutil:receive_one().
%% nothing_there
%%
%% 2> self() ! message.
%% message
%%
%% 3> scutil:receive_one().
%% {item,message}
%%
%% 4> scutil:receive_one().
%% nothing_there'''

%% @since Version 2

receive_one() ->

    receive (X) -> { item, X }
    after 0     -> nothing_there
    end.





%% @type numericlist() = list().  All members of a numeric list must be number()s.

%% @spec arithmetic_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the arithmetic mean (often called the average) of a list of numbers. ```1> scutil:arithmetic_mean([1,2,3,4,5]).
%% 3.0'''

%% @see geometric_mean/1
%% @see harmonic_mean/1
%% @see weighted_arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 33

arithmetic_mean([])                      -> 0.0;
arithmetic_mean(List) when is_list(List) -> lists:sum(List) / length(List).





%% @spec geometric_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the geometric mean of a list of numbers. ```1> scutil:geometric_mean([1,2,3,4,5]).
%% 2.6051710846973517''' The naive approach ```geometric_mean(List) -> math:pow(scutil:list_product(List), 1/length(List)).''' is not used because it accumulates error very quickly, and is as such unsuited to huge lists.

%% @see arithmetic_mean/1
%% @see harmonic_mean/1
%% @see gmean_vector_normal/1

%% @since Version 34

geometric_mean([])                       -> 0.0;
geometric_mean(List)  when is_list(List) -> math:exp(scutil:arithmetic_mean([math:log(X)||X<-List])).





%% @spec harmonic_mean(InputList::numericlist()) -> float()

%% @doc {@section Statistics} Take the harmonic mean of a list of numbers. ```1> scutil:harmonic_mean([1,2,3,4,5]).
%% 2.18978102189781'''

%% @see arithmetic_mean/1
%% @see geometric_mean/1
%% @see hmean_vector_normal/1

%% @since Version 35

harmonic_mean([])                        -> 0.0;
harmonic_mean(List)   when is_list(List) -> length(List) / lists:sum([ 1/X || X<-List ]).





%% @spec weighted_arithmetic_mean(InputList::weightlist()) -> float()

%% @doc {@section Statistics} Take the weighted arithmetic mean of the input values. ```1> scutil:weighted_arithmetic_mean([ {8,1}, {3,4}, {16,1} ]).
%% 6.0'''

%% @see arithmetic_mean/1
%% @see amean_vector_normal/1

%% @since Version 44

weighted_arithmetic_mean(List)   when is_list(List) -> weighted_arithmetic_mean(List, 0, 0).

weighted_arithmetic_mean([],           Num, Denom)  -> Num/Denom;
weighted_arithmetic_mean([{V,W}|Tail], Num, Denom)  -> weighted_arithmetic_mean(Tail, Num+(W*V), Denom+W).





%% @spec even_or_odd(Num::integer()) -> even | odd

%% @doc {@section Documentary} Documentary convenience function that returns the atoms `even' or `odd' for any integer. ```1> scutil:even_or_odd(3).
%% odd'''

%% @since Version 8

even_or_odd(Num) when is_integer(Num), Num band 1 == 0 -> even;
even_or_odd(Num) when is_integer(Num)                  -> odd.





%% @spec median(List::numberlist()) -> any()

%% @doc {@section Statistics} Takes the median (central) value of a list.  Sorts the input list, then finds and returns the middle value.  ```scutil:median([1,2,999]).
%% 2'''

%% @see arithmetic_mean/1
%% @see mode/1

%% @since Version 8

median(List) when is_list(List) ->

    SList = lists:sort(List),
    Length = length(SList),
    case even_or_odd(Length) of
        even -> [A,B] = lists:sublist(SList, round(Length/2), 2), (A+B)/2;
        odd  -> lists:nth( round((Length+1)/2), SList )
    end.





%% @spec mode(List::numberlist()) -> any()

%% @doc {@section Statistics} Takes the mode (most common) value(s) of a list, as a list.  If there are more than one value tied for most common, all tied will be returned.  This function is safe for mixed-type lists, and does not perform deep traversal (that is, the mode of `[ [2,2] ]' is `[2,2]', not `2'). ```scutil:mode([1,2,1,3,1,4]).
%% [1]
%% 
%% 2> scutil:mode([ [1,2,3], [2,3,4], [3,4,5], [2,3,4] ]).
%% [[2,3,4]]
%%
%% 3> scutil:mode([ a,b, 1, a,b, 2, a,b, 3 ]).
%% [a,b]'''

%% @see arithmetic_mean/1
%% @see median/1

%% @since Version 8

mode([])                      -> [];
mode(List) when is_list(List) -> mode_front(lists:reverse(lists:keysort(2, scutil:histograph(List)))).

mode_front([{Item,Freq}|Tail])                      -> mode_front(Tail, Freq, [Item]).

mode_front([ {Item, Freq} | Tail], Freq,   Results) -> mode_front(Tail, Freq, [Item]++Results);
mode_front([{_Item,_Freq} |_Tail],_Better, Results) -> Results;
mode_front([],                    _Freq,   Results) -> Results.





absolute_difference(A,B) -> abs(A-B).





list_product(List) when is_list(List) -> list_product(List, 1).

list_product([],          Counter) -> Counter;
list_product([Head|Tail], Counter) -> list_product(Tail, Counter*Head).





histograph(List) when is_list(List) ->

    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).

histo_count([],             Current, Count, Work) -> lists:reverse([{Current,Count}]++Work);
histo_count([Current|Tail], Current, Count, Work) -> histo_count(Tail, Current, Count+1, Work);
histo_count([New|Tail],     Current, Count, Work) -> histo_count(Tail, New,     1,       [{Current,Count}]++Work).





std_deviation(Values) when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ (Val-Mean)*(Val-Mean) || Val <- Values ])).





root_mean_square(List) when is_list(List) -> math:sqrt(arithmetic_mean([ Val*Val || Val <- List ])).





ranks_of(List) when is_list(List) -> lists:zip(lists:seq(1,length(List)),lists:reverse(lists:sort(List))).





% todo comeback make a tied_ranks_of/2 which takes a sorting predicate
% needs significant refactoring; work is being repeated

tied_ranks_of(List) -> tied_rank_worker(ranks_of(List), [], no_prev_value).

tied_add_prev(Work, {FoundAt, NewValue}) -> lists:duplicate(length(FoundAt),{lists:sum(FoundAt) / length(FoundAt), NewValue}) ++ Work.


tied_rank_worker([],               Work, PrevValue) -> lists:reverse(tied_add_prev(Work, PrevValue));
tied_rank_worker([Item|Remainder], Work, PrevValue) ->
    case PrevValue of
        no_prev_value ->
            {BaseRank,BaseVal} = Item,
            tied_rank_worker(Remainder, Work, {[BaseRank],BaseVal});
        {FoundAt,OldVal} ->
            case Item of
                {Id,OldVal} ->
                    tied_rank_worker(Remainder, Work,                           {[Id]++FoundAt,OldVal});
                {Id,NewVal} ->
                    tied_rank_worker(Remainder, tied_add_prev(Work, PrevValue), {[Id],NewVal})
            end
    end.





% todo comeback make an ordered_ranks_of/2 which takes a sorting predicate

ordered_ranks_of(List) when is_list(List) ->
    ordered_ranks_of(List, tied_ranks_of(List), []).

ordered_ranks_of([], [], Work) -> lists:reverse(Work);

ordered_ranks_of([Front|Rem], Ranks, Work) ->
    {value,Item} = lists:keysearch(Front,2,Ranks),
    {IRank,Front} = Item,
    ordered_ranks_of(Rem, Ranks--[Item], [{IRank,Front}]++Work).





% annote(Group, Key, Value) ->
% sc_config, make annote -> config, forget -> delete, recall -> get_config





to_lines(Text) -> string:tokens(Text, "\r\n"). % yay convenience functions





% test data at http://changingminds.org/explanations/research/analysis/pearson.htm

pearson_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    pearson_correlation(A,B).

pearson_correlation(List1, _)     when length(List1) < 2 -> {r,0.0};

pearson_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
pearson_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    SumXY = lists:sum([A*B || {A,B} <- lists:zip(List1,List2) ]),   % the sum of the products of each matched pair

    SumX  = lists:sum(List1),
    SumY  = lists:sum(List2),

    SumXX = lists:sum([L*L || L<-List1]),                           % the sums of the squared items
    SumYY = lists:sum([L*L || L<-List2]),

    N     = length(List1),

    case math:sqrt(   ( (N*SumXX)-(SumX*SumX) )   *   ( (N*SumYY)-(SumY*SumY) )   ) of
        0     -> {r,0.0};  % some nasty values otherwise cause divide by zero
        0.0   -> {r,0.0};  % eg [ [1,1,1,1,1], [1,1,2,1,2] ]
        Denom ->
          Numer = (N*SumXY) - (SumX * SumY),
          {r, (Numer/Denom)}
    end.





% test data at  http://geographyfieldwork.com/SpearmansRank.htm

spearman_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    spearman_correlation(A,B).

spearman_correlation(List1, _)     when length(List1) < 2 -> {rsquared,0.0};

spearman_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
spearman_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {TR1,_} = lists:unzip(ordered_ranks_of(List1)),
    {TR2,_} = lists:unzip(ordered_ranks_of(List2)),

    Numerator   = 6 * lists:sum([ (D1-D2)*(D1-D2) || {D1,D2} <- lists:zip(TR1,TR2) ]),
    Denominator = math:pow(length(List1),3)-length(List1),

    {rsquared,1-(Numerator/Denominator)}.





% http://changingminds.org/explanations/research/analysis/kendall.htm

kendall_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    kendall_correlation(A,B).

kendall_correlation(List1, _)     when length(List1) < 2 -> {tau,0.0};

kendall_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
kendall_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {RA,_} = lists:unzip(ordered_ranks_of(List1)),
    {RB,_} = lists:unzip(ordered_ranks_of(List2)),

    Ordering = lists:keysort(1,lists:zip(RA,RB)),
    {_,OrdB} = lists:unzip(Ordering),

    N = length(List1),
    P = lists:sum(kendall_right_of(OrdB, [])),

    {tau, -(( (4*P) / (N * (N - 1))) - 1) }.



kendall_right_of([],    Work) -> lists:reverse(Work);
kendall_right_of([F|R], Work) -> kendall_right_of(R, [kendall_right_of_item(F,R)]++Work).

kendall_right_of_item(B, Rem) -> length([R || R <- Rem, R < B]).





% thanks to Chile and Kraln for straightening me out on moments and central moments

moment(List, N) when is_list(List), is_number(N) ->
    scutil:arithmetic_mean( [ math:pow(Item, N) || Item <- List ] ).

moments(List)                                -> moments(List, [2,3,4]).
moments(List, Moments) when is_list(Moments) -> [ moment(List, M) || M <- Moments ].





central_moment(List, N) when is_list(List), is_number(N) ->
    ListAMean = scutil:arithmetic_mean(List),
    scutil:arithmetic_mean( [ math:pow(Item-ListAMean, N) || Item <- List ] ).

central_moments(List)                                -> central_moments(List, [2,3,4]).
central_moments(List, Moments) when is_list(Moments) -> [ central_moment(List, M) || M <- Moments ].





skewness(List) -> central_moment(List, 3).
kurtosis(List) -> central_moment(List, 4).





% quadratic scalar product average
% see http://www.inf.fu-berlin.de/inst/ag-ki/rojas_home/documents/1996/NeuralNetworks/K5.pdf pdf-page 15
% Thanks to the following for help with qsp_average and dependencies: Asterick, Chile, John Sensebe, PfhorSlayer, Raleigh

qsp_average(W, InputVecs) ->

    GetSqVnDp = fun(Xi) ->
        VnDp = vector_magnitude(dot_product(W, Xi)),
        VnDp * VnDp
        end,

    arithmetic_mean([ GetSqVnDp(Xi) || Xi <- InputVecs ]).





dot_product(VX, VY) when length(VX) == length(VY) ->
    lists:sum( [ X*Y || {X,Y} <- lists:zip(VX,VY) ] ).





cross_product( {X1,Y1,Z1}, {X2,Y2,Z2} ) -> 
    { (Y1*Z2) - (Z1*Y2) , (Z1*X2) - (X1*Z2), (X1*Y2) - (Y1*X2) }.





vector_magnitude(VX) ->
    math:sqrt(lists:sum([ X*X || X <- VX ])).





normalize_vector(VX) ->
    VM = vector_magnitude(VX),
    [ X / VM || X <- VX ].





amean_vector_normal(VX) -> arithmetic_mean(normalize_vector(VX)).
gmean_vector_normal(VX) ->  geometric_mean(normalize_vector(VX)).
hmean_vector_normal(VX) ->   harmonic_mean(normalize_vector(VX)).





% Create reverse sorted list X of 3-ary tuples {K,Ai,Bi} from sorted lists A, B of 2ary {K,Ai}/{K,Bi} tuples where key K appears in both A and B

in_both_lists(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    in_both_lists(A,B).

in_both_lists(A,B) when is_list(A), is_list(B) ->
    both_lists_next_item(A,B,[]).

both_lists_next_item([],             _,              Work) -> Work;
both_lists_next_item(_,              [],             Work) -> Work;
both_lists_next_item([ {K,Ai} | Ar], [ {K,Bi} | Br], Work) -> both_lists_next_item(Ar, Br, [{K,Ai,Bi}]++Work);

both_lists_next_item(IA,             IB,             Work) ->
    [{Ka,_}|Ar] = IA,
    [{Kb,_}|Br] = IB,
    if
        Ka < Kb -> both_lists_next_item(Ar, IB, Work);
        true    -> both_lists_next_item(IA, Br, Work)
    end.





% collects results; do not use for huge lists

all_unique_pairings(A) when is_list(A) -> all_unique_pairings(A,[]).

all_unique_pairings([],      Work) -> Work;
all_unique_pairings([Ai|Ar], Work) -> all_unique_pairings(Ar, [{Ai,Ari}||Ari<-Ar] ++ Work).





% used for side effects, doesn't gather results; appropriate for enormous lists

walk_unique_pairings([],    _) -> ok;
walk_unique_pairings([A|R], F) when is_function(F) ->
    walk_unique_pairings(A, R, F),
    walk_unique_pairings(R, F).

walk_unique_pairings(_A, [],     _F) -> ok;
walk_unique_pairings( A, [Rh|Rr], F) ->
    F(A,Rh),
    walk_unique_pairings(A, Rr, F).





list_to_number(X) ->
    case catch list_to_float(X) of
        {'EXIT',_} -> list_to_integer(X);
        Y -> Y
    end.






start_register_if_not_running(Name, Module, Function, Args) when is_atom(Name), is_atom(Module), is_atom(Function), is_list(Args) ->

    case whereis(Name) of
        undefined -> register(Name, spawn(Module, Function, Args));
        _         -> ok
    end.






counter(Name) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), get_counter, Name},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





inc_counter(Name)    -> adjust_counter(Name,     1).
inc_counter(Name,By) -> adjust_counter(Name,    By).
dec_counter(Name)    -> adjust_counter(Name,    -1).
dec_counter(Name,By) -> adjust_counter(Name, -1*By).

adjust_counter(Name, By) when is_number(By) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), adjust_counter, Name, By},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





reset_counter(Name) -> set_counter(Name, 0).

set_counter(Name, To) when is_number(To) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), set_counter, Name, To},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @todo remove any counter at zero, since a non-existant counter will report zero anyway
%% @todo add a mechanism to remove counters (already exists as reset after above change, in a sense; synonym for future under the hood changes, or document?)

%% @private

counter_process() ->
    receive
        shutdown -> ok;
        {Caller, get_counter, Name} ->
            case get(Name) of
                undefined -> Caller ! {counter_at, Name, 0}, put(Name,0), counter_process();
                Defined   -> Caller ! {counter_at, Name, Defined},        counter_process()
            end;
        {Caller, adjust_counter, Name, By} ->
            case get(Name) of
                undefined ->                 Caller ! {counter_at, Name, By},  put(Name,By),  counter_process();
                Defined   -> New=Defined+By, Caller ! {counter_at, Name, New}, put(Name,New), counter_process()
            end;
        {Caller, set_counter, Name, To} ->
            Caller ! {counter_at, Name, To},
            put(Name,To),
            counter_process()
    end.





% surprisingly useful in debugging

wait_until_terminate() -> wait_until_terminate(quiet).

wait_until_terminate(quiet) ->
    receive
        terminate -> ok;
        _         -> wait_until_terminate(quiet)
    end;

wait_until_terminate(loud) ->
    receive
        terminate -> ok;
        X         -> io:format("Received ~p~n", [X]), wait_until_terminate(loud)
    end.





module_has_function(Module, Function) ->

    scutil:deprecate("module_has_function() is deprecated in favor of erlang:function_exported/3"),
    lists:keymember(Function, 1, apply(Module, module_info, [exports])).





%% @spec shuffle(List::list()) -> list()

%% @doc Return a list with the original list's shallow members in a random order.  Deep lists are not shuffled; `[ [a,b,c], [d,e,f], [g,h,i] ]' will never produce sublist reorderings (`[b,c,a]') or list mixing (`[b,g,e]'), only reordering of the three top level lists.  The output list will always be the same length as the input list.  Repeated items and mixed types in input lists are safe. ```1> scutil:shuffle(lists:seq(1,9)).
%% [8,4,7,9,5,2,6,1,3]
%%
%% 2> {TheFaces, TheSuits} = {  [ace] ++ lists:seq(2,10) ++ [jack,queen,king],  [hearts,spades,clubs,diamonds]  }
%% {[ace,jack,queen,king,2,3,4,5,6,7,8,9,10],
%%  [hearts,spades,clubs,diamonds]}
%%
%% 3> Deck = scutil:shuffle([ {Face,Suit} || Face <- TheFaces, Suit <- TheSuits ]).
%% [ {6,spades}, {7,hearts}, {8,clubs}, {queen,spades}, {6,diamonds}, {ace,...}, {...} | ...]
%% 
%% 4> scutil:shuffle([ duck,duck,duck,duck, goose ]).
%% [duck,goose,duck,duck,duck]'''
%%
%% <i>Originally found at <a href="http://wiki.trapexit.org/index.php/RandomShuffle">http://wiki.trapexit.org/index.php/RandomShuffle</a>; refactored for clarity, and unnecessary repeat nesting behavior removed.</i>

%% @since Version 8

shuffle(List) ->
   WeightedAndShuffled        = lists:map( fun(Item) -> { random:uniform(), Item } end, List ),
   { _, SortedAndDeweighted } = lists:unzip(lists:keysort(1, WeightedAndShuffled)),
   SortedAndDeweighted.





% Handler must be no_handler_pid or { handler, PID [, idhandle] }

call_after_worker(MS, Func, Args, Handler) ->

    receive after MS ->

        case Func of
            { Module, FuncName } -> Result = apply(Module, FuncName, Args);
            FuncName             -> Result = apply(FuncName, Args)
        end,

        case Handler of
            { handler, PID, Handle } -> PID ! { call_after_result, Result, Handle };
            { handler, PID }         -> PID ! { call_after_result, Result };
            no_handler_pid           -> ok
        end

    end.





call_after(Length, Func)                -> call_after(Length, Func, [],   no_handler_pid).
call_after(Length, Func, Args)          -> call_after(Length, Func, Args, no_handler_pid).
call_after(Length, Func, Args, Handler) ->

    Worker = spawn(?MODULE, call_after_worker, [Length, Func, Args, Handler]),
    { ok, spawned_worker, Worker }.





permute(List) -> 
    permute(List, length(List)).

permute(List, 1)     when is_list(List)                    -> [ {T}                        || T <- List ];
permute(List, Depth) when is_list(List), is_integer(Depth) -> [ erlang:append_element(R,T) || T <- List, R <- permute(List--[T], Depth-1) ].





has_bit(Num, Bit) when is_integer(Num), is_integer(Bit), Num > 0, Bit >= 0, Bit < 64 -> (Num band (1 bsl Bit)) > 0.





count_bits(Num) when is_integer(Num), Num > 0 ->

    length([S || S <- lists:seq(0,63), has_bit(Num, S) == true]).





diff_timestamp({AM,AS,AU}, {BM, BS, BU}) ->

    ((BM-AM) * 1000000) + (BS-AS) + ((BU-AU)/1000000).





benchmark(Module, Func, Args) ->

    Start  = now(),
    Result = apply(Module, Func, Args),
    End    = now(),

    { diff_timestamp(Start,End), Result }.





expand_label({Label,List}) when is_list(List) -> [ {Label,L} || L<-List ];
expand_label({Label,Item})                    -> {Label, Item}.

expand_labels(List)        when is_list(List) -> lists:flatten([ expand_label(X) || X <- List ]).





% Thanks for some math help on erl-b, erl-c and engset, Vat and Wintermute

erlang_b_distribution(N,A) ->

   Num = math:pow(A,N) / scutil:factorial(N),
   Denom = lists:sum([ math:pow(A,I) / scutil:factorial(I) || I <- lists:seq(0,N) ]),

   Num / Denom.





erlang_c_distribution(N,A) ->

   Num = (math:pow(A,N) / scutil:factorial(N)) * (N/(N-A)),
   Denom = lists:sum([ math:pow(A,I) / scutil:factorial(I) || I <- lists:seq(0,N-1) ]) + ((math:pow(A,N)/scutil:factorial(N))*(N/(N-A))),

   {wait_probability, Num / Denom}.





% thanks for a much better implementation, etnt
implode(Separator, Data) when is_list(Data) andalso is_list(Separator) -> lists:foldr(fun(Item,[]) -> Item; (Item,Acc) -> Item ++ Separator ++ Acc end, "", Data).





% Thank god they called it rem.  -5 mod 2 is 1, not -1.  Most CPUs implement remainder and call it modulus.
% This is real modulus.
mod(Base, Range) when is_integer(Base), is_integer(Range) ->

    case Base rem Range of
        X when X < 0 -> X + Range;
        Z            -> Z
    end.





% Just:
%   1) put    -svn_revision("$Revision$").    in your code,
%   2) set the svn property svn:keywords to include at least Revision (case sensitive), and
%   3) check in the code.
% Magic follows.

scan_svn_revision(Module) ->

    "$Revision: " ++ X = get_module_attribute(Module, svn_revision),
    [ Head | _Rem ]    = string:tokens(X, " "),
    list_to_integer(Head).





%  Thanks Table:
%
%    Alain O'Dea                   http://concise-software.blogspot.com/
%    Ayrnieu
%    Bryon Vandiver / Asterick     http://sublab.net/
%    Chile
%    Dave Murphy / Wintermute      http://devkitpro.org/
%    DizzyD
%    Dylan Barrie / PfhorSlayer
%    Etnt
%    GrizzlyAdams                  http://grizzly.thewaffleiron.net/
%    Jeff Katz / Kraln             http://kraln.com/
%    John Sensebe
%    Raleigh
%    Toby Opferman                 http://www.opferman.com/
%    Vat Raghavan                  http://www.blueventhorizon.com/   (that missing e is correct)
