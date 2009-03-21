
%%%%%%%%%%
%%
%%  ScUtil
%%  ------
%%
%%  StoneCypher's erlang utility library.
%%
%%  Code starts several hundred lines down; search for -module.  EDoc requires a lot of the documentation to be at the beginning of the file.
%%
%%  To automatically generate documentation, from the erlang shell, type:
%%    edoc:application(scutil, "/projects/libraries/erlang/scutil/src", [{dir,"/projects/libraries/erlang/scutil/doc"}]).
%%  Replace those paths with paths appropriate for your machine.





%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since September 14, 2007

%% @doc <p>ScUtil is StoneCypher's Utility Library, a collection of hundreds of routines of a variety of topics which have aggregated from reuse in other projects.</p>
%%
%% <ul>
%%   <li>{@section Conversion}</li>
%%   <li>{@section Counters}</li>
%%   <li>{@section Dispatch}</li>
%%   <li>{@section Documentary}</li>
%%   <li>{@section List}</li>
%%   <li>{@section Math}</li>
%%   <li>{@section Network}</li>
%%   <li>{@section Parallelism}</li>
%%   <li>{@section Persistence}</li>
%%   <li>{@section Probability}</li>
%%   <li>{@section Random}</li>
%%   <li>{@section Regex}</li>
%%   <li>{@section Serialism}</li>
%%   <li>{@section Statistics}</li>
%%   <li>{@section String} and</li>
%%   <li>{@section Utility} routines.</li>
%% </ul> <style type="text/css">h2,h3 { margin-top:6em; } h3.typedecl { margin-top:3em; }</style>
%%
%% <p>
%%   This file has aggregated
%%   dozens of useful miscellaneous routines which I'm releasing to the public in good faith.  There's no particular direction to
%%   this library; any time I write a routine that I tend to use in a lot of situations, which isn't already meaningfully
%%   classifiable into one of my other libraries, I throw it in here.  This has ended up creating a range of unrelated
%%   functionality on which most of my other libraries depend heavily.  Have fun digging around.
%% </p>
%%
%% == Routines by Category ==
%%
%% The most common way for people to find what they need.
%%
%% <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% === Conversion ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for converting between basic types, annotated types and user types<br/>
%%     {@link hex_to_int/1}, {@link byte_to_hex/1}, {@link nybble_to_hex/1}, {@link io_list_to_hex/1}, {@link list_to_number/1}, {@link int_to_u32_iolist/1}, {@link u32_iolist_to_int/1}, {@link u32_iolist_to_int/4}, {@link int_to_u64_iolist/1}, {@link u64_iolist_to_int/1}, {@link u64_iolist_to_int/8}
%%   </dd>
%% </dl>
%% === Counters ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for tracking counters shared between processes<br/>
%%     {@link counter/1}, {@link counters/1}, {@link reset_counter/1}, {@link inc_counter/1}, {@link inc_counter/2}, {@link dec_counter/1}, {@link dec_counter/2}, {@link adjust_counter/2}, {@link set_counter/2}
%%   </dd>
%% </dl>
%% === Dispatch ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for managing send/receive patterns between processes and process sets<br/>
%%      {@link call_after/2}, {@link call_after/3}, {@link call_after/4}
%%   </dd>
%% </dl>
%% === Documentary ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines whose purpose is to clarify dependant code by presenting their name, rather than routine behavior, as well as to establish standard result messages where appropriate<br/>
%%     {@link even_or_odd/1}, {@link absolute_difference/2}, {@link receive_one/0}, {@link start_register_if_not_running/3}, {@link start_register_if_not_running/4}, {@link start_register_if_not_running/5} (see also {@link square/1}, {@link cube/1})
%%   </dd>
%% </dl>
%% === List ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for operating on lists of data augmenting the standard lists module.<br/>
%%     {@link permute/1}, {@link combinations/2}, {@link shuffle/1}, {@link sanitize_tokens/2}, {@link shared_keys/1}, {@link shared_keys/2}, {@link shared_keys/3}, {@link all_unique_pairings/1}, {@link walk_unique_pairings/2}, {@link zip_n/1}, {@link count_of/2} (see also {@link random_from/3}, {@link random_from_weighted/1})
%%   </dd>
%% </dl>
%% === Math ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for higher math computation missing from the standard math module.<br/>
%%     {@link list_product/1}, {@link dot_product/2}, {@link cross_product/2}, {@link vector_magnitude/1}, {@link normalize_vector/1}, {@link root_mean_square/1}, {@link root_sum_square/1}, {@link tuple_sum/1}, {@link square/1}, {@link cube/1}, {@link mod/2}
%%   </dd>
%% </dl>
%% === Network ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for network behaviors and management.<br/>
%%     {@link standard_listener/3}
%%   </dd>
%% </dl>
%% === Parallelism ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for manipulating behavior across sets of parallel processes.<br/>
%%     {@link map_reduce/2}, {@link map_reduce/3}, {@link map_reduce/4}
%%   </dd>
%% </dl>
%% === Persistence ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to simplify and automate the storage of local information.<br/>
%%     {@link make_notebook/1}, {@link remove_notebook/1}, {@link has_notebook/1}, {@link annote/3}, {@link read_note/2}, {@link has_note/2}, {@link remove_note/2}
%%   </dd>
%% </dl>
%% === Probability ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to calculate the likelihoods of things.<br/>
%%     {@link bayes_likelihood_of/3} (see also {@link count_of/2}, {@link histograph/1})
%%   </dd>
%% </dl>
%% === Random ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to provide complex pseudorandom services and convenience wrappers to augment the standard random module.<br/>
%%     {@link grid_scatter/2}, {@link srand/0}, {@link srand/3}, {@link rand/1}, {@link random_from/1}, {@link random_from/2}, {@link random_from/3}, {@link random_from_weighted/1} (see also {@link shuffle/1})
%%   </dd>
%% </dl>
%% === Regex ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to provide a convenience wrapper for the standard regex module.<br/>
%%     {@link regex_read_matches/2}, {@link regex_read_matches/3}
%%   </dd>
%% </dl>
%% === Serialism ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines for manipulating behavior in repeat series within a single process.<br/>
%%     {@link multi_do/3}, {@link multi_do/4}
%%   </dd>
%% </dl>
%% === Statistics ===
%% <dl>
%%   <dt>Means</dt>
%%   <dd>
%%     Routines for finding various kinds of mean values for numeric lists<br/>
%%     {@link arithmetic_mean/1}, {@link geometric_mean/1}, {@link harmonic_mean/1}, {@link weighted_arithmetic_mean/1}, {@link arithmetic_mean/1}, {@link centroid/1}
%%   </dd>
%%   <dt>Descriptive</dt>
%%   <dd>
%%     Routines which provide informative measurements of numeric lists<br/>
%%     {@link median/1}, {@link mode/1}, {@link histograph/1}, {@link std_deviation/1}, {@link median_absolute_deviation/1}, {@link moment/2}, {@link moments/1}, {@link moments/2}, {@link central_moment/2}, {@link central_moments/1}, {@link central_moments/2}, {@link skewness/1}, {@link kurtosis/1} (see also {@link count_of/2}, {@link root_mean_square/1})
%%   </dd>
%%   <dt>Normals</dt>
%%   <dd>
%%     Routines to calculate the various normals of vectors<br/>
%%     {@link amean_vector_normal/1}, {@link gmean_vector_normal/1}, {@link hmean_vector_normal/1}
%%   </dd>
%%   <dt>Ranking</dt>
%%   <dd>
%%     Routines to provide statistical rankings of lists<br/>
%%     {@link ranks_of/1}, {@link tied_ranks_of/1}, {@link ordered_ranks_of/1}
%%   </dd>
%%   <dt>Correlations</dt>
%%   <dd>
%%     Routines to measure the statistical correlations between two numeric lists<br/>
%%     {@link kendall_correlation/1}, {@link kendall_correlation/2}, {@link pearson_correlation/1}, {@link pearson_correlation/2}, {@link spearman_correlation/1}, {@link spearman_correlation/2}
%%   </dd>
%% </dl>
%% === String ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines to augment the existing string module.  All scutil string routines are safe for widechar lists.<br/>
%%     {@link sanitize_filename/1}, {@link to_lines/1}
%%   </dd>
%% </dl>
%% === Utility ===
%% <dl>
%%   <dt></dt>
%%   <dd>
%%     Routines which don't classify well into larger categories<br/>
%%     {@link type_of/1}, {@link get_module_attribute/1}, {@link get_module_attribute/2}, {@link scan_svn_revision/1}
%%   </dd>
%% </dl>
%%
%% == License ==
%% <p>ScUtil is MIT license, because <a href="http://WhyIHateTheGPL.com/">the author feels very strongly against the GPL</a>.</p>
%%
%% == Registration ==
%% <p>ScUtil is free and may be used at discretion.  However, the author would appreciate three things if you have time:</p>
%% <table><tr><td><script type="text/javascript" src="http://www.ohloh.net/p/316896/widgets/project_users.js?style=blue"></script></td><td><ol><li>Putting a link to the main domain <a style="font-family:consolas,monospace" href="http://scutil.com/">http://scutil.com/</a> on your site(s) to help get the word out</li><li style="margin-top: 0.75em;">Marking yourself a user of the library on <a href="https://www.ohloh.net/p/scutil">ohloh</a> (<i style="color:#369;">press the button to the left if you're already a user</i>), to help get the word out</li><li style="margin-top: 0.75em;">Dropping me an email at <a style="font-family:consolas,monospace" href="mailto:stonecypher@gmail.com">stonecypher@gmail.com</a>, to let me know where my library has ended up.  It's interesting.</li></ol></td></tr></table>
%%
%% == Thanks ==
%% <p>ScUtil has profited significantly from the donations of time, understanding and code given by a variety of generous friends and strangers.  The list of small tweaks would
%%    be prohibitive, but significant influence on this library is due the following people, in alphabetical order (the least fair of all generic orderings):</p>
%%
%% <ul>
%%   <li>Alain O'Dea of <a href="http://concise-software.blogspot.com/" target="_blank">Concise Software</a></li>
%%   <li>Alisdair Sullivan</li>
%%   <li>Aristid Breitkreuz / [MrN, MisterN]</li>
%%   <li>Ayrnieu</li>
%%   <li>Bryon Vandiver of <a href="http://sublab.net/" target="_blank">Sublab Research and Design</a></li>
%%   <li>Chile</li>
%%   <li>Dave Murphy / <a href="http://devkitpro.org/" target="_blank">WinterMute</a></li>
%%   <li>DizzyD</li>
%%   <li>Dylan Barrie / PhforSlayer</li>
%%   <li>Geoff Cant / <a href="http://github.com/archaelus">Archaelus</a></li>
%%   <li>GrizzlyAdams of <a href="http://grizzly.thewaffleiron.net/" target="_blank">The Waffle Iron</a></li>
%%   <li>Jeff Katz / <a href="http://kraln.com/" target="_blank">Kraln</a></li>
%%   <li>John Sensebe of <a href="http://bargaintuan.com/" target="_blank">Bargaintuan</a></li>
%%   <li>raleigh</li>
%%   <li><a href="http://hem.bredband.net/richardc/">Richard Carlsson</a></li>
%%   <li><a href="http://rvirding.blogspot.com/" target="_blank">Robert Virding</a></li>
%%   <li><a href="http://akkit.org/" target="_blank">Steve Stair</a></li>
%%   <li><a href="http://steve.vinoski.net/">Steve Vinoski</a></li>
%%   <li>Torbj*rn T*rnkvist (those asterisks are o-umlauts, until I work out a bug in edoc) / <a href="http://github.com/etnt">Etnt</a></li>
%%   <li><a href="http://opferman.com/" target="_blank">Toby Opferman</a></li>
%%   <li><a href="http://blueventhorizon.com/" target="_blank">Vat Raghavan</a></li>
%%   <li>Vladimir Sessikov</li>
%% </ul>
%%
%% @end

%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Direct link to zip archive</span><a href="http://crunchyd.com/release/scutil.zip">Current version</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Public SVN at</span><a href="svn://crunchyd.com/scutil/">svn://crunchyd.com/scutil/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Discussion forum at</span><a href="http://crunchyd.com/forum/scutil-discussion/">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Bugtracker at</span><a href="http://crunchyd.com/forum/project.php?projectid=7">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-03-15 13:47:01 -0600 (Sun, 15 Mar 2009) $</tt></span>
%% @reference <span style="margin-top:1em;padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Test sets require min. version 16</span><a href="http://testerl.com/">TestErl</a></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add warnings re: spearman, pearson, kendall use on lists containing repetitions
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





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

-library_requirements([
    {dq,      141},
    {testerl, 16}
]).





-export( [

    type_of/1,

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
    root_mean_square/1, root_sum_square/1, % needs tests
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

    shared_keys/1, shared_keys/2, shared_keys/3, % needs tests
    all_unique_pairings/1, % needs tests
    walk_unique_pairings/2, % needs tests
    list_to_number/1, % needs tests
    counter/1, counters/1, inc_counter/1, inc_counter/2, dec_counter/1, dec_counter/2, reset_counter/1, adjust_counter/2, set_counter/2, counter_process/0, % needs tests
    start_register_if_not_running/3, start_register_if_not_running/4, start_register_if_not_running/5, % needs tests
    wait_until_terminate/0, wait_until_terminate/1, % needs tests
    module_has_function/2, % needs tests

    call_after/2, call_after/3, call_after/4, call_after_worker/4, % needs tests
    shuffle/1, % needs tests

    permute/1, permute/2, % needs tests

    has_bit/2, count_bits/1, % needs tests
    diff_timestamp/2, % needs tests

    expand_label/1, expand_labels/1, % needs tests

    benchmark/2, benchmark/3, % needs tests

    hex_to_int/1, % needs tests

    erlang_b_distribution/2, % needs tests
    erlang_c_distribution/2, % needs tests

    % exponential_distribution, poisson_distribution, gamma_distribution, phase_type_distribution, compound_poisson_distribution
    % engset_calculation

    implode/2, % needs tests

    mod/2, % needs tests

    scan_svn_revision/1, % needs tests

    median_absolute_deviation/1, % needs tests

    make_notebook/1,    % needs tests
    remove_notebook/1,  % needs tests
    has_notebook/1,     % needs tests
    annote/2, annote/3, % needs tests
    read_note/2,        % needs tests
    has_note/2,         % needs tests
    remove_note/2,      % needs tests

    tuple_sum/1, % needs tests

    map_reduce/2, map_reduce/3, map_reduce/4, % needs tests
    combinations/2, % needs tests

    standard_listener/3, standard_listener_accept_loop/6, standard_listener_shunt/5, standard_listener_controller/6, % needs tests

    int_to_u32_iolist/1, int_to_u32_iolist/2, % needs tests
    u32_iolist_to_int/1, u32_iolist_to_int/2, u32_iolist_to_int/4, u32_iolist_to_int/5, % needs tests

    int_to_u64_iolist/1, int_to_u64_iolist/2, % needs tests
    u64_iolist_to_int/1, u64_iolist_to_int/2, u64_iolist_to_int/8, u64_iolist_to_int/9, % needs tests

    float_to_f32_iolist/1, float_to_f32_iolist/2, % needs tests
    f32_iolist_to_int/1, f32_iolist_to_int/2, f32_iolist_to_int/4, f32_iolist_to_int/5, % needs tests

    zip_n/1, zip_n/2, % needs tests

    centroid/1, % needs tests

    square/1, cube/1, % needs tests

    euclidean_distance/2, % manhattan_distance chebyshev_distance minkowski_distance mahalanobis_distance hamming_distance % needs tests

    bayes_likelihood_of/3, % needs tests

    count_of/2, % needs tests

    list_intersection/2, list_intersection/3, % needs tests

%   group_by_distance/2, % needs tests

    tuple_member/2, record_member/2, % needs tests

    every_member_representation/1, every_member_representation/2, % needs tests

    every_flag_representation/1, % needs tests

    isolate_signal/1, unit_scale_signal/1, minmax/1, % needs tests

    flesch_kincaid_readability/4, flesch_kincaid_readability_score/3, interpret_flesch_kincaid_score/1, % needs tests

    halstead_complexity/4, halstead_complexity/5, % needs tests

    eval/1, eval/2, % needs tests

    gen_docs/1, % needs tests

    factorial/1, % needs tests

    list_rotate/2, index_of_first/2, rotate_to_first/2, rotate_first_to_end/2, % needs tests

    columnated_rows/2, columns/2, columnate/1, columnate/2, columnated_text/2, % needs tests
    first_row/1, first_or_nothing/1, % needs tests

    floor/1, ceil/1, ceiling/1, % needs tests

    get_linked_processes/0, % needs tests

    key_duplicate/1, % needs tests

    mersenne_prime/1, % needs tests

    levenshtein/2, % needs tests

    make_node/2, % needs tests
    
    map_scanline/2, map_scanline/3, % needs tests
    
    list_to_term/1, % needs tests

    is_numeric_char/1, is_numeric_char/2,     % needs tests
    is_numeric_string/1, is_numeric_string/2, % needs tests
    
    explode/2, explode/3, % needs tests

    starts_with/2, % needs tests

    extrema_of/1, % needs tests
    
    expected_value/1

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





%% @type hexchar() = integer().  Integer must be in the range $0 - $9, the range $a - $f, or the range $A - $F, all inclusive, for inputs; outputs will always use lower case.
%% @type hexstring() = list().  All elements of the list must be of type {@type hexchar()}.

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

hex_to_int(Hex) when is_list(Hex) -> 
    hex_to_int(Hex, 0).

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

byte_to_hex(TheByte) when is_integer(TheByte), TheByte >= 0, TheByte =< 255 -> 

    [ nybble_to_hex(TheByte bsr 4), nybble_to_hex(TheByte band 15) ].





%% @type nybble() = integer().  A nybble must be an integer in the range 0-15, inclusive.

%% @spec nybble_to_hex(Nyb::nybble()) -> integer()

%% @doc {@section Conversion} Convert a nybble() to a hexchar(). ```1> scutil:nybble_to_hex(7).
%% 55
%%
%% 2> scutil:nybble_to_hex(15).
%% 102'''

%% @since Version 19

nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 0,  Nyb < 10 ->

    $0 + Nyb;
    


nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 10, Nyb < 16 ->

    $a + Nyb - 10.





%% @type io_list() = list().  Every list member of an {@type io_list()} must be a {@type byte()}.

%% @spec io_list_to_hex(Input::io_list()) -> hexstring()

%% @doc {@section Conversion} Convert an io_list() to a hexstring().  ```1> scutil:io_list_to_hex("a").
%% "61"
%%
%% 2> scutil:io_list_to_hex("a08n408nbqa").
%% "6130386e3430386e627161"'''

%% @since Version 19

io_list_to_hex(Input) when is_list(Input) ->

    io_list_to_hex(Input, []).



io_list_to_hex([], Work) -> 

    lists:reverse(Work);
    


io_list_to_hex([Item|Remainder], Work) when is_integer(Item), Item >= 0, Item =< 255 -> 

    [A,B] = byte_to_hex(Item), 
    io_list_to_hex(Remainder, [B,A]++Work);
    


io_list_to_hex(_, _) ->

    {error, not_an_io_list}.





%% @equiv multi_do(C,M,F,[])
%% @since Version 38
multi_do(C, Module, Func) ->
   
    multi_do(C, Module, Func, [],   []).



%% @spec multi_do(Count::integer(), Module::atom(), Function::atom(), Args::list()) -> list()

%% @doc {@section Serialism} Take an iteration count, a module name, a function name and an argument list, and repeatedly apply the argument list to the module/function, count times.  This is primarily useful with nondeterministic functions whose result might change despite identical arguments, such as functions with random behavior; for example, this function is invoked to implement stochastic testing in <a href="http://testerl.com/">TestErl</a>. ```1> scutil:multi_do(10, scutil, rand, [100]).
%% [9,94,4,82,77,44,89,19,45,92]
%%
%% 2> scutil:multi_do(10, scutil, rand, [10000]).
%% [2377,2559,1713,8489,4468,3261,3344,3751,380,2525]'''

%% @since Version 38
multi_do(C, Module, Func, Args) ->
    
    multi_do(C, Module, Func, Args, []).



multi_do(0,_Module,_Func,_Args, Work) -> 

    Work;
    
    

multi_do(I, Module, Func, Args, Work) -> 
    
    multi_do(I-1, Module, Func, Args, Work ++ [apply(Module, Func, Args)]).





%% @equiv regex_read_matches(String, Reg, {0,0})
%% @since Version 41

regex_read_matches(String, Reg) ->

    regex_read_matches(String, Reg, {0,0}).




%% @equiv regex_read_matches(String, Reg, {TrimFront,TrimLength})
%% @since Version 41

regex_read_matches(String, Reg, TrimFront, TrimLength) ->

    regex_read_matches(String, Reg, {TrimFront, TrimLength}).



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

        { match, Matches } ->
            [ string:substr(String, Start+TrimFront, End-(TrimLength+1)) || {Start,End} <- Matches ];

        { error, E } ->
            { error, E }

    end.





%% @type gridsize() = coord2() | integer().  Coordinates are the width and height of a (1,1) originated grid; as such, coordinates are of the range [1,X] , [1,Y] inclusive, and returned in the form {A,B}.  The integer form implies a square grid.
%% @type coord() = tuple().  Every member of a {@type coord()} is a {@type number()}.  Represents a coordinate, which may imply a sized cartesian space.  Many functions expect integer coordinates; the type does not require them.  This type does not define member count.  If your function requires a specific count of members, name it, as in a {@type coord2()} or {@type coord3()}.
%% @type coordlist() = list().  All members of a {@type coordlist()} must be {@type coord()}s.  All member coordinates must be of the same size, though this type does not define what that size is.  If your function requires a specific count of members, name it, as in a {@type coord2list()} or {@type coord3list()}.
%% @type coord2() = { number(), number() }.  Represents a coordinate, which may imply a sized rectangle.  Many functions expect integer coordinates; the type does not require them.
%% @type coord2list() = list().  All members of a {@type coord2list()} must be {@type coord2()}s.
%% @type coord3() = { number(), number(), number() }.  Represents a coordinate, which may imply a sized 3d box region.  Many functions expect integer coordinates; the type does not require them.
%% @type coord3list() = list().  All members of a {@type coord3list()} must be {@type coord3()}s.

%% @spec grid_scatter(Count::integer(), Size::gridsize()) -> coordlist()

%% @doc {@section Random} Return a Count-length list of non-repeating coordinates in a grid of specified size; useful for feature generation.

%% @todo @comeback give code examples (edoc was failing here?)

%% @since Version 42

grid_scatter(0, []) -> []; % skips a lot of work



grid_scatter(Count, {SizeX, SizeY}) ->

    scutil:random_from(Count, [ {X,Y} || X <- lists:seq(1,SizeX), Y <- lists:seq(1,SizeY) ]);



grid_scatter(Count, Size) ->

    grid_scatter(Count, {Size, Size}).





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

        undefined ->
            ok;

        _Defined ->
            unregister(scutil_rand_source)  % todo fixme leak : this should notify the old rand_source that it is being discarded

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

random_from(List) ->

    [X] = random_from(1, List, no_remainder), X.



%% @equiv random_from(N, List, no_remainder)
%% @since Version 6

random_from(N, List) ->

    random_from(N, List, no_remainder).



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

random_from(N, List, no_remainder) ->

    {R,_} = random_from(N,List,remainder), R;



random_from(N, List, remainder) ->

    lists:split(N,shuffle(List)).





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

random_from_weighted_worker([], _) ->

    { error, limit_miscalculation };



% but if the list has reasonable contents and the limit is a pos-or-0 integer

random_from_weighted_worker(InputList, Limit) when is_list(InputList), is_integer(Limit), Limit >= 0 ->

    [ {Item,Weight} | Remainder ] = InputList,   % break off the input list's head as {I,W} and keep the rest as Remainder

    case Weight =< Limit of                                             % if the weight is less than or equal to the limit,

        true  ->
            random_from_weighted_worker(Remainder, Limit-Weight);       % recurse the next item with a decremented weight

        false ->
            Item                                                        % if not, this item is the one we want

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

        false ->
            elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx);

        { value, Tuple } ->
            elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx);

        AnythingElse ->
            { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }

    end.





elements_worker(Retlist, _,      [],        _,      strip) -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx, strip) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of

        false ->
            elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx, strip);

        { value, {_,Tuple} } ->
            elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx, strip);

        AnythingElse ->
            { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }

    end.





%% @spec even_or_odd(Num::integer()) -> even | odd

%% @doc {@section Documentary} Documentary convenience function that returns the atoms `even' or `odd' for any integer. ```1> scutil:even_or_odd(3).
%% odd'''

%% @since Version 8

even_or_odd(Num) when is_integer(Num), Num band 1 == 0 ->

    even;



even_or_odd(Num) when is_integer(Num) ->

    odd.






%start() ->
%
%    case gen_tcp:listen(25,[]) of
%
%        { ok, ListeningSocket } -> { ok, listening_on_pid, spawn(?MODULE, accept_loop, [ListeningSocket]) };
%        { error, E }            -> { error, E }
%
%    end.
%
%
%
%
%
%accept_loop(ListeningSocket) ->
%
%    case gen_tcp:accept(ListeningSocket) of
%
%        { ok, ConnectedSocket } ->
%            spawn(?MODULE, handler_loop, [ConnectedSocket]),
%            accept_loop(ListeningSocket);
%
%        { error, E } ->
%            accept_loop(ListeningSocket)
%
%    end.
%
%
%
%
%
%handler_loop(ConnectedSocket) ->
%
%    receive
%
%        terminate              -> ok;
%        { tcp, Socket, Input } -> gen_tcp:send(Socket, "You said " ++ Input ++ "\r\n"), handler_loop(ConnectedSocket);
%        { error, E }           -> { error, E }
%
%    end.





%% @todo TODO

% key_split(KeyId, TupleList)           when is_list(TupleList) -> key_split(KeyId, TupleList,                       unsorted).
% key_split(KeyId, TupleList, unsorted) when is_list(TupleList) -> key_split(KeyId, lists:keysort(KeyId, TupleList), sorted);
% key_split(KeyId, TupleList, sorted)   when is_list(TupleList) ->

% key_minimum(
% key_maximum(





%% @since Version 152

gen_docs( [From, To] ) ->

    edoc:application(scutil, From, [{dir,To},{new,true}] ).




% todo invert this so that it returns {currentcount, fun, result} so that it can be continued
% generate(0, _) -> [];
% generate(N, Fun) when is_integer(N) andalso N > 0 andalso is_function(Fun) -> [Fun()] ++ generate(N-1,Fun).
