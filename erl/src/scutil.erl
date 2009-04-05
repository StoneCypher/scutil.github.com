
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
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date$</tt></span>
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

    compile_all/1,       % not finished   % needs tests
    install/1,           % not finished   % needs tests
    verify_install/0,    % not finished   % needs tests

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
