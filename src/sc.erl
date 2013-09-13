
%%%%%%%%%
%%
%%  Stonecypher's Erlang Utility Library - reboot 2011
%%  Written by John Haugeland, http://fullof.bs/
%%
%%  To skip to code, which is hundreds of lines down, search for "-module("
%%  without quotes.
%%
%%  This was last tested by the author in Erl OTP 14b1 / 5.8.1.1 .  Please
%%  run sc:test(deep) or sc:test([verbose,deep]) before using, to verify that
%%  this library functions correectly in the current Erlang virtual machine
%%  and environment.  Removing deep will execute a faster, less trustworthy
%%  subset of the tests.  Removing verbose will dump much less information to
%%  the console.
%%
%%  There is significant documentation.  With paths appropriate for your
%%  system, call sc:gen_docs("/path/to/source", "/path/for/docs/to/live")
%%  to generate.  Do not use trailing slashes.  Windows paths are fine;
%%  remember to use \\ , because it's a string and you're quoting the
%%  backslash.  Automatic documentation generation via edoc will then
%%  generate HTML docs.
%%
%%  For example, if you have this source in /projects/scutil or
%%  c:\projects\scutil, and you wanted the documentation in
%%  /project/scutil/erl/src/docs or c:\projects\scutil\docs
%%
%%  Past here, documentation should be generally be read in the HTML format.





%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since September 14, 2007

%% @todo burn out sc_
%% @todo burn out sc.
%% @todo burn out scutil:
%% @todo burn out @section
%% @todo burn out was
%% @todo distinguish -opaque types from -types (opaque are not meant to be understood by the outside world, merely tracked, eg handles)

%% @todo paranoid guards

%% @todo http://tidier.softlab.ntua.gr:20000/tidier/getstarted

%% @todo module attributes and documentation attributes for license name, license url
%% @todo Every documentation example should be enforced as a unit test, to keep the docs up to date
%% @todo Automate the version back into the docs
%% @todo Automate the version into the .app.src





%% @doc This is the 2011 revamp of the erlang portion of the scutil library.
%%
%% ScUtil (erlang and other languages) is available from <a href="http://scutil.com/" target="_blank">its own webpage</a>, or on <a href="https://github.com/StoneCypher/scutil.github.com" target="_blank">github</a>.  The erlang portion of scutil is a valid OTP application and rebar dependency.
%%
%% <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <a href="http://fullof.bs/">I'm</a> modernizing my library.  Originally it got too big, so I split it up.  However, that's proven more trouble than it's worth - I often found myself asking whether the thing I wanted was
%% in `_math', `_correlate', `_statistics', `_lists', `_operators', etc.  No more.  It's all getting dumped into `sc.erl' and `sc_tests.erl'.  Prototypes are pushed into `sc_prototype.erl'; that is
%% code which is either insufficiently tested or unfinished.  It's moving to eunit; sc_test is being dumped.  I'm taking a lot of the customization stuff out of the generated docs, too.
%% There was always an emphasis on testing, but it's getting taken much more seriously this time.  I am also no longer a language novice; I can replace at least some of the naive implementations
%% with marginally less naive ones, break fewer conventions, replicate fewer extant library functions, choose better naming and a more appropriate argument order, et cetera.
%%
%% This also means I can completely ditch all the remnants of the botched move to packages, provide radically less automation which is radically more effective, and generally do a better job of
%% presenting the actual volume of functionality present here.  We can get the coverage tools out.  We can run prototypical work in a single separate file, to distinguish quality.  Etc.
%%
%% Generally, this is intended to just be an all around improvement and replacement for the library I was pretty happy with, but which was showing strain.
%%
%% As always, this work remains open source under the MIT license, because free isn't real unless it's free for everyone.  <a href="http://scutil.com/license/">http://scutil.com/license/</a>
%%
%% ScUtil is free.  However, I'd like to know where it's ended up.  Therefore, please consider mail to stonecypher@gmail.com with text saying where this went a form of "registration."  This is not required.
%%
%% <a name="Thanks"><h2>Thanks</h2></a>
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
%%   <li>Essen</li>
%%   <li><a href="ferd.ca">Fred Hebert</a> / <a href="learnyousomeerlang.com">MononcQc</a></li>
%%   <li>Geoff Cant / <a href="http://github.com/archaelus">Archaelus</a></li>
%%   <li>GrizzlyAdams of <a href="http://grizzly.thewaffleiron.net/" target="_blank">The Waffle Iron</a></li>
%%   <li>Jeff Katz / <a href="http://kraln.com/" target="_blank">Kraln</a></li>
%%   <li>Jesper Louis Andersen / <a href="jlouisramblings.blogspot.com">jlouis</a></li>
%%   <li>John Sensebe of <a href="http://bargaintuan.com/" target="_blank">Bargaintuan</a></li>
%%   <li><a href="http://functional-orbitz.blogspot.com/" target="_blank">Orbitz</a></li>
%%   <li>raleigh</li>
%%   <li><a href="http://hem.bredband.net/richardc/">Richard Carlsson</a></li>
%%   <li><a href="http://rvirding.blogspot.com/" target="_blank">Robert Virding</a></li>
%%   <li><a href="http://akkit.org/" target="_blank">Steve Stair</a></li>
%%   <li><a href="http://steve.vinoski.net/">Steve Vinoski</a></li>
%%   <li>Torbj&#246;rn T&#246;rnkvist / <a href="http://github.com/etnt">Etnt</a></li>
%%   <li><a href="http://opferman.com/" target="_blank">Toby Opferman</a></li>
%%   <li><a href="http://blueventhorizon.com/" target="_blank">Vat Raghavan</a></li>
%%   <li>Vladimir Sessikov</li>
%% </ul>
%%
%% <h2>Rebar use</h2>
%%
%% Here is a valid dependency rule containing instructions to fetch current `sc' and the testing dependency `PropEr' for rebar:
%%
%% ```{deps, [ {proper, ".*", {git, "git://github.com/manopapad/proper.git",              "master"}},
%%             {sc,     ".*", {git, "git://github.com/StoneCypher/scutil.github.com.git", "master"}} ].'''
%%
%% @end





-module(sc).





-export([

    extrema/1,
    key_duplicate/1,
    index_of_first/2,  %% todo needs index of last then too, no?
    combinations/2,
    flag_sets/1,
    list_product/1,
    sanitize_tokens/2,
    caspers_jones_estimate/1,
    range_scale/1,
    absolute_difference/2,
    mod/2,
    mersenne_prime/1,
    factorize/1,
    centroid/1,
    nearest_to/2,
    svn_revision/1,
    module_atoms/1,
    key_cluster/2,
    split_at/2,
    is_postfix/2,
    walk_unique_pairings/2,
    count_of/2,
    integer_to_radix_list/2,
    receive_one/0,
    power_set/1,
    shuffle/1,
    record_member/2,
    get_linked_processes/0,
    starts_with/2,
    levenshtein/2,
    sanitize_filename/1,
    circ_within_origin_circ/2,
    circles_overlap/2,
    circles_contact/2,
    wglsh/1,
    terminate_loop/0,
    bin_to_hex_list/1,
    stretch_hash/3,
    null_postpad_bin_to/2,
    ascii_alphanum_list_subset/1,
    country_codes/0,
    random_unicode_char/0,
    file_to_binary_literal_as_string/1,
    months_as_short_atoms/0,
    module_is_loaded/1,
    replace/3,
    merge_settings/2,
    neighbors/2,
    markhov_chain/2,
    to_lines/1,
    isolate_waveform/1,
    unit_scale/1,
    type_of/1,
    cross_product/2,
    dot_product/2,
    vector_normalize/1,
    qsp_average/2,
    has_bit/2,
    count_bits/1,
    now_str_utc24/0,
    segment_size/1,
    parallelize/2,
    has_debug_info/1,
    ad_rate/5,
    grab_first/1,
    funnel/2,
    list_cross_multiply/1,
    outcomes/2,
    logb/2,

    unixtime/0,
      unixtime_daybase/0,
      unixtime_daybase/1,
      unixday/0,
      unixday/1,

    wang_carpet/2,
      wang_carpet/3,
      wang_such_that/2,
      wang_such_that/3,
      wang_row/1,
      wang_row/2,
      ms_wang/0,

    pred_rate/1,
      truth_density/1,

    ww/2,
%      word_wrap/3,

    solarized/0,
      solarized/1,

    bucket/2,

    key_bucket/1,
      key_bucket/2,

    unique_send_receive/3,
      unique_receive_respond/1,
      unique_receive_respond/2,

    naive_bayes_likelihood/4,
      naive_bayes_likelihood/5,

    ensure_started/1,
      ensure_started/2,

    notebook_validate/1,
      notebook_create/1,
      notebook_destroy/1,
      notebook_write/3,
      notebook_read/2,
      notebook_contains/2,
      notebook_remove/2,
      notebook_contents/1,

    time_diff/2,

    benchmark/1,
      benchmark/2,
      benchmark/3,

    is_numeric_char/1,
      is_numeric_char/2,

    columnated_text/2,
      columnated_rows/2,

    columnate/1,
      columnate/2,

    columns/2,
      first_row/1,

    standard_card_backs/0,
      standard_card_backs/1,

    multi_deck/2,

    fk_readability/3,
      labelled_fk_readability/1,
      fk_readability/4,

    unfunnel/2,
      unfunnel/3,

    is_between/3,
      is_between/4,

%   module_flow/1,     % module attributes can't be amongst the functions :(

    counter_at/1,
      set_counter_value/2,
      reset_counter/1,
      adjust_counter_by/2,
      inc_counter/1,
      inc_counter/2,
      dec_counter/1,
      dec_counter/2,

    ngrams/1,
      ngrams/2,

    probability_all/1,
      probability_any/1,

    in_range/3,
      out_of_range/3,

    even_or_odd/1,
      parity/1,

    hmac/3,
      hmac/4,
      hmac_md4/2,
      hmac_md5/2,
      hmac_sha1/2,

    factorial/1,
      additive_factorial/1,

    show/1,
      to_list/1,

    paper_3d_render/1,
      paper_3d_render/2,
      paper_3d_render/3,
      paper_3d_render/4,
      paper_3d_basic_depth/4,

    triangle_index/1,
      triangle_index/2,

    map_scanline/2,
      map_scanline/3,

    is_numeric_string/1,
      is_numeric_string/2,

    explode/2,
      explode/3,
      implode/2,

    multi_do/3,
      multi_do/4,

    start_register_if_not_running/2,
      start_register_if_not_running/3,
      start_register_if_not_running/4,
      start_register_if_not_running/5,

    tuple_sum/1,
      tuple_sort/1,
      tuple_member/2,
      tuple_duplicate/2,

    key_max/1,
      key_max/2,

    key_min/1,
      key_min/2,

    key_extrema/1,
      key_extrema/2,

    srand/0,
      srand/3,

    rand/1,
      is_rand_seeded/0,

    frand/0,
      frand_between/2,

    random_from/1,
      random_from/2,
      random_from/3,

    rand_between/2,

    random_from_weighted/1,

    grid_scatter/2,

    union/1,
      union/2,
      union/3,
      union/4,

    send_receive/2,
      send_receive/3,
      send_receive_masked/3,
      send_receive_masked/4,

    euclidean_distance/2,  % todo 3d version?

    list_to_term/1,
      list_to_number/1,

    io_list_to_hex_string/1,
      nybble_to_hex/1,
      byte_to_hex/1,
      hex_to_int/1,

    halstead_complexity/4,
      halstead_complexity/5,

    simple_ranking/1,
      tied_ranking/1,
      tied_ordered_ranking/1,

    spearman_correlation/1,
      spearman_correlation/2,

    pearson_correlation/1,
      pearson_correlation/2,

    kendall_correlation/1,
      kendall_correlation/2,

    eval/1,
      eval/2,

    every_member_representation/1,
      every_member_representation/2,
      every_flag_representation/1,

    zip_n/1,
      zip_n/2,

    expand_label/1,
      expand_labels/1,

    differences/1,
      first_difference/1,
      second_difference/1,
      third_difference/1,
      nth_difference/2,

%    find_difference_sequence/1,

    elements/2,
      elements/3,
      elements/4,

    reverse_map/2,
      reverse_filter/2,
      reverse_map_filter/3,

    key_group/2,
      key_group/3,

    first_pos/2,
      first_pos/3,

    last_while_pos/2,
      last_while_pos/3,

    partition_by_residue/2,

    all_neighbor_pairs/1,
      all_neighbor_pairs/2,

    distinct_neighbor_pairs/1,
      distinct_neighbor_pairs/2,

    module_abstract_representation/1,
      module_abstract_representation/2,
      abstract_attributes/1,

    entrypoints/1,
      entrypoints/2,

    abstract_function/2,
      abstract_functions/1,

    function_stats/1,
      function_point_count/1,

    module_attribute/1,
      module_attribute/2,

    module_feature/2,

    by_distance/2,
      by_distance_raw/2,

    alarm_set/3,
      alarm_terminate/1,

    is_sorted_list/1,
      is_unique_list/1,
      is_repeated_list/1,

    floor/1,
      ceil/1,
      ceiling/1,

    square/1,
      cube/1,
%     nth_root/2,    % comeback todo

    dstat/2,
      dstat_ex/2,

    moment/2,
      moments/1,
      moments/2,

    root_mean_square/1,
      root_sum_square/1,
      vector_magnitude/1,

    central_moment/2,
      central_moments/1,
      central_moments/2,

    histograph/1,
      histo_2d/1,

    skewness/1,
      kurtosis/1,

    median/1,
      mode/1,

    expected_value/1,

    median_absolute_deviation/1,
%     median_absolute_value/1,    comeback todo

    arithmetic_mean/1,
      geometric_mean/1,
      harmonic_mean/1,
      weighted_arithmetic_mean/1,

    amean_vector_normal/1,
      gmean_vector_normal/1,
      hmean_vector_normal/1,

    zipf_nearness/1,
      zipf_estimate_list/1,
      zipf_position_estimate/2,

    bandwidth_calc/1,
      bandwidth_calc/2,

    member_sets/1,
      member_sets/2,

    list_intersection/2,
      list_intersection/3,

    shared_keys/1,
      shared_keys/2,
      shared_keys/3,

    rotate_list/2,
      rotate_to_first/2,
      rotate_to_last/2,

    permute/1,
      permute/2,

    test/0,
      test/1,

    gen_docs/0,
      gen_docs/1,
      gen_docs/2,

    htget/1

]).





-export_type([

    grid_size/0,
    type_label/0,
    function_or_list/0,
    timestamp/0,

    hexchar/0,
      hexstring/0,

    keylist/0,
      sorted_keylist/0,

    list_of_lists/0,
      list_of_lists/1,
      list_of_tuples/0,
      list_of_tuples/1,
      list_of_2ary_tuples/0,

    filter_function/0,
      sanitizer/0,

    coord/0,
      coord_list/0,
      coord2/0,
      coord2_list/0,
      coord3/0,
      coord3_list/0,

    vector/0,
      vector/1,
      three_vector/1,
      three_vector_list/1,
      three_vector_tuple/1,
      seven_vector/1,
      seven_vector_list/1,
      seven_vector_tuple/1,
      three_or_seven_vector/1,
      unit_vector/0,
      vector_list/0,
      vector_list/1,

    ranking_list/0,
      ranking_list/1,
      ranking/0,
      ranking/1,

    string_list/0,
      numeric_list/0,
      pos_numeric_list/0,

    numeric_tuple/0,

    byte/0,
      io_list/0,

    weight_list/0,
      weight_list/1,
      weighted_value/0,
      weighted_value/1,

    list_or_binary/0,
      io_list_or_binary/0,

    bw_scale/0,
      bw_rate/0,

    time12/0,
      time24/0,
      time12s/0,
      time24s/0

]).





-include_lib("eunit/include/eunit.hrl").





-type time12()                   :: { 1..12, 1..60, am|pm }.               %% A human readable 12-hour time
-type time24()                   :: { 0..23, 1..60 }.                      %% A human readable 24-hour time

-type time12s()                  :: { 1..12, 1..60, 1..60, am|pm }.        %% A human readable 12-hour time, with seconds
-type time24s()                  :: { 0..23, 1..60, 1..60 }.               %% A human readable 24-hour time, with seconds

-type list_of_lists()            :: [[]].                                  %% Every member of a `list_of_lists()' is a `list()'.
-type list_of_lists(T)           :: [[ T ]].                               %% Every member of a `list_of_lists(T)' is a `list(T)'.
-type list_of_tuples()           :: [{}].                                  %% Every member of a `list_of_lists()' is a `tuple()'.
-type list_of_tuples(T)          :: [{ T }].                               %% Every member of a `list_of_lists(T)' is a `tuple(T)'.
-type list_of_2ary_tuples()      :: [ {any(),any()} ].

-type nybble()                   :: 0..15.                                 %% Integer must be in the range $0 - $9, the range $a - $f, or the range $A - $F, all inclusive, for inputs; outputs will always use lower case.  Synonym of hexchar().
-type hexchar()                  :: 0..15.                                 %% Integer must be in the range $0 - $9, the range $a - $f, or the range $A - $F, all inclusive, for inputs; outputs will always use lower case.  Synonym of nybble().
-type hexstring()                :: [ hexchar() ].                         %% All elements of the list must be of type {@type hexchar()}.

-type keylist()                  :: [{}].                                  %% All members of keylists are tuples of two-or-greater arity, and the first element is considered their key in the list.  List keys are unique; therefore `[{a,1},{b,1}]' is a keylist, but `[{a,1},{a,1}]' is not.
-type sorted_keylist()           :: keylist().                             %% A sorted keylist is a keylist in the order provided by {@link lists:sort/1}.  Because of erlang tuple ordering rules and the fact that keylist keys are unique, this means the list will be ordered by key.

-type filter_function()          :: function().                            %% Filter functions are 1ary binary predicates - they accept an argument and return either true or false.
-type sanitizer()                :: [] | filter_function().                %% Sanitizers are used by {@link sanitize_tokens/2} for input sanitization; they define what parts of an input list are valid, and the remainder are removed.  Sanitizers may either be a list of acceptable elements or a filter function.

-type coord()                    :: {}.                                    %% Every member of a {@type coord()} is a {@type number()}.  Represents a coordinate, which may imply a sized cartesian space.  Many functions expect integer coordinates; the type does not require them.  This type does not define member count.  If your function requires a specific count of members, name it, as in a {@type coord2()} or {@type coord3()}.
-type coord_list()               :: [{}].                                  %% All members of a {@type coord_list()} must be {@type coord()}s.  All member coordinates must be of the same size, though this type does not define what that size is.  If your function requires a specific count of members, name it, as in a {@type coord2_list()} or {@type coord3_list()}.
-type coord2()                   :: { number(), number() }.                %% Represents a coordinate, which may imply a sized rectangle.  Many functions expect integer coordinates; the type does not require them.
-type coord2_list()              :: [ coord2() ].                          %% All members of a {@type coord2_list()} must be {@type coord2()}s.
-type coord3()                   :: { number(), number(), number() }.      %% Represents a coordinate, which may imply a sized 3d box region.  Many functions expect integer coordinates; the type does not require them.
-type coord3_list()              :: [ coord3() ].                          %% All members of a {@type coord3_list()} must be {@type coord3()}s.

-type three_vector(T)            :: [T] | {T,T,T}.                         %% A three-vector always has three elements, so this can be expressed as the alternation `{A::T(), B::T(), C::T()} | [A::T(), B::T(), C::T()]'.
-type three_vector_list(T)       :: [T].                                   %% A three-vector expressed as a list.
-type three_vector_tuple(T)      :: {T,T,T}.                               %% A three-vector expressed as a tuple.
-type seven_vector(T)            :: [T] | {T,T,T,T,T,T,T}.                 %% A seven-vector always has seven elements, so this can be expressed as the alternation `{A::number(), B::number(), C::number(), D::number(), E::number(), F::number(), G::number()} | [A::number(), B::number(), C::number(), D::number(), E::number(), F::number(), G::number()]'.
-type seven_vector_list(T)       :: [T].                                   %% A seven-vector expressed as a list.
-type seven_vector_tuple(T)      :: {T,T,T,T,T,T,T}.                       %% A seven-vector expressed as a tuple.
-type three_or_seven_vector(T)   :: three_vector(T) | seven_vector(T).     %% A three or a seven-vector.
-type unit_vector()              :: vector().                              %% The hypoteneuse of a unit vector is precisely one unit long.  Unit vectors are also called normalized or magnitude-normalized vectors.
-type vector()                   :: [ number() ] | tuple(number()).        %% Every member element of a vector() is a {@type number()}.
-type vector(T)                  :: [ T ] | tuple(T).                      %% Every member element of a vector(T) is a T.
-type vector_list()              :: [ vector() ].                          %% Every member element of a vectorlist() is a {@type vector()}.
-type vector_list(T)             :: [ vector(T) ].                         %% Every member element of a vectorlist(T) is a {@type vector(T)}.

-type weighted_value()           :: { Value::any(), Weight::number() }.    %% Used by functions like weighted_arithmetic_mean/1 and from_weighted/1, weighted_value()s represent a value with an associated importance or "weight".
-type weighted_value(T)          :: { Value::T, Weight::number() }.        %% Used by functions like weighted_arithmetic_mean/1 and from_weighted/1, weighted_value()s represent a value with an associated importance or "weight".
-type weight_list()              :: [ weighted_value() ].                  %% All members of weightlists must be weighted_value()s.
-type weight_list(T)             :: [ weighted_value(T) ].                 %% All members of weightlists must be weighted_value(T)s.

-type ranking()                  :: { Ranking::number(), Value::any() }.   %% Values are usually {@type number()}s, but do not have to be with custom ranking predicates.
-type ranking(T)                 :: { Ranking::number(), Value::T }.       %% Values are usually {@type number()}s, but do not have to be with custom ranking predicates.
-type ranking_list()             :: [ ranking() ].                         %% Members of a {@type rankinglist()} must be {@type ranking()}s.
-type ranking_list(T)            :: [ ranking(T) ].                        %% Members of a {@type rankinglist()} must be {@type ranking()}s.

-type string_list()              :: [[]].                                  %% Every member of a stringlist() is a string().
-type io_list()                  :: [ byte() ].                            %% Every list member of an {@type io_list()} must be a {@type byte()}.
-type numeric_list()             :: [ number() ].                          %% All members of a numeric list must be number()s.
-type pos_numeric_list()         :: [ number() ].                          %% All members of a numeric list must be number()s.  Should enforce positive-ness but don't know how without losing floats.
-type numeric_tuple()            :: tuple(number()).                       %% Every member of a {@type numeric_tuple()} must be a {@type number()}.

-type list_or_binary()           :: list() | binary().                     %% It's either a {@type list()} or a {@type binary()}.  Duh.
-type io_list_or_binary()        :: io_list() | binary().                  %% It's either an {@type io_list()} or a {@type binary()}.  Duh.

-type bw_scale()                 :: { Unit::atom(), Timescale::atom() }.   %% `bw_scale' - Bandwidth scale - is a units-per-time notation for bandwidth measurement, eg `{megabits,day}'.
-type bw_rate()                  :: { Scale::bw_scale(), Rate::float() }.  %% `bw_rate' - Bandwidth rate - is a rate-in-units-per-time notation for bandwidth measurement, eg `{{megabits,day},10.5}'.

-type grid_size()                :: coord2() | integer().                  %% Coordinates are the width and height of a (1,1) originated grid; as such, coordinates are of the range [1,X] , [1,Y] inclusive, and returned in the form {A,B}.  The integer form implies a square grid.

-type type_label()               :: integer | float | list | tuple |
                                    binary | bitstring | boolean |
                                    function | pid | port | reference |
                                    atom | unknown.                        %% Used by type_of(), this is just any single item from the list of erlang's primitive types, or the atom <tt>unknown</tt>.

-type positive_integer_or_list() :: pos_integer() | list().                %% Positive integer or list.

-type function_or_list()         :: function() | list().
-type timestamp()                :: { Megaseconds::non_neg_integer(),
                                      Seconds::non_neg_integer(),
                                      MicroSeconds::non_neg_integer() }.   %% Erlang's timestamp type




%% @equiv gen_docs("/projects/github/scutil.github.com", "/projects/github/scutil.github.com/erl/doc")
%%
%% @doc (not testworthy) Generates library documentation using the paths appropriate for the author's PC; you almost certainly want {@link gen_docs/2} instead.  ```1> sc:gen_docs().
%% ok'''
%%
%% @since 458

-spec gen_docs() -> ok | { 'EXIT', any() }.

gen_docs() ->

    gen_docs("/projects/scutil.github.com", "/projects/scutil.github.com/doc/erl").





%% @doc (not testworthy) Generates library documentation using the default lib paths.  Target library should be specified <strong>without</strong> the trailing slash.  ```1> sc:gen_docs("/projects/scutil").
%% ok'''
%% @end
%%
%% @since 843

-spec gen_docs(_Tgt) -> ok | { 'EXIT', any() }.

gen_docs(Tgt) ->

    gen_docs(Tgt, Tgt ++ "/doc/erl").





%% @doc (not testworthy) Generates library documentation from and to the specified paths `WhereIsSrc' and `WhereToPutDocs' respectively.  Do not use trailing slashes.  Windows paths are okay; remember to double your
%% backslashes, as backslashes in strings need to be quoted.  ```1> sc:gen_docs("/projects/scutil", "/projects/scutil/erl/src/docs").
%% ok'''
%%
%% @since 458

-spec gen_docs(WhereIsSrc::string(), WhereToPutDocs::string()) -> ok | {'EXIT', any()}.

gen_docs(WhereIsBase, WhereToPutDocs) ->

    WhereIsSrc     = WhereIsBase ++ "/src",
    {ok,CMaj}      = file:read_file(WhereIsBase ++ "/major.version"),
    {ok,CMin}      = file:read_file(WhereIsBase ++ "/minor.version"),
    {ok,CVer}      = file:read_file(WhereIsBase ++ "/version.counter"),
    CurrentVersion = << CMaj/binary, <<".">>/binary, CMin/binary, <<".">>/binary, CVer/binary >>,

    DocTargets = ["sc", "sc_tests", "htstub", "htstub_tests", "htstub_adaptors", "htstub_adaptors_tests"],
    SrvTargets = ["hello_world", "restaurant"],

    filelib:ensure_dir(WhereToPutDocs),
    edoc:files([WhereIsSrc++"/"++DocFile++".erl"                || DocFile <- DocTargets ], [{new,true}, {dir, WhereToPutDocs}]),
    edoc:files([WhereIsSrc++"/htstub_servers/"++DocFile++".erl" || DocFile <- SrvTargets ], [{new,true}, {dir, WhereToPutDocs++"/htstub_servers"}]),

    VerFix = fun(Tgt) ->
      { ok, OldTextBin } = file:read_file(Tgt),
      file:write_file(Tgt, binary:replace(OldTextBin, <<"<p><b>Version:</b> $Revision$</p>">>, << <<"<p><b>Version:</b> ">>/binary, CurrentVersion/binary, <<"</p>">>/binary >>))
    end,

    [ VerFix(WhereToPutDocs ++ "/" ++ Doc ++ ".html") || Doc <- DocTargets ].





%% @doc (not testworthy) Runs the test suite in terse form. ```1> sc:test().
%%   All 9 tests passed.
%% ok'''
%%
%% @since 458

-spec test() -> ok | error.

test() ->

    eunit:test(sc).





%% @doc (not testworthy) Runs the test suite in verbose form.  Also responds to [verbose] to be more familiar to eunit devs.  An (ancient) example of output: ```1> sc:test(verbose).
%% ======================== EUnit ========================
%% module 'sc'
%%   module 'sc_tests'
%%     Index of first tests
%%       sc_tests:73: index_of_first_test_ (0,  [ ])...ok
%%       sc_tests:74: index_of_first_test_ (b,  [ a,b,c ])...ok
%%       sc_tests:75: index_of_first_test_ (g,  [ a,b,c ])...ok
%%       [done in 0.046 s]
%%     Rotate list tests
%%       sc_tests:52: rotate_list_test_ (0,  [ ])...ok
%%       sc_tests:53: rotate_list_test_ (1,  [ ])...ok
%%       sc_tests:54: rotate_list_test_ (-1, [ ])...ok
%%       sc_tests:56: rotate_list_test_ (0,  [ a,b,c ])...ok
%%       sc_tests:57: rotate_list_test_ (1,  [ a,b,c ])...ok
%%       sc_tests:58: rotate_list_test_ (-1, [ a,b,c ])...ok
%%       sc_tests:59: rotate_list_test_ (3,  [ a,b,c ])...ok
%%       sc_tests:60: rotate_list_test_ (-3, [ a,b,c ])...ok
%%       sc_tests:61: rotate_list_test_ (9,  [ a,b,c ])...ok
%%       [done in 0.141 s]
%%     Key duplicate tests
%%       sc_tests:38: key_duplicate_test_ ([ ])...ok
%%       sc_tests:39: key_duplicate_test_ ([ {2,a} ])...ok
%%       sc_tests:40: key_duplicate_test_ ([ {2,a},{3,b} ])...ok
%%       [done in 0.047 s]
%%     Extrema tests
%%       sc_tests:19: extrema_test_ (1,2,3,4)...ok
%%       sc_tests:20: extrema_test_ (-1,-2,-3)...ok
%%       sc_tests:21: extrema_test_ (-1.1,0,1.1)...ok
%%       sc_tests:22: extrema_test_ (a,b,c)...ok
%%       sc_tests:23: extrema_test_ (1,a,{})...ok
%%       sc_tests:24: extrema_test_ (1)...ok
%%       sc_tests:26: extrema_test_ ([] error)...ok
%%       [done in 0.109 s]
%%     [done in 0.343 s]
%%   [done in 0.343 s]
%% =======================================================
%%   All 22 tests passed.
%% ok'''
%%
%% @since 460

-spec test(verbose) -> ok | error.

test(verbose=_Style) ->

    eunit:test(sc, [verbose]);





test([verbose]=_Style) ->

    eunit:test(sc, [verbose]).






%% @doc <span style="color: green; font-weight: bold;">Tested</span> Returns the lowest and highest values in a list of one or more member in the form `{Lo,Hi}'.  Undefined over the empty list.  Mixed-type safe; sorts according to type order rules.  ```1> sc:extrema([1,2,3,4]).
%% {1,4}
%%
%% 2> sc:extrema([1,2,3,a,b,c]).
%% {1,c}'''
%%
%% 3> sc:extrema( [] ).
%% ** exception error: no function clause matching sc:extrema([])'''
%%
%% Unit, doc and stochastic (min and max are list members) tested.
%%
%% @since Version 460

-spec extrema(List::list()) -> { Low::any(), Hi::any() }.

extrema([First | _] = List)

    when is_list(List) ->

    Next = fun(Next,T) ->

        {Lo, Hi} = T,

        Lo2 = if
            Next < Lo -> Next;
            true      -> Lo
        end,

        Hi2 = if
            Next > Hi -> Next;
            true      -> Hi
        end,

        {Lo2, Hi2}

    end,

    lists:foldl(Next, {First,First}, List).





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Iterates a list of `{Count,Term}', producing a list of `[Term,Term,...]'.  ```1> sc:key_duplicate([ {3,bork} ]).
%% [bork,bork,bork]
%%
%% 2> sc:key_duplicate([ {3,sunday}, {2,monster}, {2,truck}, {1,'MADNESS'} ]).
%% [sunday,sunday,sunday,monster,monster,truck,truck,'MADNESS']'''
%%
%% Unit, doc, spec and stochastic (correct length) tested.
%%
%% @since Version 462

-spec key_duplicate(KeyList::list({non_neg_integer(),any()})) -> [any()].

key_duplicate(KeyList) ->

    lists:append( [ lists:duplicate(Key, Value) || {Key,Value} <- KeyList ] ).





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Rotates the front `Distance' elements of a list to the back, in order.  Negative distances rotate the back towards the front.  Distances over the length of
%% the list wrap in modulus.  ```1> sc:rotate_list(2, [1,2,3,4,5,6,7,8]).
%% [3,4,5,6,7,8,1,2]
%%
%% 2> sc:rotate_list(-2, [1,2,3,4,5,6,7,8]).
%% [7,8,1,2,3,4,5,6]
%%
%% 3> sc:rotate_list(0, [1,2,3,4,5,6,7,8]).
%% [1,2,3,4,5,6,7,8]
%%
%% 4> sc:rotate_list(16, [1,2,3,4,5,6,7,8]).
%% [1,2,3,4,5,6,7,8]'''
%%
%% @since Version 463

-spec rotate_list(Distance::integer(), ListData::list()) -> list().

rotate_list(_, []) ->

    [];





rotate_list(0, List) ->

    List;





rotate_list(By, List)

    when By =< (-(length(List))) ->

    rotate_list(By rem length(List), List);





rotate_list(By, List)

    when By < 0 ->

    rotate_list(length(List) + By, List);





rotate_list(By, List)

    when By >= length(List) ->

    rotate_list(By rem length(List), List);



rotate_list(By, List) ->

    { Front, Rear } = lists:split(By, List),

    Rear ++ Front.





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Returns the index of the first instance of `Item' in the `List', or `undefined' if `Item' is not present.  ```1> sc:index_of_first(c, [a,b,c,d,e]).
%% 3
%%
%% 2> sc:index_of_first(j, [a,b,c,d,e]).
%% undefined'''
%%
%% @since Version 463

-spec index_of_first(Item::any(), List::list()) -> integer() | undefined.

index_of_first(Item, List) ->

    index_of_first(Item, List, 1).





index_of_first(_Item, [], _Pos) ->

    undefined;





index_of_first(Item, [Item|_ListRem], Pos) ->

    Pos;





index_of_first(Item, [_OtherItem|ListRem], Pos) ->

    index_of_first(Item, ListRem, Pos+1).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Rotates the list to the first instance of Item.  ```1> sc:rotate_to_first(c, [a,b,c,d,e]).
%% [c,d,e,a,b]
%%
%% 2> sc:rotate_to_first(j, [a,b,c,d,e]).
%% no_such_element'''
%%
%% @since Version 464

-spec rotate_to_first(Item::any(), List::list()) -> list().

rotate_to_first(Item, List) ->

    case index_of_first(Item, List) of

        undefined ->
            no_such_element;

        IoF ->
            rotate_list(IoF-1, List)

    end.





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Rotates the list so that the first instance of Item becomes the last element in the list.  ```1> sc:rotate_to_last(c, [a,b,c,d,e]).
%% [d,e,a,b,c]
%%
%% 2> sc:rotate_list(j, [a,b,c,d,e]).
%% no_such_element'''
%%
%% @since Version 464

-spec rotate_to_last(Item::any(), List::list()) -> list().

rotate_to_last(Item, List) ->

    case index_of_first(Item, List) of

        undefined ->
            no_such_element;

        IoF ->
            rotate_list(IoF, List)

    end.





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Returns every interpretation of the list as a set of boolean flags, including all-off and all-on. ```1> sc:flag_sets([1,2,3,4]).
%% [ [], [4], [3], [3,4], [2], [2,4], [2,3], [2,3,4], [1], [1,4], [1,3], [1,3,4], [1,2], [1,2,4], [1,2,3], [1,2,3,4] ]
%%
%% 2> length(sc:flag_sets(lists:seq(1,16))).
%% 65536
%%
%% 3> sc:flag_sets([]).
%% [ [] ]
%%
%% 4> SourceOfPowers = sc:flag_sets([magic,technology,evil,alien]).
%% [[],                              % Batman
%%  [alien],                         % Superman
%%  [evil],                          % Darkseid
%%  [evil,alien],                    % Sinestro
%%  [technology],                    % Mister Terrific (Michael Holt)
%%  [technology,alien],              % The Blue Beetle
%%  [technology,evil],               % The OMACs
%%  [technology,evil,alien],         % Braniac
%%  [magic],                         % Shazam
%%  [magic,alien],                   % Green Lantern (Alan Scott)
%%  [magic,evil],                    % Lucifer Morningstar
%%  [magic,evil,alien],              % pre-crisis Star Sapphire
%%  [magic,technology],              % Alexander Luthor Jr.
%%  [magic,technology,alien],        % Mister Miracle
%%  [magic,technology,evil],         % pre-crisis Sinestro
%%  [magic,technology,evil,alien]]   % Granny Goodness'''
%%
%% @since Version 465

-spec flag_sets(Flags::list()) -> list_of_lists().

flag_sets([]) ->

    [[]];





flag_sets([Flag|RemFlags]) ->

    [ MaybeFlag ++ Reps ||
        MaybeFlag <- [[],[Flag]],
        Reps      <- flag_sets(RemFlags)
    ].





%% @equiv member_sets(Memberships, no_absence)

member_sets(Memberships) ->

    member_sets(Memberships, no_absence).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> For a list of memberships, return every possible combination of one representative member from each list.
%% The parameter `AllowAbsence' controls whether memberships may be unrepresented; if unrepresented memberships are possible, then
%% one possible representation becomes the empty list. ```1> sc:member_sets([ [a,b],[1,2,3],[i,ii,iii] ], no_absence).
%% [ [a,1,i], [a,1,ii], [a,1,iii], [a,2,i], [a,2,ii], [a,2,iii], [a,3,i], [a,3,ii], [a,3,iii],
%%   [b,1,i], [b,1,ii], [b,1,iii], [b,2,i], [b,2,ii], [b,2,iii], [b,3,i], [b,3,ii], [b,3,iii]]
%%
%% 2> sc:member_sets([ [a,b],[1,2],[i,ii] ], allow_absence).
%% [ [], [i], [ii], [1], [1,i], [1,ii], [2], [2,i], [2,ii], [a], [a,i], [a,ii], [a,1], [a,1,i],
%%   [a,1,ii], [a,2], [a,2,i], [a,2,ii], [b], [b,i], [b,ii], [b,1], [b,1,i], [b,1,ii], [b,2],
%%   [b,2,i], [b,2,ii] ]
%%
%% 3> sc:member_sets([ [toast,pancakes], [sausage,bacon] ] ).
%% [[toast,sausage],
%%  [toast,bacon],
%%  [pancakes,sausage],
%%  [pancakes,bacon]]
%%
%% 4> sc:member_sets([ [toast,pancakes], [sausage,bacon] ], no_absence ).
%% [[toast,sausage],
%%  [toast,bacon],
%%  [pancakes,sausage],
%%  [pancakes,bacon]]
%%
%% 5> sc:member_sets([ [toast,pancakes], [sausage,bacon] ], allow_absence).
%%  [[],
%%  [sausage],
%%  [bacon],
%%  [toast],
%%  [toast,sausage],
%%  [toast,bacon],
%%  [pancakes],
%%  [pancakes,sausage],
%%  [pancakes,bacon]]
%%
%% 6> Format = fun(Person, Place, Weapon) -> "It was " ++ Person ++ " in the " ++ Place ++ " with the " ++ Weapon ++ "!" end.
%% #Fun<erl_eval.18.105910772>
%%
%% 7> [ Format(Pe,Pl,WW) || [Pe,Pl,WW] <- sc:member_sets( [ ["Col. Mustard", "Ms. Scarlett"], ["conservatory", "hallway", "kitchen"], ["lead pipe"] ] ) ].
%% ["It was Col. Mustard in the conservatory with the lead pipe!",
%%  "It was Col. Mustard in the hallway with the lead pipe!",
%%  "It was Col. Mustard in the kitchen with the lead pipe!",
%%  "It was Ms. Scarlett in the conservatory with the lead pipe!",
%%  "It was Ms. Scarlett in the hallway with the lead pipe!",
%%  "It was Ms. Scarlett in the kitchen with the lead pipe!"]'''
%%
%% @since Version 466

-spec member_sets(Memberships::list_of_lists(), AllowAbsence::allow_absence|no_absence) -> list_of_lists().

member_sets([], _) ->

    [[]];





member_sets([[]], _) ->

    [[]];





member_sets( [Membership|RemMemberships], no_absence   ) ->

    [ [Member] ++ RemRep ||
        Member <- Membership,
        RemRep <- member_sets(RemMemberships, no_absence)
    ];





member_sets( [Membership|RemMemberships], allow_absence) ->

    Compact = fun(Member, RemRep) ->

        case Member of

            empty ->
                RemRep;

            {item,X} ->
                [X] ++ RemRep

        end

    end,

    [ Compact(Member, RemRep) ||
        Member <- [empty] ++ [{item,X}||X<-Membership],
        RemRep <- member_sets(RemMemberships, allow_absence)
    ].





%% @equiv list_intersection(List1, List2, unsorted)

list_intersection(List1, List2) ->

    list_intersection(List1, List2, unsorted).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Efficiently computes the intersection of two lists.  The third parameter, which is optional and defaults to `unsorted', is either the atom `sorted' or `unsorted'.  If `sorted' is used, the function will sort both inputs before proceeding, as it requires sorted lists; as such, if you already know your lists to be sorted, passing `unsorted' will save some time.  The return list will be reverse sorted. ```1> sc:list_intersection([1,2,3,4,5,2,3,10,15,25,30,40,45,55],[1,3,5,5,5,15,20,30,35,40,50,55]).
%% [55,40,30,15,5,3,1]
%%
%% 2> sc:list_intersection([1],[2]).
%% []''' {@section Thanks} to Ayrnieu for catching a defect in the initial implementation.
%%
%% @since Version 471

-spec list_intersection(List1::list(), List2::list(), IsSorted::sorted|unsorted) -> list().

list_intersection(List1, List2, unsorted) ->

    list_intersection(lists:sort(List1), lists:sort(List2), sorted);





list_intersection(List1, List2, sorted) ->

    intersect_walk(List1, List2, []).





%% @private

intersect_walk( [], _L2, Work ) ->

    Work;





intersect_walk( _L1, [], Work) ->

    Work;





intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work)

    when L1Head == L2Head ->

    intersect_walk(L1Rem, L2Rem, [L1Head]++Work);





intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work)

    when L1Head < L2Head ->

    intersect_walk(L1Rem, [L2Head|L2Rem], Work);





intersect_walk( [L1Head|L1Rem], [L2Head|L2Rem], Work)

    when L1Head > L2Head ->

    intersect_walk( [L1Head|L1Rem], L2Rem, Work).





%% @equiv zip_n(Ls, to_tuple)

-spec zip_n(Ls::list()) -> list_of_tuples().

zip_n(Ls) ->

    zip_n(Ls, to_tuple).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Computes a zip on any sized group of lists, rather than just two or three as offered by the lists module. ```1> sc:zip_n([ [1,2,3], [a,b,c], [i,ii,iii] ]).
%% [{1,a,i},{2,b,ii},{3,c,iii}]
%%
%% 2> sc:zip_n([ [1,2,3], [a,b,c], [i,ii,iii], [x,y,z], [red,blue,green], [april,may,june] ]).
%% [{1,a,i,x,red,april},
%%  {2,b,ii,y,blue,may},
%%  {3,c,iii,z,green,june}]'''
%%
%% This is actually more efficient than one might expect at first glance.  I ran a benchmark of 100,000 transformations of a list of lists into a list of tuples using {@link benchmark/3} and {@link multi_do/4} against both zip_n and the library function zip3; the library function won at 150 seconds to 175, which is a far smaller difference than I expected.```3> Testy = [ [1,2,3], [1,2,3], [1,2,3] ].
%% [[1,2,3],[1,2,3],[1,2,3]]
%%
%% 4> sc:benchmark(sc, multi_do, [100000, sc, zip_n, [Testy]]).
%% {174.95563, [[{1,1,1},{2,2,2},{3,3,3}], [{1,1,1},{2,2,2},{3,3,3}], ... }
%%
%% 5> sc:benchmark(sc, multi_do, [100000, lists, zip3, Testy]).
%% {149.605, [[{1,1,1},{2,2,2},{3,3,3}], [{1,1,1},{2,2,2},{3,3,3}], ... }'''
%%
%% {@section Thanks} Thanks to Vladimir Sessikov for contributing this to and thus allowing conscription from <a href="http://www.erlang.org/ml-archive/erlang-questions/200207/msg00066.html">the mailing list</a>.
%%
%% @since Version 472

-spec zip_n(Ls::list(), ResultType::atom()) -> list_of_tuples().

zip_n(Ls, to_tuple) ->

    [ list_to_tuple(L) ||
        L <- zip_n_listn(Ls)
    ];





zip_n(Ls, to_list) ->

    zip_n_listn(Ls).





%% @private

zip_n_listn(Ls) ->

    [ lists:reverse(L) ||
        L <- zip_n_foldn(fun (A, Acc) -> [A|Acc] end, [], Ls)
    ].





%% @private

zip_n_foldn(_, _, []) ->

    [];





zip_n_foldn(Fun, Acc0, Ls) ->

    zip_n_foldn(Fun, Acc0, Ls, []).





zip_n_foldn(_, _, [ [] | _ ], Ret) ->

    lists:reverse(Ret);





zip_n_foldn(Fun, Acc0, Ls, Ret) ->

    zip_n_foldn(
        Fun,
        Acc0,
        [ tl(L) || L <- Ls ],
        [ lists:foldl(Fun, Acc0, [hd(L) || L <- Ls] ) | Ret ]
    ).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Provides a list of every unique combination of input terms, order-ignorant; contrast {@link permute/2}.  Permutations are all unique combinations of a set of tokens; the 2-permutations of `[a,b,c]' for example are `[a,b]', `[a,c]' and `[b,c]'.  Note the absence of other orderings, such as `[b,a]', which are provided by {@link permute/2}.  Combinations are taken of a smaller count of tokens than the main set.  Combinations are not ordered, but this implementation happens to provide answers in the same order as the input list.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> sc:combinations(2, [a,b,c,d]).
%% [ [a,b], [a,c], [a,d], [b,c], [b,d], [c,d] ]
%%
%% 2> sc:combinations(2, ["dave","kate","pat"]).
%% [ ["dave","kate"], ["dave","pat"], ["kate","pat"] ]
%%
%% 3> sc:combinations(2, [fast, strong, smart, lucky]).
%% [ [ fast,   strong ],
%%   [ fast,   smart  ],
%%   [ fast,   lucky  ],
%%   [ strong, smart  ],
%%   [ strong, lucky  ],
%%   [ smart,  lucky  ] ]''' {@section Thanks} to Alisdair Sullivan for this implementation, which has been slightly but not significantly modified since receipt.
%%
%% @since Version 473

-spec combinations(OutputItemSize::pos_integer(), Items::list()) -> list_of_lists().

combinations(1, Items)

    when is_list(Items) ->

    [ [I] || I <- Items ];





combinations(_N, []) ->

    [];





combinations(0, L)

    when is_list(L) ->

    [];





combinations(N, Items)

    when is_list(Items),
         is_integer(N),
         N > 0         ->

    [ lists:flatten(lists:append( [lists:nth(I, Items)], [J] )) ||
      I <- lists:seq(1, length(Items)),
      J <- combinations( (N-1), lists:nthtail(I, Items) )
    ].





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Expands a series of labels over lists to create a cartesian 2-ary tuple expansion.  ```1> sc:expand_labels([{villain,[lex_luthor,sinistar,gargamel]}]).
%% [{villain,lex_luthor},{villain,sinistar},{villain,gargamel}]
%%
%% 2> sc:expand_labels([ {hero,[superman,tinyship,papa_smurf]}, {villain,[lex_luthor,sinistar,gargamel]} ]).
%% [{hero,superman},
%%  {hero,tinyship},
%%  {hero,papa_smurf},
%%  {villain,lex_luthor},
%%  {villain,sinistar},
%%  {villain,gargamel}]'''
%%
%% @since Version 474

-spec expand_labels( [ {Label::any(), List::list()} ] ) -> list_of_lists().

expand_labels(List)

    when is_list(List) ->

    lists:flatten( [ expand_label(X) || X <- List ] ).





%% @private

expand_label({Label,List})

    when is_list(List) ->

    [ {Label,L} || L<-List ].





%% @equiv permute(List, length(List))

permute(List) ->

    permute(List, length(List)).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Calculate either the full or the depth-limited permutations of a list, order sensitive; contrast {@link combinations/2}.  Permutations are all valid orderings of a set of tokens; the permutations of `[a,b]' for example are `[a,b]' and `[b,a]'.  Depth limitation means the permutations of a smaller count of tokens from the main set; the 2-limited permutations of `[a,b,c]' for example are `[a,b]', `[a,c]', `[b,a]', `[b,c]', `[c,a]' and `[c,b]'.  Permutations are not ordered.  Mixed-type lists are safe; items are shallow evaluated, meaning that sublists within the list are treated as single elements, and will neither be rearranged nor will have elements selected from within them. ```1> sc:permute(["dave","kate","pat"]).
%% [ { "pat",  "kate", "dave" },
%%   { "kate", "pat",  "dave" },
%%   { "pat",  "dave", "kate" },
%%   { "dave", "pat",  "kate" },
%%   { "kate", "dave", "pat"  },
%%   { "dave", "kate", "pat"  } ]
%%
%% 2> sc:permute([fast, strong, smart, lucky], 2).
%% [ { strong, fast   },
%%   { smart,  fast   },
%%   { lucky,  fast   },
%%   { fast,   strong },
%%   { smart,  strong },
%%   { lucky,  strong },
%%   { fast,   smart  },
%%   { strong, smart  },
%%   { lucky,  smart  },
%%   { fast,   lucky  },
%%   { strong, lucky  },
%%   { smart,  lucky  } ]'''
%%
%% @since Version 474

-spec permute(List::list(), Depth::pos_integer()) -> list().

permute(List, 1)

    when is_list(List) ->

    [ [T] ||
        T <- List
    ];





permute(List, Depth)

    when is_list(List),
         is_integer(Depth) ->

    [ [T]++R ||
        T <- List,
        R <- permute(List--[T], Depth-1)
    ].





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Create sorted list X of 3-ary tuples {K,Ai,Bi} from sorted lists A, B of 2ary {K,Ai}/{K,Bi} tuples, where key K appears in both A and B. ```1> sc:shared_keys([{1,a},{2,a},{3,a}],[{1,b},{3,b},{4,b}]).
%% [{1,a,b},{3,a,b}]
%%
%% 2>sc:shared_keys([{1,a},{2,a}],[{3,b},{4,b}]).
%% []'''
%%
%% @since Version 475

-spec shared_keys(TupleList::sorted_keylist()) -> sorted_keylist().

shared_keys(TupleList)

    when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    shared_keys(lists:sort(A),lists:sort(B)).





%% @equiv shared_keys(lists:zip(lists:sort(A), lists:sort(B)))
%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Equivalent to {@link shared_keys/1}, but skips sorting the lists (and thus requires pre-sorted lists), which may save significant work repetition.

-spec shared_keys(TupleList::sorted_keylist(), presorted) -> sorted_keylist().

shared_keys(TupleList, presorted)

    when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    shared_keys(A,B);





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Create sorted list X of 3-ary tuples `{K,Ai,Bi}' from sorted lists A, B of 2ary `{K,Ai}'/`{K,Bi}' tuples, where key `K' appears in both `A' and `B'.

shared_keys(A,B)

    when is_list(A),
         is_list(B) ->

    both_lists_next_item(lists:sort(A),lists:sort(B),[]).





%% @equiv shared_keys(lists:sort(A),lists:sort(B))
%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Equivalent to {@link shared_keys/2}, but skips sorting the lists (and thus requires pre-sorted lists), which may save significant work repetition.

-spec shared_keys(A::sorted_keylist(), B::sorted_keylist(), presorted) -> sorted_keylist().

shared_keys(A,B,presorted)

    when is_list(A),
         is_list(B) ->

    both_lists_next_item(A,B,[]).





%% @private

both_lists_next_item([], _, Work) ->

    lists:reverse(Work);





%% @private

both_lists_next_item(_, [], Work) ->

    lists:reverse(Work);





%% @private

both_lists_next_item([ {K,Ai} | Ar], [ {K,Bi} | Br], Work) ->

    both_lists_next_item(Ar, Br, [{K,Ai,Bi}]++Work);





%% @private

both_lists_next_item(IA, IB, Work) ->

    [{Ka,_}|Ar] = IA,
    [{Kb,_}|Br] = IB,

    if

        Ka < Kb ->
            both_lists_next_item(Ar, IB, Work);

        true ->
            both_lists_next_item(IA, Br, Work)

    end.





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Takes the product of all numbers in the list.  Offered mostly to make dependant code clearer. ```1> sc:list_product([1,2,5.4]).
%% 10.8'''
%%
%% @since Version 476

-spec list_product(A::numeric_list()) -> number().

list_product(List)

    when is_list(List) ->

    list_product(List, 1).





list_product([], Counter) ->

    Counter;





list_product([Head|Tail], Counter) ->

    list_product(Tail, Counter*Head).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Remove unacceptable elements from an input list, as defined by another list or a filter function.  Common reasons for sanitization include reducing arbitrary or bulk data to key format (such as using an original filename and new size to generate a new filename or database key) and removing malformed items from a list before processing. ```1> sc:sanitize_tokens("ae0z4nb'wc-04bn ze0e 0;4ci ;e0o5rn;", "ace").
%% "aeceece"
%%
%% 2> Classifier = fun(apple) -> true; (banana) -> true; (cherry) -> true; (date) -> true; (elderberry) -> true; (_) -> false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 3> sc:sanitize_tokens([apple, boat, cherry, dog, elderberry], Classifier).
%% [apple,cherry,elderberry]
%%
%% 4> Vowels = fun($a)->true; ($e)->true; ($i)->true; ($o)->true; ($u)->true; ($A)->true; ($E)->true; ($I)->true; ($O)->true; ($U)->true; (_)->false end.
%% #Fun<erl_eval.6.13229925>
%%
%% 5> sc:sanitize_tokens("A quick brown fox jumped over the lazy dog", Vowels).
%% "Auiooueoeeao"
%%
%% 6> sc:sanitize_tokens("A quick brown fox jumped over the lazy dog", "abcdefABCDEF").
%% "Acbfedeead"
%%
%% 7> BobcatGoldthwait = fun(X) -> sc:sanitize_tokens(X, "aeiouAEIOU") end.
%% #Fun<erl_eval.6.13229925>
%%
%% 8> BobcatGoldthwait("A quick brown fox jumped over the lazy dog").
%% "Auiooueoeeao"'''
%%
%% @see sanitize_filename/1
%%
%% @since Version 477

-spec sanitize_tokens(InputList::list(), Allowed::sanitizer()) -> list().

sanitize_tokens(List, Allowed)

    when is_list(List),
         is_function(Allowed) ->

    lists:filter(Allowed, List);





sanitize_tokens(List, Allowed)

    when is_list(List),
         is_list(Allowed) ->

    lists:filter(fun(X) -> lists:member(X,Allowed) end, List).





%% @equiv bandwidth_calc(Data,all)
%%
%% @since Version 478

-spec bandwidth_calc(Data::any()) -> list_of_2ary_tuples().

bandwidth_calc(Data) ->

    bandwidth_calc(Data, all).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculates digital line bandwidth over timescales in converted units. ```1>'''
%% Also knows the shorthand notations `{X,meg}', `{X,gig}' and `{X,t}' for input only in base-10 only. ```5> sc:bandwidth_calc({10,meg}, {gigabits,day}).
%% {{gigabits,day},864.0}'''
%%
%% @since Version 478
%%
%% @todo That return type specification is wrong.  It needs to self-recurse.  Figure out how to at-spec that later.

-spec bandwidth_calc(Data::any(), Scale::bw_scale()|all) -> bw_rate() | [ bw_rate() ].

bandwidth_calc({bits_per_second, BitsPerSecond}, To) ->

    bandwidth_calc(BitsPerSecond, To);





bandwidth_calc({megabits_per_second, MbpS}, To) ->

    bandwidth_calc(MbpS * 1000000, To);





bandwidth_calc({MbpS, meg}, To) ->

    bandwidth_calc(MbpS * 1000000, To);





bandwidth_calc({MbpS, gig}, To) ->

    bandwidth_calc(MbpS * 1000000000, To);





bandwidth_calc({MbpS, t}, To) ->

    bandwidth_calc(MbpS * 1000000000000, To);





bandwidth_calc(BitsPerSecond, all)

    when is_integer(BitsPerSecond) ->

    [
        bandwidth_calc(BitsPerSecond, {Unit, Timeframe})
    ||
        Unit      <- [bits, megabits, mebibits, gigabits, gibibits, terabits, tebibits],
        Timeframe <- [second, minute, hour, day, week, month_28, month_29, month_30, month_31]
    ];





bandwidth_calc(BitsPerSecond, {Unit, Timeframe}) 

    when is_integer(BitsPerSecond) ->

    Divisor = scale_i(bits, Unit),

    Timescale = case Timeframe of
        second   -> 1;
        minute   -> 60;
        hour     -> 60*60;
        day      -> 60*60*24;
        week     -> 60*60*24*7;
        month_28 -> 60*60*24*28;
        month_29 -> 60*60*24*29;
        month_30 -> 60*60*24*30;
        month_31 -> 60*60*24*31;
        year_365 -> 60*60*24*365;
        year_366 -> 60*60*24*366
    end,

    { {Unit, Timeframe}, (BitsPerSecond / Divisor) * Timescale }.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

% ##todo comeback

scale_i(X,X)            -> 1;

scale_i(inches, feet)   -> 12;
scale_i(inches, yards)  -> 36;

scale_i(bits, kilobits) -> 1000;
scale_i(bits, kibibits) -> 1024;
scale_i(bits, megabits) -> 1000*1000;
scale_i(bits, mebibits) -> 1024*1024;
scale_i(bits, gigabits) -> 1000*1000*1000;
scale_i(bits, gibibits) -> 1024*1024*1024;
scale_i(bits, terabits) -> 1000*1000*1000*1000;
scale_i(bits, tebibits) -> 1024*1024*1024*1024;
scale_i(bits, petabits) -> 1000*1000*1000*1000*1000;
scale_i(bits, pebibits) -> 1024*1024*1024*1024*1024;
scale_i(bits, exabits)  -> 1000*1000*1000*1000*1000*1000;
scale_i(bits, exbibits) -> 1024*1024*1024*1024*1024*1024.





%% @since Version 478
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> <a href="http://www.drdobbs.com/architecture-and-design/225701139?pgno=1">See</a>

caspers_jones_estimate(FunctionPoints)

    when is_integer(FunctionPoints),
         FunctionPoints >= 0 ->


    [   { estimated_defects,   math:pow(FunctionPoints, 1.25) },
        { unit_test_baseline,  math:pow(FunctionPoints, 1.2)  },
        { months_to_implement, math:pow(FunctionPoints, 0.4)  },
        { staff_to_implement,  FunctionPoints/150             }
    ].





%% @equiv naive_bayes_likelihood(FeatureEvident, FeatureTotal, NonFeatureEvident, NonFeatureTotal, simple)
%%
%% @since Version 772

-spec naive_bayes_likelihood(FeatureEvident::non_neg_integer(), FeatureTotal::pos_integer(), NonFeatureEvident::non_neg_integer(), NonFeatureTotal::pos_integer()) -> Result::list().

naive_bayes_likelihood(FeatureEvident, FeatureTotal, NonFeatureEvident, NonFeatureTotal) ->

    naive_bayes_likelihood(FeatureEvident, FeatureTotal, NonFeatureEvident, NonFeatureTotal, simple).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Calculates the contributing difference probability, feature likelihood and non-feature likelihood of an event
%% via Naive Bayes Likelihood.
%%
%% For example, given a bulk of 100 email, where 60 are known spam and 40 known legit, where of the spam 48 contain the word "buy" and of the non-spam only 4 contain the word "buy," calculate bayes likelihoods regarding new email containing the word "buy." ```1> sc:naive_bayes_likelihood(48, 60, 4, 40).
%% 0.9230769230769231
%%
%% 2> sc:naive_bayes_likelihood(48, 60, 4, 40, full).
%% [ {contributing_difference,0.7000000000000001},
%%   {likelihood_featured,0.8},
%%   {likelihood_nonfeatured,0.1},
%%   {evident_feature_likelihood,0.9230769230769231} ]
%%
%% 3> sc:naive_bayes_likelihood(48, 60, 4, 40, simple).
%% 0.9230769230769231'''
%%
%% Unit and doc tested.
%%
%% @since Version 772

-spec naive_bayes_likelihood(FeatureEvident::non_neg_integer(), FeatureTotal::pos_integer(), NonFeatureEvident::non_neg_integer(), NonFeatureTotal::pos_integer(), WhetherIsSimple::simple|full) -> Result::list().

naive_bayes_likelihood(FeatureEvident, _FeatureTotal, NonFeatureEvident, _NonFeatureTotal, simple)

    when is_integer(FeatureEvident),
         is_integer(NonFeatureEvident),

         FeatureEvident    >= 0,
         NonFeatureEvident >= 0 ->


    FeatureEvident / (FeatureEvident + NonFeatureEvident);





naive_bayes_likelihood(FeatureEvident, FeatureTotal, NonFeatureEvident, NonFeatureTotal, full)

    when is_integer(FeatureEvident),
         is_integer(FeatureTotal),
         is_integer(NonFeatureEvident),
         is_integer(NonFeatureTotal),

         FeatureEvident    >= 0,
         FeatureTotal      >  0,
         NonFeatureEvident >= 0,
         NonFeatureTotal   >  0       ->


    LF  = FeatureEvident / FeatureTotal,
    NF  = NonFeatureEvident / NonFeatureTotal,
    CD  = LF - NF,
    EFL = FeatureEvident / (FeatureEvident + NonFeatureEvident),

    [ {contributing_difference,    CD},
      {likelihood_featured,        LF},
      {likelihood_nonfeatured,     NF},
      {evident_feature_likelihood, EFL} ].





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Get the scale of a same-sign numeric range.  Gives nonsense results for non-numeric lists, or for lists which have both positive and negative members.  For a numeric list [4,5,6,12], the scale of the range 4..12 is 3:1, which is represented as 3.0 . ```1> sc:range_scale([3, 4, 5, 6]).
%% 2.0
%%
%% 2> sc:range_scale([3, 6]).
%% 2.0
%%
%% 3> sc:range_scale([6, 5, 3]).
%% 2.0
%%
%% 4> sc:range_scale([3, 4, 5, 6, 7, 7.5]).
%% 2.5
%%
%% 5> sc:range_scale([3, 10, 12, 99]).
%% 33.0
%%
%% 6> sc:range_scale([3, 3, 3]).
%% 1.0'''
%%
%% Unit and doc tested.
%%
%% @since Version 479

-spec range_scale(NumList::numeric_list()) -> number().

range_scale(Nums)

    when is_list(Nums) ->

    {Lo, Hi} = sc:extrema(Nums),
    Hi/Lo.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Estimates the zipf baseline from a score and a rank position. ```1> sc:zipf_position_estimate(120, 3).
%% 360'''
%%
%% @since Version 480

-spec zipf_position_estimate(Score::number(), Rank::pos_integer()) -> number().

zipf_position_estimate(Score, Rank) ->

    Score * Rank.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Estimates the zipf baseline from each number in a numeric list. ```1> sc:zipf_estimate_list([ 120, 60, 40, 30, 24, 20 ]).
%% [120, 120, 120, 120, 120, 120]
%%
%% 2> sc:zipf_estimate_list([411,198,135,101,82]).
%% [411, 396, 405, 404, 410]
%%
%% 3> sc:zipf_estimate_list([630,298,231,180,118]).
%% [630, 596, 693, 720, 590]'''
%%
%% @since Version 480

-spec zipf_estimate_list(PosNumericList::pos_numeric_list()) -> pos_numeric_list().

zipf_estimate_list(PosNumericList) ->

    [ zipf_position_estimate(Li, I) || {Li,I} <- lists:zip(PosNumericList, lists:seq(1, length(PosNumericList)) ) ].





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> todo. ```1> sc:zipf_nearness([ 120, 60, 40, 30, 24, 20 ]).
%% [[ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ],
%%  [ {strength,1.0}, {center,120.0} ]]
%%
%% 2> sc:zipf_nearness([ 411, 198, 135, 101, 82 ]).
%% [[{strength,0.9635036496350365}, {center,405.2}],
%%  [{strength,0.9658536585365854}, {center,403.75}],
%%  [{strength,0.9853658536585366}, {center,406.3333333333333}],
%%  [{strength,0.9853658536585366}, {center,407.0}],
%%  [{strength,1.0},                {center,410.0}]]
%%
%% 3> sc:zipf_nearness([640,244,231,180,148]).
%% [[{strength, 0.6594594594594595}, {center,656.2}],
%%  [{strength, 0.6594594594594595}, {center,660.25}],
%%  [{strength, 0.9364864864864865}, {center,717.6666666666666}],
%%  [{strength, 0.972972972972973},  {center,730.0}],
%%  [{strength, 1.0},                {center,740.0}]]'''
%%
%% @since Version 480

-spec zipf_nearness(PosNumericList::pos_numeric_list()) -> number().

zipf_nearness(PosNumericList) ->

    ZD = zipf_estimate_list(PosNumericList),
    zipf_nearness_walk_strengths(ZD, []).





zipf_nearness_walk_strengths([], Work) ->

    lists:reverse(Work);





zipf_nearness_walk_strengths([_|Rem]=ZD, Work) ->

    Strength = 1 / (range_scale(ZD)),
    AMean    = arithmetic_mean(ZD),

    zipf_nearness_walk_strengths(Rem, [[{strength,Strength}, {center,AMean}]] ++ Work).





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Take the arithmetic mean (often called the average) of a list of numbers. ```1> sc:arithmetic_mean([1,2,3,4,5]).
%% 3.0
%%
%% 2> sc:arithmetic_mean([]).
%% 0.0
%%
%% 3> sc:arithmetic_mean([2,2,2,2]).
%% 2.0
%%
%% 4> sc:arithmetic_mean([-3,2]).
%% -0.5'''
%%
%% <a href="http://www.wolframalpha.com/input/?i=mean%281%2C2%2C3%2C4%2C5%29">Wolfram Alpha confirms result 1</a><br/>
%% <a href="http://www.wolframalpha.com/input/?i=mean%282%2C2%2C2%2C2%29">Wolfram Alpha confirms result 3</a><br/>
%% <a href="http://www.wolframalpha.com/input/?i=mean%28-3%2C2%29">Wolfram Alpha confirms result 4</a><br/>
%%
%% Unit, doc, spec and stochastic (result is number(); result between-eq extrema) tested.
%%
%% @see geometric_mean/1
%% @see harmonic_mean/1
%% @see weighted_arithmetic_mean/1
%% @see amean_vector_normal/1
%%
%% @since Version 481

-spec arithmetic_mean(InputList::numeric_list()) -> float().

arithmetic_mean([]) ->

    0.0;





arithmetic_mean(List)

    when is_list(List) ->

    lists:sum(List) / length(List).





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Take the geometric mean of a list of numbers. ```1> sc:geometric_mean([1,2,3,4,5]).
%% 2.6051710846973517
%%
%% 2> sc:geometric_mean([2,2,2]).
%% 2.0
%%
%% 3> sc:geometric_mean([3]).
%% 3.0
%%
%% 4> sc:geometric_mean([1,10,100]).
%% 10.000000000000002'''
%%
%% <a href="http://www.wolframalpha.com/input/?i=geometric+mean+{1%2C2%2C3%2C4%2C5}">Wolfram Alpha confirms result 1</a><br/>
%% <a href="http://www.wolframalpha.com/input/?i=geometric+mean+{2%2C2%2C2}">Wolfram Alpha confirms result 2</a><br/>
%% <a href="http://www.wolframalpha.com/input/?i=geometric+mean+{3}">Wolfram Alpha confirms result 3</a><br/>
%% <a href="http://www.wolframalpha.com/input/?i=geometric+mean+{1%2C10%2C100}">Wolfram Alpha confirms result 3</a>
%%
%% The geometric mean is not defined for lists including 0.  This implementation does not handle the geometric mean of lists including negative numbers.
%%
%% The naive approach `geometric_mean(List) -> math:pow(sc:list_product(List), 1/length(List))' is not used because it accumulates error very quickly, and is as such unsuited to huge lists.  This is the same as the expected function nth-root(prod, 1/n), but calculated differently for machine reasons.'''
%%
%% Unit, doc and stochastic (all results are floats, all results between extrema) tested.
%%
%% Thanks to Forest (anonymous by choice) for help resolving 0-correctness.
%%
%% @see arithmetic_mean/1
%% @see harmonic_mean/1
%% @see gmean_vector_normal/1
%%
%% @since Version 482

-spec geometric_mean(InputList::pos_numeric_list()) -> float().

geometric_mean([]) ->

    0.0;





% resist loss of precision for single item

geometric_mean([X]) ->

    X * 1.0;





geometric_mean(List)

    when is_list(List) ->

    % resist loss of precision for repeated item
    case is_repeated_list(List) of

        true ->
            [Item|_] = List,
            geometric_mean([Item]);

        false ->
            math:exp(arithmetic_mean([ math:log(X) || X<-List ]))

    end.







%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Take the harmonic mean of a list of numbers. ```1> sc:harmonic_mean([1,2,3,4,5]).
%% 2.18978102189781'''
%%
%% <a href="http://www.wolframalpha.com/input/?i=harmonic+mean+{1%2C2%2C3%2C4%2C5}">WolframAlpha Confirms</a>
%%
%% The harmonic mean is not defined for lists including 0.
%%
%% Thanks to Forest (anonymous by choice) for help resolving 0-correctness.
%%
%% @see arithmetic_mean/1
%% @see geometric_mean/1
%% @see hmean_vector_normal/1
%%
%% @since Version 483

-spec harmonic_mean(InputList::numeric_list()) -> float().

harmonic_mean([]) ->

    0.0;





harmonic_mean(List)

    when is_list(List) ->

    length(List) / lists:sum([ 1/X || X<-List ]).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Take the weighted arithmetic mean of the input values. ```1> sc:weighted_arithmetic_mean([ {8,1}, {3,4}, {16,1} ]).
%% 6.0'''
%%
%% @see arithmetic_mean/1
%% @see amean_vector_normal/1
%%
%% @since Version 484

-spec weighted_arithmetic_mean(InputList::weight_list()) -> float().

weighted_arithmetic_mean(List)

    when is_list(List) ->

    weighted_arithmetic_mean(List, 0, 0).





weighted_arithmetic_mean([], Num, Denom) ->

    Num/Denom;





weighted_arithmetic_mean( [{V,W} | Tail], Num, Denom) ->

    weighted_arithmetic_mean(Tail, Num+(W*V), Denom+W).





%% @since Version 487
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Return descriptive statistics of a numeric list.  nspired by J dstat from <a href="http://bbot.org/blog/archives/2011/03/17/on_being_surprised_by_a_programming_language/">bbot.org</a>.  J seems to be assuming sample for statistics.  I do not like assumptions.

% ##todo comeback code sample

dstat(NumericList, PopulationOrSample) ->

    { Min, Max } = extrema(NumericList),

    [   { sample_size,        length(NumericList)                                 },
        { minimum,            Min                                                 },
        { maximum,            Max                                                 },
        { median,             median(NumericList)                                 },
        { arithmetic_mean,    arithmetic_mean(NumericList)                        },
        { standard_deviation, standard_deviation(NumericList, PopulationOrSample) },
        { skewness,           skewness(NumericList)                               },
        { kurtosis,           kurtosis(NumericList)                               }
    ].





%% @since Version 487
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Return extended descriptive statistics of a numeric list.  nspired by J dstat from <a href="http://bbot.org/blog/archives/2011/03/17/on_being_surprised_by_a_programming_language/">bbot.org</a>.  J seems to be assuming sample for statistics.  I do not like assumptions.

% ##todo comeback code sample

% ##todo comeback add variance, percentiles.  histogram?  might be dangerous.

dstat_ex(NumericList, PopulationOrSample) ->

    { Min, Max } = extrema(NumericList),

    [   { sample_size,        length(NumericList)                                 },
        { minimum,            Min                                                 },
        { maximum,            Max                                                 },
        { median,             median(NumericList)                                 },
        { mode,               mode(NumericList)                                   },
        { arithmetic_mean,    arithmetic_mean(NumericList)                        },
        { geometric_mean,     geometric_mean(NumericList)                         },
        { harmonic_mean,      harmonic_mean(NumericList)                          },
        { standard_deviation, standard_deviation(NumericList, PopulationOrSample) },
        { skewness,           skewness(NumericList)                               },
        { kurtosis,           kurtosis(NumericList)                               }
    ].





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes the median (central) value of a list.  Sorts the input list, then finds and returns the middle value.  ```1> sc:median([1,2,999]).
%% 2'''
%%
%% @see arithmetic_mean/1
%% @see mode/1
%%
%% @since Version 488

-spec median(List::numeric_list()) -> number().

median(List)

    when is_list(List) ->

    SList  = lists:sort(List),
    Length = length(SList),

    case even_or_odd(Length) of

        even ->
            [A,B] = lists:sublist(SList, round(Length/2), 2),
            (A+B)/2;

        odd ->
            lists:nth( round((Length+1)/2), SList )

    end.





%% @equiv even_or_odd(Num)
%%
%% @doc Documentary convenience function (synonymous with even_or_odd/1) that returns the atoms `even' or `odd' for any integer. ```1> sc:parity(3).
%% odd'''
%%
%% Thanks for the suggestion, Forest.
%%
%% @since Version 648

-spec parity(Num::integer()) -> even | odd.

parity(Num) ->

    even_or_odd(Num).





%% @doc Documentary convenience function (synonymous with parity/1) that returns the atoms `even' or `odd' for any integer. ```1> sc:even_or_odd(3).
%% odd'''
%%
%% @since Version 489

-spec even_or_odd(Num::integer()) -> even | odd.

even_or_odd(Num)

    when is_integer(Num),
         Num band 1 == 0 ->

    even;





even_or_odd(Num)

    when is_integer(Num) ->

    odd.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Measures the standard deviation of the values in the list.  ```1> sc:standard_deviation([1,2,3,4,5],population).
%% 1.4142135623730951
%%
%% 2> sc:standard_deviation([1,2,3,4,5],sample).
%% 1.5811388300841898
%%
%% 3> sc:standard_deviation([2,2,2,2],population).
%% 0.0
%%
%% 4> sc:standard_deviation([2,2,2,2],sample).
%% 0.0'''
%%
%% @since Version 490

-spec standard_deviation(Values::numeric_list(), Kind::population|sample) -> float().

standard_deviation(Values, population)

    when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ sc:square(Val-Mean) || Val <- Values ]));





standard_deviation(Values, sample)

    when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt( lists:sum([ sc:square(Val-Mean) || Val <- Values ]) / (length(Values)-1) ).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes the Nth moment of a list.  The Nth moment of a list is the arithmetic mean of the list items, each taken to the Nth power.  Fractional Ns are well defined
%% and have obscure uses, though most will only ever use this with integer values of N; this function is valid for both.  Not to be confused with {@link central_moment/2}.  {@section Thanks}
%% to Kraln and Chile for straightening me out on moments and central moments.  ```1> sc:moment([1,1,1], 2).
%% 1.0
%%
%% 2> sc:moment([2,2,2], 2).
%% 4.0
%%
%% 3> sc:moment([1,2,3], 2).
%% 4.666666666666667
%%
%% 4> sc:moment([1,2,3], 3).
%% 12.0
%%
%% 5> sc:moment([1,2,3], 3.5).
%% 19.693026767781483'''
%%
%% Thanks to Chile and Kraln for straightening me out on moments and central moments
%%
%% @since Version 491

-spec moment(List::numeric_list(), N::number()) -> float().

moment(List, N)

    when is_list(List),
         is_number(N) ->

    arithmetic_mean( [ math:pow(Item, N) || Item <- List ] ).





%% @equiv [ moment(List, N) || N <- [2,3,4] ]
%%
%% @since Version 491
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

moments(List) ->

    moments(List, [2,3,4]).





%% @equiv [ moment(List, N) || N <- Moments ]
%%
%% @since Version 491
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

moments(List, Moments)

    when is_list(Moments) ->

    [ moment(List, M) || M <- Moments ].





%% @doc <span style="color:red"><b><i>BUGGY</i></b></span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes the Nth cetral moment of a list.  The Nth central moment of a list is the arithmetic mean of (the list items each minus the mean of the list, each
%% taken to the Nth power).  In a sense, this is the "normalized" moment.  Fractional Ns are not defined.  Not to be confused with {@link moment/2}.  {@section Thanks} to Kraln and
%% Chile for straightening me out on moments and central moments.  ```1> sc:central_moment([1,1,1], 2).
%% 0.0
%%
%% 2> sc:central_moment([2,2,2], 2).
%% 0.0
%%
%% 3> sc:central_moment([1,2,3], 2).
%% 0.666666666666666
%%
%% 4> sc:central_moment([1,2,3], 3).
%% 0.0'''
%%
%% Thanks to Chile and Kraln for straightening me out on moments and central moments
%%
%% @since Version 492

-spec central_moment(List::list(), N::integer()) -> float().

central_moment(List, N)

    when is_list(List),
         is_integer(N) ->

    ListAMean = arithmetic_mean(List),
    arithmetic_mean( [ math:pow(Item-ListAMean, N) || Item <- List ] ).





%% @equiv [ central_moment(List, N) || N <- [2,3,4] ]
%%
%% @since Version 492
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

central_moments(List) ->

    central_moments(List, [2,3,4]).





%% @equiv [ central_moment(List, N) || N <- Moments ]
%%
%% @since Version 492
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

central_moments(List, Moments)

    when is_list(Moments) ->

    [ central_moment(List, M) || M <- Moments ].





%% @equiv central_moment(List, 3)
%%
%% @since Version 493
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

skewness(List) ->

    central_moment(List, 3).





%% @equiv central_moment(List, 4)
%%
%% @since Version 494
%%
%% @doc <span style="color:red"><b><i>BUGGY</i></b></span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% Wrong! ##todo comeback

kurtosis(List) ->

    central_moment(List, 4).





% todo comeback

% excess_kurtosis(List) ->





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes a histograph count of the items in the list.  Mixed type lists are safe.  Input lists do not need to be sorted.  The histograph is shallow - that is, the histograph of `[ [1,2], [1,2], [2,2] ]' is `[ {[1,2],2}, {[2,2],1} ]', not `[ {1,2}, {2,4} ]'. ```1> sc:histograph([1,2,a,2,b,1,b,1,b,2,a,2,2,1]).
%% [{1,4},{2,5},{a,2},{b,3}]
%%
%% 2> sc:histograph([ sc:rand(10) || X <- lists:seq(1,100000) ]).
%% [{0,10044}, {1,9892}, {2,10009}, {3,10016}, {4,10050}, {5,10113}, {6,9990}, {7,9994}, {8,10004}, {9,9888}]
%%
%% 3> ChessBoard = [ rook,  knight, bishop, king,  queen, bishop, knight, rook,
%%                   pawn,  pawn,   pawn,   pawn,  pawn,  pawn,   pawn,   pawn,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   empty, empty,  empty,  empty, empty, empty,  empty,  empty,
%%                   pawn,  pawn,   pawn,   pawn,  pawn,  pawn,   pawn,   pawn,
%%                   rook,  knight, bishop, king,  queen, bishop, knight, rook ].
%% [rook,knight,bishop,king,queen,bishop,knight,rook,pawn,pawn,
%%  pawn,pawn,pawn,pawn,pawn,pawn,empty,empty,empty,empty,empty,
%%  empty,empty,empty,empty,empty,empty,empty,empty|...]
%%
%% 4> sc:histograph(ChessBoard).
%% [ { bishop, 4  },
%%   { empty,  32 },
%%   { king,   2  },
%%   { knight, 4  },
%%   { pawn,   16 },
%%   { queen,  2  },
%%   { rook,   4  } ]'''
%%
%% @since Version 496
%%
%% @todo add an argument presort to this and other functions to skip the sorting pass

-spec histograph(List::list()) -> weight_list().

histograph([]) ->

    [];





histograph(List)

    when is_list(List) ->

    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).





%% @private

histo_count( [], Current, Count, Work) ->

     lists:reverse([{Current,Count}]++Work);





histo_count( [Current|Tail], Current, Count, Work) ->

    histo_count(Tail, Current, Count+1, Work);





histo_count( [New|Tail], Current, Count, Work) ->

    histo_count(Tail, New, 1, [{Current,Count}] ++ Work).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes the mode (most common) value(s) of a list, as a list.  If there are more than one value tied for most common, all tied will be returned.  This function is safe for mixed-type lists, and does not perform deep traversal (that is, the mode of `[ [2,2] ]' is `[2,2]', not `2'). ```sc:mode([1,2,1,3,1,4]).
%% [1]
%%
%% 2> sc:mode([ [1,2,3], [2,3,4], [3,4,5], [2,3,4] ]).
%% [[2,3,4]]
%%
%% 3> sc:mode([ a,b, 1, a,b, 2, a,b, 3 ]).
%% [a,b]'''
%%
%% @see arithmetic_mean/1
%% @see median/1
%%
%% @since Version 497

-spec mode(List::numeric_list()) -> any().

mode([]) ->

    [];





mode(List)

    when is_list(List) ->

    mode_front(lists:reverse(lists:keysort(2, histograph(List)))).





mode_front([{Item,Freq}|Tail]) ->

    mode_front(Tail, Freq, [Item]).





mode_front([ {Item, Freq} | Tail],  Freq,   Results) ->

    mode_front(Tail, Freq, [Item]++Results);





mode_front([ {_Item,_Freq} |_Tail], _Better, Results) ->

    Results;





mode_front( [], _Freq, Results) ->

    Results.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the arithmetic mean of the elements of the unit vector for the vector provided.
%%
%% @since Version 497

-spec amean_vector_normal(VX::numeric_list()) -> number().

amean_vector_normal(VX) ->

    arithmetic_mean(sc:vector_normalize(VX)).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the geometric mean of the elements of the unit vector for the vector provided.
%%
%% @since Version 498

-spec gmean_vector_normal(VX::numeric_list()) -> number().

gmean_vector_normal(VX) ->

    geometric_mean(sc:vector_normalize(VX)).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the harmonic mean of the elements of the unit vector for the vector provided.
%%
%% @since Version 499

-spec hmean_vector_normal(VX::numeric_list()) -> number().

hmean_vector_normal(VX) ->

    harmonic_mean(sc:vector_normalize(VX)).





% Thanks for some math help on erl-b, erl-c and engset, Vat and Wintermute

% todo incomplete comeback

% erlang_b_distribution(N,A) ->
%
%    Num   = math:pow(A,N) / factorial(N),
%    Denom = lists:sum([ math:pow(A,I) / factorial(I) || I <- lists:seq(0,N) ]),
%
%    Num / Denom.





% todo incomplete comeback

% erlang_c_distribution(N,A) ->
%
%    Num   = (math:pow(A,N) / sc:factorial(N)) * (N/(N-A)),
%
%    Denom = lists:sum([ math:pow(A,I) / factorial(I) || I <- lists:seq(0,N-1) ])
%          + ((math:pow(A,N)/factorial(N))*(N/(N-A))),
%
%    {wait_probability, Num / Denom}.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculate the median absolute deviation of a {@type numericlist()}. ```1> sc:median_absolute_deviation([1,1,2,2,4,6,9]).
%% 1'''
%%
%% @since Version 501

-spec median_absolute_deviation(List::numeric_list()) -> number().

median_absolute_deviation(List)

    when is_list(List) ->

    ListMedian = median(List),
    median( [ abs(ListItem - ListMedian) || ListItem <- List ] ).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Returns the expected value of infinite selection from a weighted numeric list.  Elements of the list may either be numbers or tuples in the form `{Value,Weight}' (weight may be floating-point).  This means that `[ 1,1,1, 10,10 ]' and `[ {1,3}, {10,2} ]' are the same list. The expected value of an empty list is undefined, and will throw `badarith'. This includes zero-weighted tuple form (example 6.) ```1> sc:expected_value([1,2,3,4,5,6]).
%% 3.50000
%%
%% 2> sc:expected_value([ {1,5}, {10,1} ]).
%% 2.5
%%
%% 3> sc:expected_value([ 1,1,1,1,1, {10,1} ]).
%% 2.5
%%
%% 4> sc:expected_value([ {1,8}, {-1,7} ]).
%% 0.06666666666666667
%%
%% 5> catch sc:expected_value([ ]).
%% {'EXIT',{badarith,[{sc,expected_value,3},
%%                    {erl_eval,do_apply,5},
%%                    {erl_eval,expr,5},
%%                    {shell,exprs,7},
%%                    {shell,eval_exprs,7},
%%                    {shell,eval_loop,3}]}}
%%
%% 6> catch sc:expected_value([ {1,0}, {2,0} ]).
%% {'EXIT',{badarith,[{sc,expected_value,3},
%%                    {erl_eval,do_apply,5},
%%                    {erl_eval,expr,5},
%%                    {shell,exprs,7},
%%                    {shell,eval_exprs,7},
%%                    {shell,eval_loop,3}]}}'''
%%
%% <a href="http://www.wolframalpha.com/input/?i=ExpectedValue[+f%2C+{1%2C2%2C3%2C4%2C5%2C6}%2C+f+]">Wolfram Alpha confirms result 1</a><br/>
%% <a href="http://www.wolframalpha.com/input/?i=ExpectedValue[+f%2C+{1%2C1%2C1%2C1%2C1%2C10}%2C+f+]">Wolfram Alpha confirms result 2</a><br/>
%% <a href="http://www.wolframalpha.com/input/?i=ExpectedValue[+f%2C+{+1%2C+1%2C+1%2C+1%2C+1%2C+1%2C+1%2C+1%2C+-1%2C+-1%2C+-1%2C+-1%2C+-1%2C+-1%2C+-1+}%2C+f+]">Wolfram Alpha confirms result 4</a>
%%
%% Unit and doc tested.
%%
%% @since Version 502

-spec expected_value(List::weight_list()) -> number().

expected_value(List) ->

    expected_value(List, 0, 0).





expected_value([], Sum, Range) ->

    Sum/Range;





expected_value( [ {Value,Probability} | Remainder], Sum, Range) ->

    expected_value(Remainder, Sum+(Value*Probability), Range+Probability);





expected_value( [ UnweightedItem | Remainder], Sum, Range) ->

    expected_value([{UnweightedItem,1}] ++ Remainder, Sum, Range).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes the absolute value of the difference between the two arguments.  Offered mostly to make dependant code clearer. ```1> sc:absolute_difference(1.25, 1).
%% 0.25
%%
%% 2> sc:absolute_difference(2,1).
%% 1
%%
%% 3> sc:absolute_difference(1,2).
%% 1
%%
%% 4> sc:absolute_difference(1,1).
%% 0
%%
%% 5> sc:absolute_difference(1,-1).
%% 2
%%
%% 6> sc:absolute_difference(100,35).
%% 65'''
%%
%% @since Version 504

-spec absolute_difference(A::number(), B::number()) -> number().

absolute_difference(A,B) ->

    abs(A-B).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculates the root mean square of the values in the list.  ```1> sc:root_mean_square([1,2,3,4,5]).
%% 3.3166247903554
%%
%% 2> sc:root_mean_square([2,2,2]).
%% 2.0'''
%%
%% @since Version 505

-spec root_mean_square(Values::numeric_list()) -> float().

root_mean_square(List)

    when is_list(List) ->

    math:sqrt(arithmetic_mean([ Val*Val || Val <- List ])).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculate the magnitude (also known as the root sum square) of a vector.
%%
%% @since Version 506

-spec root_sum_square(VX::vector()) -> number().

root_sum_square(VX)

    when is_list(VX) ->

    math:sqrt(lists:sum([ X*X || X <- VX ]));





root_sum_square(VX)

    when is_tuple(VX) ->

    root_sum_square(tuple_to_list(VX)).




%% @equiv root_sum_square(VX)
%%
%% @since Version 506

vector_magnitude(VX) ->

    root_sum_square(VX).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes the modulus of an integer by another integer.  Luckily, erlang calls what most languages refer to as modulus by its correct name, remainder (c's `%', erlang's `rem').  Modulus is implemented incorrectly in nearly every language, because chip vendors implement remainder and the wrong name stuck.  The difference is in how the operator reacts to a negative `Base': -10 modulo 3 is 2, whereas -10 rem 3 is -1.  Remainder takes the residue of dividing the base by the lowest (nearest negative infinity) integer N adjacent the real valued divisor; modulo returns the highest, which is less CPU efficient but always provides an answer on [0..Range-1]. ```1> sc:mod(10,3).
%% 1
%%
%% 2> [ sc:mod(X,4) || X <- lists:seq(-10,10) ].
%% [2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2]'''
%%
%% @since Version 507

-spec mod(Base::integer(), Range::integer()) -> integer().

mod(Base, Range)

    when is_integer(Base),
         is_integer(Range) ->

    case Base rem Range of

        X when X < 0 ->
            X + Range;

        Z ->
            Z

    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Squares the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> sc:square(2).
%% 4
%%
%% 2> sc:square(2.5).
%% 6.25'''
%%
%% @since Version 508

-spec square(Input::number()) -> number().

square(X) ->

    X*X.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Cubes the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> sc:cube(2).
%% 8
%%
%% 2> sc:cube(2.5).
%% 6.25'''
%%
%% @since Version 508

-spec cube(Input::number()) -> number().

cube(X) ->

    X*X*X.





% comeback todo documentation

%% @since Version 509
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

factorial(X) ->

    factorial(X, 1).





%% @private

factorial(0, _Counter) ->

    1;





factorial(1, Counter) ->

    Counter;





factorial(X, Counter)

    when is_integer(X),
         X > 1 ->

    factorial(X-1, Counter*X).





% comeback todo documentation

%% @since Version 639
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

additive_factorial(X) ->

    additive_factorial(X, 1).





%% @private

additive_factorial(0, _Counter) ->

    0;





additive_factorial(1, Counter) ->

    Counter;





additive_factorial(X, Counter)

    when is_integer(X),
         X > 1 ->

    additive_factorial(X-1, Counter+X).





%% @since Version 510
%% @equiv ceiling(X)

ceil(X) ->

     ceiling(X).





%% @since Version 510

-spec ceiling(X :: number()) -> integer().

%% @doc <span style="color: green; font-weight: bold;">Tested</span> Returns the ceiling (round towards positive infinity) of a float. ```1> sc:ceil(0.5).
%% 1
%%
%% 2> sc:ceil(0).
%% 0
%%
%% 3> sc:ceil(0.0).
%% 0
%%
%% 4> sc:ceil(1.0).
%% 1
%%
%% 5> sc:ceil(-1.0).
%% -1
%%
%% 6> sc:ceil(-1.5).
%% -1
%%
%% 7> sc:ceil(-1).
%% -1
%%
%% 8> sc:ceil(1).
%% 1'''
%%
%% Unit, doc and stochastic property (int as float identity; float always smaller within 1; all results integers) tested.

ceiling(X)

    when X < 0 ->

    trunc(X);





ceiling(X) ->

    T = trunc(X),

    case X - T of

        0   -> T;
        0.0 -> T;
        _   -> T + 1

    end.





% todo comeback docs; point out this isn't erlang:floor because of negative number behavior

%% @since Version 511

-spec floor(X :: number()) -> integer().

%% @doc <span style="color: green; font-weight: bold;">Tested</span> Takes the floor (round towards negative infinity) of a number.  This is different than `erlang:trunc/1', which removes the mantissa, in its
%% handling of negative numbers: trunc diminishes towards zero, not towards negative infinity (note examples 6 and 7 below.) ```1> sc:floor(0.5).
%% 0
%%
%% 2> sc:floor(0).
%% 0
%%
%% 3> sc:floor(0.0).
%% 0
%%
%% 4> sc:floor(1.0).
%% 1
%%
%% 5> sc:floor(-1.0).
%% -1
%%
%% 6> sc:floor(-1.5).
%% -2
%%
%% 7> erlang:trunc(-1.5).
%% -1
%%
%% 8> sc:floor(-1).
%% -1
%%
%% 9> sc:floor(1).
%% 1'''
%%
%% Unit, doc and stochastic property (int as float identity; float always larger within 1; all results integers) tested.

floor(X) when X < 0 ->

    TruncX = trunc(X),

    case X - TruncX of
        0   -> TruncX;
        0.0 -> TruncX;
        _   -> TruncX - 1
    end;





floor(X) ->

    trunc(X).





% comeback todo docs

%% @since Version 512
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

mersenne_prime(Which) ->

    mersenne_prime_worker(Which, 1).





%% @private

mersenne_prime_worker(0, Current) ->

    Current - 1;





mersenne_prime_worker(Remain, Current)

    when Remain > 30 ->

    mersenne_prime_worker(Remain-30, Current*1073741824);





mersenne_prime_worker(Remain, Current) ->

    mersenne_prime_worker(Remain-1, Current*2).





% comeback todo docs

%% since Version 513
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Generates a list of the factors of an integer.  Not an awesome implementation.  Thanks for noticing that I was repeating sqrt unnecessarily, Forest.

factorize(N)

    when is_integer(N), N > 1 ->

    factorize(N, 2, [], math:sqrt(N));





factorize(1) ->

    [1];





factorize(0) ->

    {error, "The factorization of 0 is undefined."}.





factorize(N, Current, Work, Cap) ->

    case Current > Cap of

        true ->
            case N of
                1 -> lists:reverse(       Work);
                _ -> lists:reverse([N] ++ Work)
            end;

        false ->
            case N rem Current of
                0 -> factorize(N div Current, Current,   [Current] ++ Work, Cap);
                _ -> factorize(N,             Current+1, Work,              Cap)
            end

    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns true if the list is unique; false otherwise.  List uniqueness is defined as whether any member of the list compares equally to any other member; deep list inspection is not performed.  Comparison is type-safe. ```2> sc:is_unique_list([1,2,3]).
%% true
%%
%% 2> sc:is_unique_list([1,2,3,1]).
%% false
%%
%% 3> sc:is_unique_list([1,2,3,{1}]).
%% true
%%
%% 4> sc:is_unique_list([1,2,3,[1]]).
%% true
%%
%% 5> sc:is_unique_list([1,2,3,[1],[1]]).
%% false'''
%%
%% @since Version 514

-spec is_unique_list(List::list()) -> true | false.

is_unique_list(List) ->

    length(lists:usort(List)) == length(List).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns true if the list is sorted; false otherwise.  List sortedness is typesafe, and defined equivalently to how defined by the language and `lists:sort()'. ```1> sc:is_sorted_list([1,2,3]).
%% true
%%
%% 2> sc:is_sorted_list([1,2,3,1]).
%% false
%%
%% 3> sc:is_sorted_list([1,2,3,false]).
%% true
%%
%% 4> sc:is_sorted_list([false,1,2,3]).
%% false'''
%%
%% @since Version 514

%% @todo ascending, descending, both forms

-spec is_sorted_list(List::list()) -> true | false.

is_sorted_list([]) ->

    true;





is_sorted_list([Head|Rem]) ->

    is_sorted_list_worker(Rem, Head).





is_sorted_list_worker([], _Last) ->

    true;





is_sorted_list_worker([Cur|Rem], Last)

    when Cur >= Last ->

    is_sorted_list_worker(Rem, Cur);





is_sorted_list_worker([_Cur|_Rem], _Last) ->

    false.





% todo comeback docs

%% @since Version 515
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

alarm_set(Time, Lambda, Repeat) ->

    { alarm_actor_pid,
      spawn( fun() -> alarm_head(Time, Lambda, Repeat, construct) end )
    }.





% todo comeback docs

%% @since Version 515
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

alarm_terminate( { alarm_actor_pid, Pid } ) ->

    Pid ! terminate,
    ok.





%% @since Version 515

alarm_head(Time, Lambda, Repeat, construct) ->

    Head = self(),
    alarm_head(Time, Lambda, Repeat, spawn( fun() -> alarm_trigger(Time, Repeat, Head) end ));





alarm_head(Time, Lambda, Repeat, TriggerPid) ->

    receive

        terminate ->
            TriggerPid ! terminate,
            ok;

        trigger ->
            NextLambda = case is_list(Lambda) of
                true ->
                    [ThisLambda | RemLambda] = Lambda,
                    ThisLambda(),
                    RemLambda;
                false ->
                    Lambda(),
                    Lambda
            end,
            alarm_head(Time, NextLambda, Repeat, TriggerPid);

        UnknownMessage ->
            io:format("Warning: alarm head ~p took unexpected message.  Discarded:~n  ~w~n~n", [self(), UnknownMessage]),
            alarm_head(Time, Lambda, Repeat, TriggerPid)

    end.





%% @since Version 515

alarm_trigger(Time, Repeat, Head) ->

    receive

        terminate ->
            Head ! terminate,
            ok;

        UnknownMessage ->
            io:format("Warning: alarm trigger ~p took unexpected message; this screws with its timing cycle.  Discarded:~n  ~w~n~n", [self(), UnknownMessage]),
            alarm_trigger(Time, Repeat, Head)

    after Time ->

        Head ! trigger,

        case Repeat of
            1         -> Head ! terminate, ok;
            0         -> Head ! terminate, ok;
            no_repeat -> Head ! terminate, ok;
            forever   -> alarm_trigger(Time, forever, Head);
            Count     -> alarm_trigger(Time, Count-1, Head)
        end

    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculates the coordinate which represents the per-axis arithmetic mean of a set of points.  Convenient in list comprehensions.  To calculate the centroid of `[1,1]', `[2,3]', you gather the X coordinates `[1,2]', then use their mean `1.5'; then do the same for the Y, `[1,3]' to `2'.  The centroid would thus be `[1.5,2]'.  You may pass any number of coordinates to this function, of any axis count, but they must all be the same axis count.  The return value will be a coordinate with the same axis count.  Negative and real values are fine; imaginary math is not implemented. ```1> sc:centroid([[1]]).
%% [1.0]
%%
%% 2> sc:centroid([[1,1],[2,2]]).
%% [1.5,1.5]
%%
%% 3> sc:centroid([[1,1,1],[2,2,2],[3,3,3]]).
%% [2.0,2.0,2.0]
%%
%% 4> sc:centroid([[1,-1,1.0],[-2,-2,-2],[3,3,3],[4,4,4],[5,5,5]]).
%% [2.2,1.8,2.2]'''
%%
%% @since Version 516

-spec centroid(InputList::coord_list()) -> coord().

centroid(CoordList)

    when is_list(CoordList) ->

    [ sc:arithmetic_mean(X) ||
        X <- sc:zip_n(CoordList, to_list)
    ].





% comeback todo docs

%% @since Version 517
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

nearest_to(Centers, Point) ->

    { C, _ } = sc:keymin(2, [ { Center, sc:euclidean_distance(Center, Point) } || Center <- Centers ]),
    C.





% comeback todo docs

%% @since Version 518
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

by_distance_raw(Centers, Points)

    when is_list(Centers), is_list(Points) ->

    [ {Key, [NV || {_NC,NV} <- Near]} || {Key, Near} <- sc:key_group(1, [ { nearest_to(Centers, Point), Point } || Point <- Points ]) ].





%% @since Version 519
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

by_distance(Centers, Points)

    when is_list(Centers),
         is_list(Points)  ->

    Work = by_distance_raw(Centers, Points),

    Grab = fun(Center) ->
        case lists:keysearch(Center, 1, Work) of
            {value, {Center, Matches}} -> Matches;
            false                      -> []
        end
    end,

    [ Grab(Center) || Center <- Centers ].





% comeback todo to implement

% k_means(KCount, ItemList) when is_integer(KCount), is_list(CoordList) ->





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Look up all attributes of a given module.  ```1> sc:module_attribute(sc).
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
%%
%% @since Version 520

-spec module_attribute(Module::atom()) -> AttributeList::list() | { error, no_such_module }.

module_attribute(Module) ->

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->
            Attributes;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





%% @doc <span style="color:red"><b><i>BUGGY</i></b></span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Look up an Erlang module attribute value by title.  Originally found at <a href="http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html">Mastering Erlang Part 3</a>; subsequently cleaned up and given error reporting.  ```1> sc:module_attribute(scutil, author).
%% "John Haugeland <stonecypher@gmail.com>"
%%
%% 2> sc:module_attribute(scutil, license).
%% [{mit_license,"http://scutil.com/license.html"}]'''{@section Thanks} to Alain O'Dea for pointing out defects in this routine regarding repeated module elements, and available improvements to the provided API.  <a href="http://fullof.bs/reading-module-attributes-in-erlang#comment-475" target="_blank">Mr. O'Dea's insightful advice</a> will be implemented, but that time has not yet come.
%%
%% Found at http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html
%% Reformatted for clarity, removed unnessecary framing list
%% Added error handling behavior
%%
%% @since Version 520

-spec module_attribute(Module::atom(), Attribute::atom()) -> { value, {Attribute::any(), Value::any()} } | { error, no_such_attribute } | { error, no_such_module }.

module_attribute(Module,Attribute) ->

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





% todo comeback docs

%% @since Version 521
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

module_feature(Module, Feature) ->

    case beam_lib:chunks(Module, [Feature]) of

        { ok, { Module, [ {Feature,Attributes} ] } } ->
            Attributes;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Scans a module for an attribute svn_revision, parses it in the format expected from the svn:keyword Revision, and returns the version number as an integer.  To use, add a module attribute to your module as follows: `-svn_revision("$+Revision$).', after removing the plus (if the plus wasn't there, the example would get corrupted when I updated the module `;)').  Then set the svn keyword "Revision" on the file, and check it in.  After that, your version is magically updated every time you check in!  `:D'  The sole argument to this function is the name of the module to be scanned, as an atom. ```1> sc:scan_svn_revision(testerl).
%% 16'''
%%
%% @since Version 523

-spec svn_revision(ModuleName::atom()) -> integer().

svn_revision(Module) ->

    "$Revision: " ++ X = module_attribute(Module, svn_revision),
    [ Head | _Rem ]    = string:tokens(X, " "),

    list_to_integer(Head).





% todo comeback docs

%% @since Version 524
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

function_stats(Module) ->

    [ { entrypoints,     entrypoint_count(Module)     },
      { function_labels, function_label_count(Module) },
      { function_points, function_point_count(Module) }
    ].





% todo comeback docs

%% @since Version 525
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

function_point_count(Module) ->

    length(function_points(Module)).





% todo comeback docs

%% @since Version 526
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

function_label_count(Module) ->

    length(function_labels(Module)).





% todo comeback docs

%% @since version 527
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

entrypoint_count(Module) ->

    length(entrypoints(Module)).





% todo comeback docs

%% @since Version 528
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

function_labels(Module) ->

    lists:usort(
        [ L ||
            {_,L,_,_} <- abstract_functions(Module)
        ]
    ).





% todo comeback docs

%% @since Version 529
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

function_points(Module) ->

    lists:usort(
        [ {L,A} ||
            {_,L,A,_} <- abstract_functions(Module)
        ]
    ).





% todo comeback docs

%% @since Version 530
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

entrypoints(Module) ->

    lists:flatten(
        [
            [
                { L, A, [ {Kind,Name} || {Kind,_LineNum,Name}<-ThisAcArg ], When }
            ||
                { _, _, ThisAcArg, When, _ } <- AbstractClauseList
            ]
        ||
            { _, L, A, AbstractClauseList } <- sc:abstract_functions(Module)
        ]
    ).





% todo comeback docs

%% @since Version 530
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

entrypoints(Module, FName) ->

    lists:flatten(
        [
            [
                { L,
                  A,
                  [
                      { Kind, Name }
                  ||
                      { Kind, _LineNum, Name } <- ThisAcArg
                  ],
                  When
                }
            ||
                { _, _, ThisAcArg, When, _ } <- AbstractClauseList
            ]
        ||
            { _, L, A, AbstractClauseList } <- abstract_functions(Module), L==FName
        ]
    ).





% todo comeback docs

%% @since version 531
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

abstract_functions(Module) ->

    [
        { Id, Name, Arity, Code }
    ||
        {function, Id, Name, Arity, Code} <- module_abstract_representation(Module, stripped)
    ].





% todo comeback docs

%% @since version 531
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

abstract_function(Module, FName) ->

    [
        {Id, Name, Arity, Code}
    ||
        {function, Id, Name, Arity, Code} <- module_abstract_representation(Module, stripped),
        Name == FName
    ].





% todo comeback docs

%% @since version 532
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

abstract_attributes(Module) ->

    [
        {Id, Name, Value}
    ||
        {attribute, Id, Name, Value} <- module_abstract_representation(Module, stripped)
    ].





%% @since Version 533
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

module_atoms(Module) ->

    module_feature(Module, atoms).





% todo comeback docs

%% since Version 534
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

key_cluster(_Index, []) ->

    [];





% 177> sc:key_cluster(1,[{1,a},{1,aa},{2,a}]).
% [{1,[{1,a},{1,aa}]},{2,[{2,a}]}]

key_cluster(Index, List) ->

    SortedList = lists:keysort(Index, List),
    [First|_]  = List,
    Current    = element(Index, First),

    key_cluster(Index, SortedList, Current, [], []).





key_cluster(Index, [], _Current, Work, Storage) ->

    % Not happy about this, but too lazy to fix it.

    PossibleResult = lists:reverse([ lists:reverse(Sublist) || Sublist <- ([Work] ++ Storage) ]),

    [RemoveEmptyHeadCons|FixedResult] = PossibleResult,

    FinalResult = case RemoveEmptyHeadCons of
        [] -> FixedResult;
        _  -> PossibleResult
    end,

    LabelItem = fun(Item) -> [X|_] = Item, Label = element(Index, X), { Label, Item } end,

    [ LabelItem(Item) || Item <- FinalResult ];





key_cluster(Index, [WorkItem|Rem], Current, Work, Storage) ->

    case element(Index, WorkItem) of
        Current -> key_cluster(Index, Rem, Current, [WorkItem]++Work, Storage);
        Other   -> key_cluster(Index, Rem, Other,   [WorkItem],       [Work]++Storage)
    end.





% todo comeback docs

%% @since Version 535
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

distinct_neighbor_pairs(List) ->

    distinct_neighbor_pairs(List, [], make_tuples).





% todo comeback docs

%% @since Version 535
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

distinct_neighbor_pairs(List, MakeType) ->

    distinct_neighbor_pairs(List, [], MakeType).





distinct_neighbor_pairs([], Work, _Make_type) ->

    lists:reverse(Work);  % should actually only ever be an empty list anyway





distinct_neighbor_pairs([[_LastItemIsNotInAPairByItself]], Work, _Make_type) ->

    lists:reverse(Work);





distinct_neighbor_pairs([[]], Work, _Make_type) ->

    lists:reverse(Work);





distinct_neighbor_pairs([A,B|Rem], Work, make_tuples) ->

    distinct_neighbor_pairs(Rem, [{A,B}] ++ Work, make_tuples);





distinct_neighbor_pairs([A,B|Rem], Work, make_lists) ->

    distinct_neighbor_pairs(Rem, [[A,B]] ++ Work, make_lists).





% todo comeback docs

%% @since Version 536
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

all_neighbor_pairs(List) ->

    all_neighbor_pairs(List, make_tuples).





% todo comeback docs

%% @since Version 536
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

all_neighbor_pairs(List, WorkType) ->

    all_neighbor_pairs(List, [], WorkType).





all_neighbor_pairs([], Work, _Work_type) ->

    lists:reverse(Work);  % should actually only ever be an empty list anyway





all_neighbor_pairs([[]], Work, _Work_type) ->

    lists:reverse(Work);





all_neighbor_pairs([_LastItemIsNotInAPairByItself], Work, _Work_type) ->

    lists:reverse(Work);





all_neighbor_pairs([A,B|Rem], Work, make_lists) ->

    all_neighbor_pairs([B]++Rem, [[A,B]] ++ Work, make_lists);





all_neighbor_pairs([A,B|Rem], Work, make_tuples) ->

    all_neighbor_pairs([B]++Rem, [{A,B}] ++ Work, make_tuples).





%% @since Version 537
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

partition_by_residue(Data, Function) ->

    [
        { GroupId, [   GDatum
                   ||  { _H, GDatum } <- GroupData
                   ]
        }
    ||
        { GroupId, GroupData } <- key_group( 1, [   { Function(Datum), Datum }
                                                ||  Datum <- Data
                                                ]
                                           )
    ].





% comeback todo docs

%% @since Version 538
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the last element of the initial sequence where all items pass the predicate function.  ```1> sc_lists:last_while_pos(fun erlang:is_atom/1, [a,b,c,d,2,f]).
%% 4
%%
%% 2> sc_lists:last_while_pos(fun erlang:is_atom/1, [a,b,c,d,r,f]).
%% 6
%%
%% 3> sc_lists:last_while_pos(fun erlang:is_atom/1, [1,a,b,c,d,r,f]).
%% false'''

last_while_pos(Predicate, List) ->

    last_while_pos(1, Predicate, List, false).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

last_while_pos(Predicate, List, Default) ->

    last_while_pos(1, Predicate, List, Default).





last_while_pos(_N, [],  _Pred, Last) ->

    Last;





last_while_pos(N, [Head|Tail], Pred, Last) ->

    case Pred(Head) of
        true  -> last_while_pos(N+1, Tail, Pred, N);
        false -> Last
    end.





%% @since Version 538
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

key_group(Pos, List) ->

    key_group(Pos, List, unsorted).





%% @since Version 538
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

key_group(Pos, List, unsorted)

    when is_list(List) ->

    SList = lists:keysort(Pos,List),
    [F|_] = SList,

    key_group(Pos, SList, element(Pos,F), [], []);





%% @since Version 538
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

key_group(Pos, List, sorted)

    when is_list(List) ->

    [F|_] = List,

    key_group(Pos, List, element(Pos,F), [], []).





key_group(_Pos, [], WorkKey, Work, Output) ->

    [{WorkKey, Work}] ++ Output;





key_group(Pos, [Item|Rem], WorkKey, Work, Output) ->

    NewKey = element(Pos, Item),

    case NewKey == WorkKey of

        true  ->
            key_group(Pos, Rem, WorkKey, [Item]++Work, Output);

        false ->
            key_group(Pos, Rem, NewKey,  [Item],       [{WorkKey,Work}]++Output)

    end.





% @todo swap argument order

%% @since Version 539
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Finds the 1-offset index of the first item in the list which passes the given predicate, or returns false if none pass. ```1> sc_lists:first_pos([a,b,c,d,2,f],fun erlang:is_integer/1).
%% 5
%%
%% 2> sc_lists:first_pos([a,b,c,d,e,f],fun erlang:is_integer/1).
%% false'''

first_pos(List, Predicate) ->

    first_pos(1, List, Predicate, false).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Finds the 1-offset index of the first item in the list which passes the given predicate, or returns a default value if none is found.  See {@link first_pos/2} for details.

first_pos(List, Predicate, Default) ->

    first_pos(1, List, Predicate, Default).





first_pos(_N, [],  _Pred, Default) ->

    Default;





first_pos(N, [Head|Tail], Pred, Default) ->

    case Pred(Head) of
        true  -> N;
        false -> first_pos(N+1, Tail, Pred, Default)
    end.





%% @since Version 541 TODO
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

split_at(N, List) ->

    split_at(N, N, List, [], []).





split_at(_N, _BlockN, [], Current, Work) ->

    lists:reverse([lists:reverse(Current)] ++ Work);





split_at(N, 0, Workload, Current, Work) ->

    split_at(N, N, Workload, [], [lists:reverse(Current)] ++ Work);





split_at(N, BN, [Item|Rem], Current, Work) ->

    split_at(N, BN-1, Rem, [Item]++Current, Work).





%% @since Version 543
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

is_postfix(Postfix, String) ->

    lists:prefix(lists:reverse(Postfix), lists:reverse(String)).





%% @since Version 544
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

reverse_map_filter(Workload, MapFun, FilterFun) ->

    reverse_map_filter(Workload, [], MapFun, FilterFun).





%% @since Version 544

reverse_map_filter([], Work, _MapFun, _FilterFun) ->

    Work;





%% @since Version 544

reverse_map_filter([Item|Rem], Work, MapFun, FilterFun) ->

    Res = MapFun(Item),

    case FilterFun(Res) of
        true  -> reverse_map_filter(Rem, [Res]++Work, MapFun, FilterFun);
        false -> reverse_map_filter(Rem, Work,        MapFun, FilterFun)
    end.





%% @since Version 545
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

reverse_filter(Workload, Fun) ->

    reverse_filter(Workload, [], Fun).





%% @since Version 545

reverse_filter([], Work, _Fun) ->

    Work;





%% @since Version 545

reverse_filter([Item|Rem], Work, Fun) ->

    case Fun(Item) of
        true  -> reverse_filter(Rem, [Item]++Work, Fun);
        false -> reverse_filter(Rem, Work,         Fun)
    end.





%% @since Version 546
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

reverse_map(Workload, Fun) ->

    reverse_map(Workload, [], Fun).





%% @since Version 546

reverse_map([], Work, _Fun) ->

    Work;





%% @since Version 546

reverse_map([Item|Rem], Work, Fun) ->

    reverse_map(Rem, [Fun(Item)]++Work, Fun).





% todo implement catching tuple { key, reqtype } from list, to auto-convert before return
% todo There may be a crashing bug here for repeated attributes, which are apparently legal, see http://fullof.bs/reading-module-attributes-in-erlang#comment-466
% todo It may help to re-implement this using proplists instead of doing it manually, profile

%% @todo document this
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @since Version 546

% interface

elements(Config, Requested)

    when is_list(Config), 
         is_list(Requested) ->

    elements_worker([], Config, Requested, 1).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

elements(Config, Requested, KeyIdx)

    when is_list(Config), 
         is_list(Requested), 
         is_integer(KeyIdx) ->

    elements_worker([], Config, Requested, KeyIdx);





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

elements(Config, Requested, strip)

    when is_list(Config), 
         is_list(Requested) ->

    elements_worker([], Config, Requested, 1, strip).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

elements(Config, Requested, KeyIdx, strip)

    when is_list(Config), 
         is_list(Requested), 
         is_integer(KeyIdx) ->

    elements_worker([], Config, Requested, KeyIdx, strip).





% implementation

elements_worker(Retlist, _, [], _) ->

    Retlist;





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





elements_worker(Retlist, _, [], _, strip) ->

    Retlist;





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





%% @since Version 547
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

differences(List)

    when is_list(List),
         length(List) >= 2 ->

    [ B-A || {A,B} <- all_neighbor_pairs(List) ].





%% @since Version 547
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

first_difference(List)

    when is_list(List),
         length(List) >= 2 ->

    differences(List).





%% @since Version 547
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

second_difference(List)

    when is_list(List),
         length(List) >= 3 ->

    differences(differences(List)).





%% @since Version 547
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

third_difference(List)

    when is_list(List),
         length(List) >= 4 ->

    differences(differences(differences(List))).





%% @since Version 547
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

% todo comeback implement the neato solver from http://www.y-maths.co.uk/sequen2.htm

nth_difference(0, List) ->

    List;





nth_difference(N, List)

    when is_list(List),
         length(List) > (N+1),
         N > 0               ->

    nth_difference(N-1, differences(List)).





% used for side effects, doesn't gather results; appropriate for enormous lists

% comeback

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @since Version 547

walk_unique_pairings([], _) ->

    ok;





walk_unique_pairings([A|R], F)

    when is_function(F) ->

    walk_unique_pairings(A, R, F),
    walk_unique_pairings(R, F).





walk_unique_pairings(_A, [],     _F) ->

    ok;





walk_unique_pairings( A, [Rh|Rr], F) ->

    F(A,Rh),
    walk_unique_pairings(A, Rr, F).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Counts the number of instances of Item in List.  ```1> TestData = lists:duplicate(40, [healthy, nonsmoker] ) ++
%%               lists:duplicate(10, [healthy, smoker]    ) ++
%%               lists:duplicate(7,  [cancer,  nonsmoker] ) ++
%%               lists:duplicate(3,  [cancer,  smoker]    ).
%%
%% [[healthy,nonsmoker], [healthy,nonsmoker], [healthy|...], [...]|...]
%%
%% 2> sc:count_of([healthy,smoker], TestData).
%% 10
%%
%% 3> sc:count_of([healthy,nonsmoker], TestData).
%% 40'''
%%
%% @since Version 550

-spec count_of(Item::any(), List::list()) -> non_neg_integer().

count_of(Item, List) ->

    lists:foldl(

        fun(X, Counter) ->

            case X of

                Item ->
                    Counter+1;

                _ ->
                    Counter

            end

        end,

        0,
        List

    ).





%% @equiv every_member_representation(Memberships, no_absence)
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

every_member_representation(Memberships) ->

    every_member_representation(Memberships, no_absence).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> For a list of memberships, return every possible combination of one representative member from each list.  The parameter `AllowAbsence' controls whether memberships may be unrepresented; if unrepresented memberships are possible, then one possible representation becomes the empty list. ```1> sc:every_member_representation([ [a,b],[1,2,3],[i,ii,iii] ], no_absence).
%% [[a,1,i], [a,1,ii], [a,1,iii], [a,2,i], [a,2,ii], [a,2,iii], [a,3,i], [a,3,ii], [a,3,iii], [b,1,i], [b,1,ii], [b,1,iii], [b,2,i], [b,2,ii], [b,2,iii], [b,3,i], [b,3,ii], [b,3,iii]]
%%
%% 2> sc:every_member_representation([ [a,b],[1,2],[i,ii] ], allow_absence).
%% [ [], [i], [ii], [1], [1,i], [1,ii], [2], [2,i], [2,ii], [a], [a,i], [a,ii], [a,1], [a,1,i], [a,1,ii], [a,2], [a,2,i], [a,2,ii], [b], [b,i], [b,ii], [b,1], [b,1,i], [b,1,ii], [b,2], [b,2,i], [b,2,ii] ]
%%
%% 3> Format = fun(Person, Place, Weapon) -> "It was " ++ Person ++ " in the " ++ Place ++ " with the " ++ Weapon ++ "!" end.
%% #Fun<erl_eval.18.105910772>
%%
%% 4> { People, Places, Weapons } = { ["Col. Mustard", "Mr. Green"], ["the billiards room", "the kitchen"], ["a lead pipe", "a knife", "a gun"] }.
%% {["Col. Mustard","Mr. Green"],
%%  ["the billiards room","the kitchen"],
%%  ["a lead pipe","a knife","a gun"]}
%%
%% 5> Places.
%% ["the billiards room","the kitchen"]
%%
%% 6> Format("Mrs. Scarlett", "the observatory", "a noose").
%% "It was Mrs. Scarlett in the the observatory with the a noose!"
%%
%% 7> EveryClueOutcome = [ Format(ThisPerson, ThisPlace, ThisWeapon) || ThisPerson <- People, ThisPlace <- Places, ThisWeapon <- Weapons ].
%% ["It was Col. Mustard in the the billiards room with the a lead pipe!",
%%  "It was Col. Mustard in the the billiards room with the a knife!",
%%  "It was Col. Mustard in the the billiards room with the a gun!",
%%  "It was Col. Mustard in the the kitchen with the a lead pipe!",
%%  "It was Col. Mustard in the the kitchen with the a knife!",
%%  "It was Col. Mustard in the the kitchen with the a gun!",
%%  "It was Mr. Green in the the billiards room with the a lead pipe!",
%%  "It was Mr. Green in the the billiards room with the a knife!",
%%  "It was Mr. Green in the the billiards room with the a gun!",
%%  "It was Mr. Green in the the kitchen with the a lead pipe!",
%%  "It was Mr. Green in the the kitchen with the a knife!",
%%  "It was Mr. Green in the the kitchen with the a gun!"]'''
%%
%% @since Version 551

-spec every_member_representation(Memberships::list_of_lists(), allow_absence|no_ansence) -> list_of_lists().

every_member_representation([], _) ->

    [[]];





every_member_representation( [Membership|RemMemberships], no_absence   ) ->

    [ [Member] ++ RemRep ||
        Member <- Membership,
        RemRep <- every_member_representation(RemMemberships, no_absence)
    ];





every_member_representation( [Membership|RemMemberships], allow_absence) ->

    Compact = fun(Member, RemRep) ->

        case Member of

            empty ->
                RemRep;

            {item,X} ->
                [X] ++ RemRep

        end

    end,

    [ Compact(Member, RemRep) ||
        Member <- [empty] ++ [{item,X}||X<-Membership],
        RemRep <- every_member_representation(RemMemberships, allow_absence)
    ].





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns every interpretation of the list as a set of boolean flags, including all-off and all-on. ```1> sc:every_flag_representation([1,2,3,4]).
%% [ [], [4], [3], [3,4], [2], [2,4], [2,3], [2,3,4], [1], [1,4], [1,3], [1,3,4], [1,2], [1,2,4], [1,2,3], [1,2,3,4] ]
%%
%% 2> length(sc:every_flag_representation(lists:seq(1,16))).
%% 65536
%%
%% 3> SourceOfPowers = sc:every_flag_representation([magic,technology,evil,alien]).
%% [[],                              % Batman
%%  [alien],                         % Superman
%%  [evil],                          % Darkseid
%%  [evil,alien],                    % Sinestro
%%  [technology],                    % Mister Terrific (Michael Holt)
%%  [technology,alien],              % The Blue Beetle
%%  [technology,evil],               % The OMACs
%%  [technology,evil,alien],         % Braniac
%%  [magic],                         % Shazam
%%  [magic,alien],                   % Green Lantern (Alan Scott)
%%  [magic,evil],                    % Lucifer Morningstar
%%  [magic,evil,alien],              % pre-crisis Star Sapphire
%%  [magic,technology],              % Alexander Luthor Jr.
%%  [magic,technology,alien],        % Mister Miracle
%%  [magic,technology,evil],         % pre-crisis Sinestro
%%  [magic,technology,evil,alien]]   % Granny Goodness'''
%%
%% @since Version 552

-spec every_flag_representation(Flags::list()) -> list_of_lists().

every_flag_representation([]) ->

    [[]];





every_flag_representation([Flag|RemFlags]) ->

    [ MaybeFlag ++ Reps ||
        MaybeFlag <- [[],[Flag]],
        Reps      <- every_flag_representation(RemFlags)
    ].





%% @since Version 553
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

module_abstract_representation(Module) ->

    module_abstract_representation(Module, unstripped).





%% @since Version 553
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

module_abstract_representation(Module, DoStrip) ->

    case module_feature(Module, abstract_code) of

        { raw_abstract_v1, ACode } ->

            case DoStrip of

                stripped ->
                    ACode;

                unstripped ->
                    { raw_abstract_v1, ACode }

            end;

        no_abstract_code ->

            { error, "ScUtil's abstract code functions require that a module be compiled with debug_info enabled, eg 'c(" ++ atom_to_list(Module) ++ ",[debug_info]).'" }

    end.





%% @since Version 556
%%
%% @doc <span style="color:red;font-style:italic;font-weight:bold">DANGEROUS</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> modified from http://www.trapexit.org/String_Eval

eval(S) ->

    eval(S,erl_eval:new_bindings()).





%% @since Version 556
%%
%% @doc <span style="color:red;font-style:italic;font-weight:bold">DANGEROUS</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> from http://www.trapexit.org/String_Eval

eval(S, Environ) ->

    {ok, Scanned,_} = erl_scan:string(S),
    {ok, Parsed}    = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed,Environ).





%% @todo use test data at http://changingminds.org/explanations/research/analysis/kendall.htm
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Compute the Kendall Tau Rank Correlation Coefficient of a list of coordinate tuples. ```1> sc:kendall([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {tau,1.0}
%%
%% 2> sc:kendall([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {tau,-1.0}
%%
%% 3> sc:kendall([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {tau,1.0}
%%
%% 4> sc:kendall([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {tau,1.0}
%%
%% 5> sc:kendall([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {tau,1.0}'''
%%
%% @since Version 557

-spec kendall_correlation(TupleList::coord_list()) -> { tau, Correlation::number() }.

kendall_correlation(TupleList)

    when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    kendall_correlation(A,B).





%% @equiv kendall(lists:zip(List1, List2))

kendall_correlation(List1, _)

    when length(List1) < 2 ->

    {tau, 0.0};





kendall_correlation(List1, List2) 

    when length(List1) /= length(List2) ->

    {error, "For the Kendall correlation, the input lists must be same length."};





kendall_correlation(List1, List2) 

    when is_list(List1), 
         is_list(List2) ->

    {RA,_} = lists:unzip(tied_ordered_ranking(List1)),
    {RB,_} = lists:unzip(tied_ordered_ranking(List2)),

    Ordering = lists:keysort(1, lists:zip(RA,RB)),
    {_,OrdB} = lists:unzip(Ordering),

    N = length(List1),
    P = lists:sum(kendall_right_of(OrdB, [])),

    {tau, -(( (4*P) / (N * (N - 1))) - 1) }.





%% @private

kendall_right_of([], Work) ->

    lists:reverse(Work);





kendall_right_of([F|R], Work) ->

    kendall_right_of(R, [kendall_right_of_item(F,R)]++Work).





kendall_right_of_item(B, Rem) ->

    length([R || R <- Rem, R < B]).





%% @todo use test data at http://geographyfieldwork.com/SpearmansRank.htm
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Compute the Spearman's Rank Correlation Coefficient of a list of coordinate tuples. ```1> sc:spearman([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {rsquared,1.0}
%%
%% 2> sc:spearman([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {rsquared,-1.0}
%%
%% 3> sc:spearman([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {rsquared,0.5}
%%
%% 4> sc:spearman([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {rsquared,1.0}
%%
%% 5> sc:spearman([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {rsquared,1.0}'''
%%
%% @since Version 558

-spec spearman_correlation(TupleList::coord_list()) -> { rsquared, Correlation::number() }.

spearman_correlation(TupleList)

    when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    spearman_correlation(A,B).





%% @equiv spearman_correlation(lists:zip(List1, List2))

spearman_correlation(List1, _)

    when length(List1) < 2 ->

    {rsquared, 0.0};





spearman_correlation(List1, List2)

    when length(List1) /= length(List2) ->

    {error, "For the Spearman correlation, the input lists must be the same length."};





spearman_correlation(List1, List2)

    when is_list(List1),
         is_list(List2) ->

    {TR1,_} = lists:unzip(simple_ranking(List1)),
    {TR2,_} = lists:unzip(simple_ranking(List2)),

    Numerator   = 6 * lists:sum([ square(D1-D2) || {D1,D2} <- lists:zip(TR1,TR2) ]),
    Denominator = math:pow(length(List1),3)-length(List1),

    {rsquared, 1-(Numerator/Denominator) }.





%% @todo use test data at http://changingminds.org/explanations/research/analysis/pearson.htm
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Compute the Pearson Correlation Coefficient of a list of coordinate tuples. ```1> sc_correlate:pearson([ {1,1}, {2,2}, {3,3}, {4,4}, {5,5} ]).
%% {r,1.0}
%%
%% 2> sc_correlate:pearson([ {1,5}, {2,4}, {3,3}, {4,2}, {5,1} ]).
%% {r,-1.0}
%%
%% 3> sc_correlate:pearson([ {1,3}, {2,3}, {3,3}, {4,3}, {5,3} ]).
%% {r,0.0}
%%
%% 4> sc_correlate:pearson([ {1,2}, {2,2.5}, {3,3}, {4,3.5}, {5,4} ]).
%% {r,1.0}
%%
%% 5> sc_correlate:pearson([ {1,2}, {2,2.4}, {3,3}, {4,3.6}, {5,4} ]).
%% {r,0.9970544855015818}'''
%%
%% @since Version 559

-spec pearson_correlation(TupleList::coord_list()) -> { r, Correlation::number() }.

pearson_correlation(TupleList)

    when is_list(TupleList) ->

    {A,B} = lists:unzip(TupleList),
    pearson_correlation(A,B).





%% @equiv pearson(lists:zip(List1, List2))

pearson_correlation(List1, _) 

    when length(List1) < 2 ->

    {r, 0.0};





pearson_correlation(List1, List2) 

    when length(List1) /= length(List2) ->

    {error, "For the Pearson correlation, the input lists must be the same length."};





pearson_correlation(List1, List2)

    when is_list(List1),
         is_list(List2) ->

    SumXY = lists:sum([A*B || {A,B} <- lists:zip(List1,List2) ]),   % the sum of the products of each matched pair

    SumX  = lists:sum(List1),
    SumY  = lists:sum(List2),

    SumXX = lists:sum([L*L || L<-List1]),                           % the sums of the squared items
    SumYY = lists:sum([L*L || L<-List2]),

    N     = length(List1),

    case math:sqrt(   ( (N*SumXX)-(SumX*SumX) )   *   ( (N*SumYY)-(SumY*SumY) )   ) of

        0 ->
            {r, 0.0};  % some nasty value sets otherwise cause divide by zero


        0.0 ->
            {r, 0.0};  % eg [ [1,1,1,1,1], [1,1,2,1,2] ]

        Denom ->
          Numer = (N*SumXY) - (SumX * SumY),
          {r, (Numer/Denom)}

    end.





%% @todo comeback make a simple/2 which takes a sorting predicate
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns a ranked ordering of the list without tie rankings.  ```1> sc_rank:simple([10,90,20,80,30,70,40,60,50]).
%% [{1,90}, {2,80}, {3,70}, {4,60}, {5,50}, {6,40}, {7,30}, {8,20}, {9,10}]
%%
%% 2> sc_rank:simple([10,10,10,10]).
%% [{1,10},{2,10},{3,10},{4,10}]'''
%%
%% @since Version 560

-spec simple_ranking(Values::numeric_list()) -> ranking_list().

simple_ranking(List)

    when is_list(List) ->

    lists:zip(lists:seq(1,length(List)),lists:reverse(lists:sort(List))).





%% @todo comeback make a tied/2 which takes a sorting predicate
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns a ranked ordering of the list with tie rankings.  As such, for uniformity, all rankings are floats.  Ties are represented as the centers of ranges. ```1> sc:tied([10,90,20,80,30,70,40,60,50]).
%% [{1.0,90}, {2.0,80}, {3.0,70}, {4.0,60}, {5.0,50}, {6.0,40}, {7.0,30}, {8.0,20}, {9.0,10}]
%%
%% 2> sc:tied([100,200,200,300]).
%% [{1.0,300},{2.5,200},{2.5,200},{4.0,100}]'''
%%
%% needs significant refactoring; work is being repeated
%%
%% @since Version 561

-spec tied_ranking(Values::numeric_list()) -> ranking_list().

tied_ranking(List) ->

    tied_rank_worker(simple_ranking(List), [], no_prev_value).





%% @private

tied_add_prev(Work, {FoundAt, NewValue}) ->

    lists:duplicate( length(FoundAt), {lists:sum(FoundAt)/length(FoundAt), NewValue} ) ++ Work.





%% @private

tied_rank_worker([], Work, PrevValue) ->

    lists:reverse(tied_add_prev(Work, PrevValue));





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





%% @todo comeback make a tied/2 which takes a sorting predicate
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns a tied ranked ordering of the list, ordered according to the input ordering rather than the sorted ordering.  As with {@link tied/1}, all rankings are floats, and ties are represented as the centers of ranges. ```1> sc:ordered([10,90,20,80,30,70,40,60,50]).
%% [{9.0,10}, {1.0,90}, {8.0,20}, {2.0,80}, {7.0,30}, {3.0,70}, {6.0,40}, {4.0,60}, {5.0,50}]
%%
%% 2> sc:ordered([100,200,200,300]).
%% [{4.0,100},{2.5,200},{2.5,200},{1.0,300}]'''
%%
%% @since Version 562

-spec tied_ordered_ranking(Values::numeric_list()) -> ranking_list().

tied_ordered_ranking(List)

    when is_list(List) ->

    tied_ordered_ranking(List, tied_ranking(List), []).





tied_ordered_ranking([], [], Work) ->

    lists:reverse(Work);





tied_ordered_ranking([Front|Rem], Ranks, Work) ->

    {value,Item}  = lists:keysearch(Front,2,Ranks),
    {IRank,Front} = Item,
    tied_ordered_ranking(Rem, Ranks--[Item], [{IRank,Front}]++Work).





%% @since Version 564
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands) ->

    halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, brief).





%% @since Version 564
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, brief) ->

    { Effort, _ } = halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, complete),
    Effort;





%% @since Version 564

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, complete) ->

    ProgramLength     = TotalOperators    + TotalOperands,
    ProgramVocabulary = DistinctOperators + DistinctOperands,

    Volume            = ProgramLength         * (math:log(ProgramVocabulary)),
    Difficulty        = (DistinctOperators/2) * (TotalOperands/DistinctOperands),

    Effort            = Volume * Difficulty,

    { Effort, [{volume, Volume}, {difficulty, Difficulty}, {program_length, ProgramLength}, {program_vocabulary, ProgramVocabulary}] }.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Convert a number to a radix string using a radix list of your specification and any size.  When appropriate, prefer the system provided `erlang:integer_to_list/2'.  Lists are accepted, but converted to tuples before use, so are inefficient.  ```1> sc_convert:integer_to_radix_list(1111, "0123456789abcdef").
%% "457"
%%
%% 2> sc_convert:integer_to_radix_list(1111, "0123456789").
%% "1111"
%%
%% 3> sc_convert:integer_to_radix_list(1234567890, "abcdefghij").
%% "bcdefghija"
%%
%% 4> sc_convert:integer_to_radix_list(12648430, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F}).
%% "C0FFEE"
%%
%% 5> sc_convert:integer_to_radix_list(1234567890, [alpha, beta, gamma, delta, epsilon, zeta, eta, theta, kappa, lambda]).
%% [beta,gamma,delta,epsilon,zeta,eta,theta,kappa,lambda,alpha]'''
%%
%% @since Version 566

-spec integer_to_radix_list(Number::number(), Radix::tuple()) -> list().

integer_to_radix_list(Number, RadixItems)

    when is_tuple(RadixItems) ->

    Radix = size(RadixItems),
    [ element(Ch+1, RadixItems) || Ch <- downshift_radix([], Number, Radix) ];





integer_to_radix_list(Number, RadixList)

    when is_list(RadixList) ->

    integer_to_radix_list(Number, list_to_tuple(RadixList)).





%% @since Version 567
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

downshift_radix(InStep, 0, _Radix) ->

    InStep;





downshift_radix(InStep, Number, Radix) ->

    downshift_radix([Number rem Radix]++InStep, trunc((Number-(Number rem Radix)) / Radix), Radix ).





%% @since Version 568
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Like binary_to_term, but not so much for binaries.  Thanks, dizzyd (modified for error reporting)

list_to_term(List) ->

    case catch erl_scan:string(List) of

        { ok, Tokens, _ } ->

            case erl_parse:parse_term( Tokens ++ [{ dot, 1 }] ) of
                { ok,Term } -> Term;
                Error       -> { error, Error }
            end;

        Error -> { error, Error }

    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Convert an io_list() to a hexstring().  ```1> sc:io_list_to_hex_string("a").
%% "61"
%%
%% 2> sc:io_list_to_hex_string("a08n408nbqa").
%% "6130386e3430386e627161"'''
%%
%% @since Version 569

-spec io_list_to_hex_string(Input::io_list()) -> hexstring().

io_list_to_hex_string(Input)

    when is_list(Input) ->

    io_list_to_hex_string(Input, []).





io_list_to_hex_string([], Work) ->

    lists:reverse(Work);





io_list_to_hex_string([Item|Remainder], Work) 

    when is_integer(Item), 
         Item >= 0, 
         Item =< 255     ->

    [A,B] = byte_to_hex(Item),
    io_list_to_hex_string(Remainder, [B,A]++Work);





io_list_to_hex_string(_, _) ->

    {error, not_an_io_list}.





-spec nybble_to_hex(Nyb::nybble()) -> hexchar().

%% @doc Convert a nybble() to a hexchar(). ```1> sc:nybble_to_hex(7).
%% 55
%%
%% 2> sc:nybble_to_hex(15).
%% 102'''
%%
%% Exhaustively unit tested.

%% @since Version 570

nybble_to_hex(Nyb) 

    when is_integer(Nyb), 
         Nyb >= 0,  
         Nyb < 10       ->

    $0 + Nyb;





nybble_to_hex(Nyb)

    when is_integer(Nyb),
         Nyb >= 10,
         Nyb < 16       ->

    $a + Nyb - 10.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Convert a byte() into a hexstring().  The hexstring() result will always be two characters (left padded with zero if necessary). ```1> sc:byte_to_hex(7).
%% "07"
%%
%% 2> sc:byte_to_hex(255).
%% "ff"'''
%%
%% @since Version 571

-spec byte_to_hex(TheByte::byte()) -> hexstring().

byte_to_hex(TheByte)

    when is_integer(TheByte),
         TheByte >= 0,
         TheByte =< 255     ->

    [ nybble_to_hex(TheByte bsr 4), nybble_to_hex(TheByte band 15) ].





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Convert a hexstring() or hexchar() into its numeric value. ```1> sc:hex_to_int("c0ffEE").
%% 12648430
%%
%% 2> sc:hex_to_int($e).
%% 14
%%
%% 3> sc:hex_to_int("100").
%% 256'''
%%
%% @since Version 572

-spec hex_to_int(Hex::hexstring() | hexchar()) -> integer().

hex_to_int(Hex) 

    when is_integer(Hex), 
         Hex >= $0, 
         Hex =< $9      -> 
         
    Hex - $0;
    
    



hex_to_int(Hex) 

    when is_integer(Hex), 
         Hex >= $a, 
         Hex =< $f      -> 
         
    Hex - $a + 10;
    
    
    
    

hex_to_int(Hex) 

    when is_integer(Hex), 
         Hex >= $A, 
         Hex =< $F      -> 
         
    Hex - $A + 10;

    



hex_to_int(Hex) 

    when is_list(Hex) ->

    hex_to_int(Hex, 0).





hex_to_int([], Acc) -> 

    Acc;

    
    
    

hex_to_int([Digit|Rem], Acc) -> 

    hex_to_int(Rem, (Acc bsl 4) + hex_to_int(Digit)).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Converts a list into a number; integers will be returned if there is no mantissa in the list representation. ```1> sc:list_to_number("2").
%% 2
%%
%% 2> sc:list_to_number("2.0").
%% 2.0
%%
%% 3> sc:list_to_number("2.1").
%% 2.1'''
%%
%% @since Version 574

-spec list_to_number(X::list()) -> number().

list_to_number(X) ->

    case catch list_to_float(X) of

        {'EXIT',_} ->
            list_to_integer(X);

        Y ->
            Y

    end.





%% @todo more distance types - manhattan, malahabanois, etc
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the distance between two coordinates in any N-space.  In two dimensions, this is known as the Pythagorean theorem.  The coordinates may be of any positive integer dimensionality (2d, 3d, but no -1d or 2.5d), but both coordinates must be of the same dimensionality.  The coordinates may have real-valued or negative components, but imaginary math is not implemented.  This function tolerates tuple coordinates by converting them to lists; list coordinates are thus slightly faster. ```1> sc:distance([0,0],[1,1]).
%% 1.4142135623730951
%%
%% 2> sc:distance({0,0},[-1,1.0]).
%% 1.4142135623730951
%%
%% 3> sc:distance([0,0,0,0],[1,-1,1,-1]).
%% 2.0'''
%%
%% @since Version 575

-spec euclidean_distance(Coordinate1::coord(), Coordinate2::coord()) -> number().

euclidean_distance(C1, C2)

    when is_tuple(C1) ->

    euclidean_distance( tuple_to_list(C1), C2 );





euclidean_distance(C1, C2)

    when is_tuple(C2) ->

    euclidean_distance( C1, tuple_to_list(C2) );





euclidean_distance(C1, C2) ->

    % squaring makes taking the absolute value to get unsigned magnitude redundant; that's not an omission, it's an optimization
    math:sqrt(
        lists:sum(
            [ sc:square(A-B) ||
                {A,B} <- sc:zip_n([C1,C2])
            ]
        )
    ).





%% @todo docs
%%
%% @since Version 576

merge_settings(S1, S2)

    when is_list(S1),
         is_list(S2) ->

    lists:ukeymerge(1, lists:ukeysort(1, S1), lists:ukeysort(1, S2) ).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> (Blocking) First send a message to an entity.  Then pop the front of the message queue and return it as `{item,X}'; block.  ```1> sc:send_receive(self(), message).
%% {item,message}'''
%%
%% @since Version 578

-spec send_receive(ToWhom::pid()|atom(), What::any()) -> { item, any() }.

send_receive(ToWhom, What) ->

    ToWhom ! What,

    receive X ->
        { item, X }
    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> (Non-Blocking) First send a message to an entity.  Then pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> sc:send_receive(self(), message).
%% {item,message}'''
%%
%% @since Version 579

-spec send_receive(ToWhom::pid()|atom(), What::any(), HowLong::non_neg_integer()|infinity) -> { item, any() } | nothing_there.

send_receive(ToWhom, What, HowLong) ->

    ToWhom ! What,

    receive X ->
        { item, X }
    after HowLong ->
        nothing_there
    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> (Blocking) First send a message to an entity.  Then pop the first message queue item matching the mask as a 2-tuple, and return it as `{Mask,X}'; block.  ```1> sc:send_receive(self(), message).
%% {item,message}'''
%%
%% @since Version 580

-spec send_receive_masked(Mask::any(), ToWhom::pid()|atom(), What::any()) -> { Mask::any(), any() }.

send_receive_masked(Mask, ToWhom, What) ->

    ToWhom ! What,

    receive { Mask, X } ->
        { Mask, X }
    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> (Non-Blocking) First send a message to an entity.  Then pop the front of the message queue and return it as `{Mask,X}', or return nothing_there for empty queues; do not block.  ```1> sc:send_receive(self(), message).
%% {item,message}'''
%%
%% @since Version 581

-spec send_receive_masked(Mask::any(), ToWhom::pid()|atom(), What::any(), HowLong::non_neg_integer()|infinity) -> { item, any() } | nothing_there.

send_receive_masked(Mask, ToWhom, What, HowLong) ->

    ToWhom ! What,

    receive { Mask, X } ->
        { Mask, X }
    after HowLong ->
        nothing_there
    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> sc:receive_one().
%% nothing_there
%%
%% 2> self() ! message.
%% message
%%
%% 3> sc:receive_one().
%% {item,message}
%%
%% 4> sc:receive_one().
%% nothing_there'''
%%
%% @since Version 582

-spec receive_one() -> { item, any() } | nothing_there.

receive_one() ->

    receive (X) ->
        { item, X }
    after 0 ->
        nothing_there
    end.





%% @since Version 586
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

union(L) ->

    lists:usort(lists:append(L)).





%% @since Version 586
%%
%% @equiv union( [L1, L2] )
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

union(L1, L2) ->

    union([L1,L2]).





%% @since Version 586
%%
%% @equiv union( [L1, L2, L3] )
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

union(L1, L2, L3) ->

    union([L1,L2,L3]).





%% @since Version 586
%%
%% @equiv union( [L1, L2, L3, L4] )
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

union(L1, L2, L3, L4) ->

    union([L1,L2,L3,L4]).

% todo test require that sort(union(powerset(A))) == sort(A)





%% @since Version 589
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

power_set(L)

    when is_list(L) ->

    Size = length(L),
    lists:append( [[[]]] ++ [ sc_lists:combinations(L,Sz) || Sz <- lists:seq(1,Size) ] ).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Return a list with the original list's shallow members in a random order.  Deep lists are not shuffled; `[ [a,b,c], [d,e,f], [g,h,i] ]' will never produce sublist reorderings (`[b,c,a]') or list mixing (`[b,g,e]'), only reordering of the three top level lists.  The output list will always be the same length as the input list.  Repeated items and mixed types in input lists are safe. ```1> sc:shuffle(lists:seq(1,9)).
%% [8,4,7,9,5,2,6,1,3]
%%
%% 2> {TheFaces, TheSuits} = {  [ace] ++ lists:seq(2,10) ++ [jack,queen,king],  [hearts,spades,clubs,diamonds]  }
%% {[ace,jack,queen,king,2,3,4,5,6,7,8,9,10],
%%  [hearts,spades,clubs,diamonds]}
%%
%% 3> Deck = sc:shuffle([ {Face,Suit} || Face <- TheFaces, Suit <- TheSuits ]).
%% [ {6,spades}, {7,hearts}, {8,clubs}, {queen,spades}, {6,diamonds}, {ace,...}, {...} | ...]
%%
%% 4> sc:shuffle([ duck,duck,duck,duck, goose ]).
%% [duck,goose,duck,duck,duck]'''
%%
%% <i>Originally found at <a href="http://wiki.trapexit.org/index.php/RandomShuffle">http://wiki.trapexit.org/index.php/RandomShuffle</a>; refactored for clarity, and unnecessary repeat nesting behavior removed.</i>
%%
%% @since Version 590

-spec shuffle(List::list()) -> list().

shuffle(List)

    when is_list(List) ->

    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),

    WeightedAndShuffled = lists:map(
        fun(Item) -> { random:uniform(), Item } end,
        List
    ),

    { _, SortedAndDeweighted } = lists:unzip(lists:keysort(1, WeightedAndShuffled)),

    SortedAndDeweighted.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Take a random single item from a list with weighted probabilities.  Probabilities may be any numeric type, and may be any non-negative value (items with zero probability will be omitted).  Input is a `weightlist()', which is a list in the form `[{Item,Probability}, {I2,P2}, ...]'. There is no requirement to normalize probabilities to any range, though probabilities normalized to ranges will still work as expected. ```1> sc:from([ {quad,4}, {double,2}, {single,1} ]).
%% quad
%%
%% 2> [ sc:from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,10) ].
%% [single,quad,quad,double,quad,double,quad,quad,quad,double]
%%
%% 3> sc:histograph([ sc:from_weighted([ {quad,4}, {double,2}, {single,1} ]) || X <- lists:seq(1,777777) ]).
%% [{double,222200},{quad,444165},{single,111412}]'''
%% @since Version 592

% InputList is [ {Item,Weight}, {Item,Weight}, ... ]

-spec random_from_weighted(InputList::weight_list()) -> any().

random_from_weighted(InputList)

    when is_list(InputList) ->

    RandomLimit = rand(
        lists:sum(
            [ Weight ||
                {_,Weight} <- InputList ]                                   % the random cap is equal to the sum of all the weights
            )
        ),

    random_from_weighted_worker(InputList, RandomLimit).                    % call the worker with the original list and the cap





% if the list is empty, the cap for randomness was calculated wrongly, and as such the random point is too high

random_from_weighted_worker([], _) ->

    { error, limit_miscalculation };





% but if the list has reasonable contents and the limit is a pos-or-0 integer

random_from_weighted_worker(InputList, Limit)

    when is_list(InputList),
         is_integer(Limit),
         Limit >= 0        ->

    [ {Item,Weight} | Remainder ] = InputList,                          % break off the input list's head as {I,W} and keep the rest as Remainder

    case Weight =< Limit of                                             % if the weight is less than or equal to the limit,

        true  ->
            random_from_weighted_worker(Remainder, Limit-Weight);       % recurse the next item with a decremented weight

        false ->
            Item                                                        % if not, this item is the one we want

    end.





%% @equiv from(1, List, no_remainder)
%% @since Version 593

random_from(List) ->

    [X] = random_from(1, List, no_remainder), X.





%% @equiv from(N, List, no_remainder)
%% @since Version 593

random_from(N, List) ->

    random_from(N, List, no_remainder).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Take N non-repeating random elements from a list in undefined order.  If the atom `remainder' is passed in as the third argument, the unused portion of the source list will be returned as the second member of a 2ary tuple with the results; the default is no_remainder, which only returns the result set.  Mixed type input lists are perfectly safe, and membership for random selection is shallow (ie, `[ [1,2], [3,4] ]' as an input list would only generate outputs of lists, never integers.)```1> sc:random_from([monday,tuesday,wednesday,thursday,friday]).
%% friday
%%
%% 2> sc:random_from(4, lists:seq(1,20)).
%% [6,3,15,12]
%%
%% 3> sc:random_from(3, [warrior, mage, cleric, thief, paladin, ranger, bard]).
%% [cleric,warrior,ranger]
%%
%% 4> sc:random_from(6, [mixed, [1,2,3], 4, {five,5}, 3, 67.2, <<"Hello">>, 8]).
%% [[1,2,3],{five,5},4,mixed,<<"Hello">>,67.2]
%%
%% 5> {Team1, Team2} = sc:random_from(3, [alice,bob,cathy,dave,edward,fawn], remainder).
%% {[cathy,fawn,dave],[bob,edward,alice]}
%%
%% 6> Team1.
%% [cathy,fawn,dave]
%%
%% 7> Where_Food = fun() -> sc:random_from([deli, fastfood, chinese, mexican, steakhouse, bistro, greek, indian, thai, sushi]) end.
%% #Fun<erl_eval.20.67289768>
%%
%% 8> Where_Food().
%% thai'''
%%
%% @since Version 593

-spec random_from(N::integer(), List::list(), remainder|no_remainder) -> list().

random_from(N, List, no_remainder) ->

    {R,_} = random_from(N,List,remainder), R;





random_from(N, List, remainder) ->

    lists:split(N, shuffle(List)).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns a pseudorandom integer on the range `[0 - (Range-1)]' inclusive.  Primarily just an auto-seeding wrapper. ```1> sc:rand(100).
%% 9
%%
%% 2> [ sc:rand(100) || X <- lists:seq(1,10) ].
%% [12,27,99,86,20,96,28,36,28,15]
%%
%% 3> sc:histograph([ sc:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,992}, {1,990}, {2,992}, {3,1033}, {4,1017}, {5,1003}, {6,996}, {7,1024}, {8,969}, {9,984}]
%%
%% 4> sc:histograph([ sc:rand(10) || X <- lists:seq(1,10000) ]).
%% [{0,1028}, {1,979}, {2,934}, {3,970}, {4,1035}, {5,1007}, {6,986}, {7,1012}, {8,1052}, {9,997}]'''
%%
%% @since Version 595

-spec rand(Range::integer()) -> integer().

rand(Range) ->

    case is_rand_seeded() of

        false ->
            srand(),
            rand(Range);

        true ->
            sc:floor(Range * random:uniform())

    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the clock, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want to know what seed was used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> sc:srand().
%% {ok,{seeded,{1227,902172,685000}}}
%%
%% 2> sc:srand().
%% {ok,{seeded,{1227,902173,231000}}}'''
%%
%% @since Version 598
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

-spec srand() -> { ok, { seeded, {integer(),integer(),integer()} } }.

srand() ->

    NewSeed = tuple_to_list(erlang:now()),
    erlang:apply(random,seed,NewSeed),
    { ok, { seeded, NewSeed }}.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> <i style="color:#888">(Called automatically)</i> Instantiates the random source, destroying a prior source if needed, and seeds the source with the three integer seed you provide, returning the seed used.  Generally speaking, you do not need this function; this is used manually when you want set what seed is used, for purposes of recreating identical pseudorandom sequences.  Otherwise, rand() will call this once on its own.  <em style="color:#a00;font-weight:bold">Because the scutil random system spawns a utility process to maintain random state, this function should be considered to have side effects for purposes of testing.</em> (Indeed, in a sense, this function's entire purpose is to cause a side effect.) ```1> sc:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}
%%
%% 2> sc:srand().
%% {ok,{seeded,{1227,902568,604600}}}
%%
%% 3> sc:srand(1,2,3).
%% {ok,{seeded,{1,2,3}}}'''
%%
%% @since Version 598
%% @todo migrate to labelled random generators, so that concurrent generators do not necessarily interfere with one another

-spec srand(A::integer(), B::integer(), C::integer()) -> { ok, { seeded, {integer(),integer(),integer()} } }.

srand(A,B,C) ->

    random:seed(A,B,C),
    { ok, { seeded, {A,B,C} } }.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Return a Count-length list of non-repeating coordinates in a grid of specified size; useful for feature generation.
%%
%% @todo @comeback give code examples (edoc was failing here?)
%%
%% @since Version 599

-spec grid_scatter(Count::integer(), Size::grid_size()) -> coord_list().

grid_scatter(0, []) ->

    []; % skips a lot of work





grid_scatter(Count, {SizeX, SizeY}) ->

    sc_random:from(
        Count,
        [ {X,Y} ||
            X <- lists:seq(1,SizeX),
            Y <- lists:seq(1,SizeY)
        ]
    );





grid_scatter(Count, Size) ->

    grid_scatter(Count, {Size, Size}).





%% @since Version 606
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @equiv key_extrema(1, List)

key_extrema(List) ->

    key_extrema(1, List).





%% @since Version 601
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

key_extrema(Pos, [L|_] = List) ->

    key_extrema(Pos, List, L, L).





key_extrema(_Pos, [], Lowest, Highest) ->

    {Lowest, Highest};





key_extrema(Pos, [Cur|Rem], false, false) ->

    key_extrema(Pos, Rem, Cur, Cur);





key_extrema(Pos, [Cur|Rem], Lowest, Highest) ->

    if

        element(Pos, Cur) < element(Pos, Lowest) ->
            key_extrema(Pos, Rem, Cur,    Highest);

        element(Pos, Cur) > element(Pos, Highest) ->
            key_extrema(Pos, Rem, Lowest, Cur);

        true ->
            key_extrema(Pos, Rem, Lowest, Highest)

    end.





%% @since Version 605
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @equiv key_max(1, List)

key_max(List) ->

    key_max(1, List).





%% @since Version 604
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

key_max(Pos, [L|_] = List) ->

    key_max(Pos, List, L).





%% @since Version 604

key_max(_Pos, [], Best) ->

    Best;





%% @since Version 604

key_max(Pos, [Cur|Rem], false) ->

    key_max(Pos, Rem, Cur);





%% @since Version 604

key_max(Pos, [Cur|Rem], Item) ->

    case element(Pos, Cur) > element(Pos, Item) of
        true  -> key_max(Pos, Rem, Cur);
        false -> key_max(Pos, Rem, Item)
    end.





%% @since Version 607
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @equiv key_min(1, List)

key_min(List) ->

    key_min(1, List).





%% @since Version 607
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

key_min(Pos, [L|_]=List) ->

    key_min(Pos, List, L).





%% @since Version 607

key_min(_Pos, [], Best) ->

    Best;





%% @since Version 607

key_min(Pos, [Cur|Rem], false) ->

    key_min(Pos, Rem, Cur);





%% @since Version 607

key_min(Pos, [Cur|Rem], Item) ->

    case element(Pos, Cur) < element(Pos, Item) of
        true  -> key_min(Pos, Rem, Cur);
        false -> key_min(Pos, Rem, Item)
    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the sum of the numeric elements of a tuple, treating non-numeric elements as zero. ```1>'''
%%
%% @since Version 609

-spec tuple_sum(T::numeric_tuple()) -> number().

tuple_sum(T)

    when is_tuple(T) ->

    tuple_sum(T, 1, size(T), 0).





tuple_sum(_T, Which, Max, Work)

    when Which > Max ->

    Work;





tuple_sum( T, Which, Max, Work) ->

     tuple_sum(T, Which+1, Max, Work+element(Which, T)).





% huhu dirty

%% @since 610
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

tuple_sort(T)

    when is_tuple(T) ->

    list_to_tuple(lists:sort(tuple_to_list(T))).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Checks whether E is a member element of tuple T, analogous to `lists::member(E, L)'. ```1> sc:tuple_member(b, {a,b,c}).
%% true
%%
%% 2> sc:tuple_member(d, {a,b,c}).
%% false
%%
%% 3> sc:tuple_member([1,2], {[1,2]}).
%% true'''
%%
%% @since Version 615

-spec tuple_member(E::any(), T::tuple()) -> true | false.

tuple_member(E, T) ->

    tuple_member(E, T, 1, size(T)).





tuple_member(_E,_T, I, Sz)

    when I > Sz -> false;





tuple_member(E, T, I, Sz) ->

    case element(I, T) == E of

        true  ->
            true;

        false ->
            tuple_member(E, T, I+1, Sz)

    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> <span style="color:red">TODO: Needs Example</span> Checks whether E is a member element of record R, analogous to `lists::member(E, L)'.  This function does not have examples because the shell does not correctly handle records; <span style="color:red">todo: add examples later</span>
%%
%% @since Version 616

-spec record_member(E::any(), R::record()) -> true | false.

record_member(E, R) ->

    tuple_member(E, R, 2, size(R)).  % just skip the 1st elem





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @since Version 617

get_linked_processes() ->

    [U] = [
              V
          ||
              {links,V} <- process_info(self())
          ],

    U.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @since Version 635

start_register_if_not_running(Name, FunctionLambda) ->

    case whereis(Name) of

        undefined ->
            P = spawn(FunctionLambda),
            register(Name, P),
            { ok, P };

        P ->
            { ok, P }

    end.






%% @equiv start_register_if_not_running(node(), Name, Module, Function, [])
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @since Version 618

start_register_if_not_running(Name, Module, Function) ->

    start_register_if_not_running(node(), Name, Module, Function, []).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>
%%
%% @equiv start_register_if_not_running(node(), Name, Module, Function, Args)

start_register_if_not_running(Name, Module, Function, Args) ->

    start_register_if_not_running(node(), Name, Module, Function, Args).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Check whether a process is registered locally, and if not, spawn it with a give function and arguments.  ```1> whereis(test).
%% undefined
%%
%% 2> sc:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% { ok, <0.726.0> }
%%
%% 3> whereis(test).
%% <0.726.0>
%%
%% 4> test ! terminate.
%% terminate
%%
%% 5> whereis(test).
%% undefined
%%
%% 6> sc:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% { ok, <0.731.0> }
%%
%% 7> whereis(test).
%% <0.731.0>
%%
%% 8> sc:start_register_if_not_running(node(), test, scutil, wait_until_terminate, []).
%% { ok, <0.731.0> }
%%
%% 9> whereis(test).
%% <0.731.0>'''
%%
%% @since Version 618

-spec start_register_if_not_running(Node::atom(), Name::atom(), Module::atom(), Function::atom(), Args::list()) -> pid() | ok.

start_register_if_not_running(Node, Name, Module, Function, Args)

    when is_atom(Name),
         is_atom(Module),
         is_atom(Function),
         is_list(Args)    ->

    case whereis(Name) of

        undefined ->
            P = spawn(Node, Module, Function, Args),
            register(Name, P),
            { ok, P };

        P ->
            { ok, P }

    end.





%% @equiv multi_do(C,M,F,[])
%% @since Version 620

multi_do(C, Module, Func) ->

    multi_do(C, Module, Func, [],   []).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Take an iteration count, a module name, a function name and an argument list, and repeatedly apply the argument list to the module/function, count times.  This is primarily useful with nondeterministic functions whose result might change despite identical arguments, such as functions with random behavior; for example, this function is invoked to implement stochastic testing in <a href="http://testerl.com/">TestErl</a>. ```1> sc:multi_do(10, scutil, rand, [100]).
%% [9,94,4,82,77,44,89,19,45,92]
%%
%% 2> sc:multi_do(10, scutil, rand, [10000]).
%% [2377,2559,1713,8489,4468,3261,3344,3751,380,2525]'''
%%
%% @since Version 620

-spec multi_do(Count::integer(), Module::atom(), Function::atom(), Args::list()) -> list().

multi_do(C, Module, Func, Args) ->

    multi_do(C, Module, Func, Args, []).





%% @private

multi_do(0,_Module,_Func,_Args, Work) ->

    Work;





%% @private

multi_do(I, Module, Func, Args, Work) ->

    multi_do(I-1, Module, Func, Args, Work ++ [apply(Module, Func, Args)]).





%% @since Version 621
%%
%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Append strings with separating string inbetween - contrast {@link explode/2}. ```1> sc:implode(",", ["a", "b", "c"]).
%% "a,b,c"
%%
%% 2> sc:implode(",", ["ab", "cd", "ef"]).
%% "ab,cd,ef"
%%
%% 3> sc:implode(",", ["", "", ""]).
%% ",,"
%%
%% 4> sc:implode("-wop ", ["do", "do", "do"]).
%% "do-wop do-wop do"
%%
%% 5> sc:implode("", ["", "", ""]).
%% []'''
%%
%% thanks for a much better implementation, etnt

implode(Separator, Data)

    when is_list(Data),
         is_list(Separator) ->

    lists:append(
        lists:foldr(

            fun(Item, [])  -> [Item];
               (Item, Acc) -> [Item] ++ [Separator] ++ Acc
            end,

            "",
            Data

        )
    ).





%% @since Version 622

%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Split any list by any other list, typically a string by a substring. The seperator list must not be empty. ```1> sc:explode(",", "1,2,5,10,20").
%% ["1","2","5","10","20"]
%%
%% 2> sc:explode(" ", "John Jacob Jingleheimer Schmidt").
%% ["John","Jacob","Jingleheimer","Schmidt"]
%%
%% 3> sc:explode("; ", "North; East; South; West").
%% ["North","East","South","West"]
%%
%% 4> sc:explode("No such delimiter", "I am the monarch of the sea").
%% ["I am the monarch of the sea"]
%%
%% 5> sc:explode(",", ",,").
%% ["", "", ""]
%%
%% 6> sc:explode([beta], [alpha, beta, gamma, delta, beta, epsilon]).
%% [[alpha],[gamma,delta],[epsilon]]
%%
%% 7> catch sc:explode([], [alpha, beta, gamma, delta, beta, epsilon]).
%% {'EXIT',{badarg,[{sc,explode,2},
%%                  {erl_eval,do_apply,5},
%%                  {erl_eval,expr,5},
%%                  {shell,exprs,7},
%%                  {shell,eval_exprs,7},
%%                  {shell,eval_loop,3}]}}
%%
%% 8> catch sc:explode(beta, [alpha, beta, gamma, delta, beta, epsilon]).
%% {'EXIT',{badarg,[{sc,explode,2},
%%                  {erl_eval,do_apply,5},
%%                  {erl_eval,expr,5},
%%                  {shell,exprs,7},
%%                  {shell,eval_exprs,7},
%%                  {shell,eval_loop,3}]}}'''
%%
%% Unit and doc tested.

-spec explode(Seperator::list(), Term::list()) -> list(list()).

% comeback todo whargarbl this is horrible and has to go

explode(Separator, Term) when is_binary(Term) ->

    [ list_to_binary(Res) || Res <- explode(binary_to_list(Separator), binary_to_list(Term))];






explode(Seperator, _Term) when not is_list(Seperator) ->

    error(badarg);






explode(_Seperator, Term) when not is_list(Term) ->

    error(badarg);






explode("", _Term) ->

    error(badarg);





explode(Separator, Term) ->

    explode(Separator, Term, [], [], -1,  0).





%% @since Version 622
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>





% comeback todo whargarbl this is horrible and has to go

explode(Separator, Term, Max) when is_binary(Term) ->

    [ list_to_binary(Res) || Res <- explode(binary_to_list(Separator), binary_to_list(Term), Max)];






explode(Separator, Term, Max) ->

    explode(Separator, Term, [], [], Max, 0).





%% @private
%% @since Version 622

explode(_Separator, [], Pass, Out, _Max, _Cur) ->

    Out ++ [Pass];





%% @private
%% @since Version 622

explode(Separator, Remainder, Pass, Out, -1, _Cur) -> % ignore cap

    case starts_with(Remainder, Separator) of

        false ->

            [ThisChar | Following] = Remainder,
            explode(Separator, Following, Pass ++ [ThisChar], Out, -1, 0);

        { true, LeftOver } ->

            explode(Separator, LeftOver, [], Out ++ [Pass], -1, 0)

    end;





%% @private
%% @since Version 622

explode(Separator, Remainder, Pass, Out, Max, Cur) -> % check cap

    if
        Cur+1 >= Max ->
            Out ++ [Remainder];

        true ->

            case starts_with(Remainder, Separator) of

                false ->

                    [ThisChar | Following] = Remainder,
                    explode(Separator, Following, Pass ++ [ThisChar], Out, Max, Cur);

                { true, LeftOver } ->

                    explode(Separator, LeftOver, [], Out ++ [Pass], Max, Cur+1)

            end

    end.





%% @since Version 624
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

% comeback todo whargarbl replace this; this is awful

starts_with(BR, BL) when is_binary(BR), is_binary(BL) ->

    starts_with(binary_to_list(BR), binary_to_list(BL));





starts_with(Remain, []) ->

    { true, Remain };





starts_with([], _) ->

    false;





starts_with([ MHead | MRemain ], [ PHead | PRemain ]) ->

    if

        PHead /= MHead ->
            false;

        true ->
            starts_with(MRemain, PRemain)

    end.





%% @since Version 625
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

is_numeric_string(Str) ->

    is_numeric_string(Str, decimal).





%% @since Version 625
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

is_numeric_string(Str, decimal) ->

    lists:all({scutil,is_numeric_char}, Str).





%% @since Version 626
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Parses a string on all three newline types, discarding any empty lines; applies F as a functor to each line,
%% and returns the tuple of the remainder and then a list of all results from the functor(s) issued
%%
%% thanks ayrnieu


map_scanline(F,L) ->

    {R,M} = lists:foldl(
        fun
            ( C, R = {[],_} ) when C == $\r orelse C == $\n -> R;
            ( C, {S,M}      ) when C == $\r orelse C == $\n -> { [], [ F(lists:reverse(S)) | M ] };
            ( C, {S,M}      )                               -> { [C|S], M }
        end,
        {[],[]}, L
    ),

    {lists:reverse(R), lists:reverse(M)}.





%% @since Version 626
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Third argument passes argument list as secondary argument to the functor; useful for passing ancillary state
%%
%% Modified from map_scaline/2 by ayrnieu

map_scanline(F,L,A) ->

    {R,M} = lists:foldl(
        fun
            ( C, R = {[],_} ) when C == $\r orelse C == $\n -> R;
            ( C, {S,M}      ) when C == $\r orelse C == $\n -> { [], [ F(lists:reverse(S), A) | M ] };
            ( C, {S,M}      )                               -> { [C|S], M }
        end,
        {[],[]}, L
    ),

    {lists:reverse(R), lists:reverse(M)}.





%% @since Version 626
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>, by fredrik svensson and adam lindberg, from http://www.merriampark.com/lderlang.htm

levenshtein(Same, Same)

    when is_list(Same) ->

    0;





levenshtein(String, [])

    when is_list(String) ->

    length(String);





levenshtein( [], String)

    when is_list(String) ->

    length(String);





levenshtein(Source, Target)

    when is_list(Source),
         is_list(Target) ->

    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).





%% @doc Recurses over every character in the source string and calculates a list of distances
%%
%% @private

levenshtein_rec( [SrcHead|SrcTail], Target, DistList, Step) ->

    levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);





levenshtein_rec( [], _, DistList, _) ->

    lists:last(DistList).





%% @doc Generates a distance list with distance values for every character in the target string
%%
%% @private

levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist)

    when length(DLT) > 0 ->

    Min = lists:min( [LastDist + 1, hd(DLT) + 1, DLH + lev_dif(TargetHead, SourceChar)] ),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);





levenshtein_distlist([], _, _, NewDistList, _) ->

    NewDistList.



%% @doc Calculates the difference between two characters or other values
%%
%% @private

lev_dif( C,   C ) -> 0;
lev_dif(_C1, _C2) -> 1.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Sanitize an arbitrary string to be appropriate for Windows and Unix filesystems, and URLs. ```1> sc:sanitize_filename("\h/e~l%lo! w^o@r#l*d.").
%% "helloworld"'''
%%
%% @see sanitize_tokens/2
%%
%% @since Version 628

-spec sanitize_filename(Filename::string()) -> string().

sanitize_filename(Filename) ->

    sanitize_tokens(

        Filename,

        lists:seq($a,$z) ++
         lists:seq($A,$Z) ++
         lists:seq($0,$9) ++
         "-_()[]"
    ).





% todo comeback

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

circ_within_origin_circ({sc_circle, OX, OY, OR}, {sc_circle, CX, CY, CR}) ->

    (sc:euclidean_distance({OX,OY}, {CX,CY}) + CR) =< OR.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

circles_overlap({sc_circle, AX, AY, AR}, {sc_circle, BX, BY, BR}) ->

    sc:euclidean_distance({AX,AY}, {BX,BY}) < (AR+BR).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

circles_contact({sc_circle, AX, AY, AR}, {sc_circle, BX, BY, BR}) ->

    sc:euclidean_distance({AX,AY}, {BX,BY}) =< (AR+BR).





%% @private

% don't worry about this, it's part of an old IRC joke

%% @since Version 632
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

wglsh(List)

    when is_list(List) ->

    [ case Ch of $a->$w;$A->$W; $e->$g;$E->$G; $i->$w;$I->$W; $o->$w;$O->$W; $u->$w;$U->$W; Z -> Z end || Ch <- List ].





%% @since Version 632
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

triangle_index(X) ->

    triangle_index(X,X).





%% @since Version 632
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

triangle_index(X1, Y)

    when is_integer(X1) ->

    triangle_index({1,X1}, Y);





%% @since Version 632

triangle_index(X, Y1)

    when is_integer(Y1) ->

    triangle_index(X, {1,Y1});





%% @since Version 632

triangle_index({X0,X1}, Y)

    when is_integer(X0),
         is_integer(X1) ->

    triangle_index(lists:seq(X0,X1), Y);





%% @since Version 632

triangle_index(X, {Y0,Y1})

    when is_integer(Y0),
         is_integer(Y1) ->

    triangle_index(X, lists:seq(Y0,Y1));





%% @since Version 632

triangle_index(LX, LY)

    when is_list(LX),
         is_list(LY) ->

    [ {X,Y} || X <- LX, Y <- LY, X<Y ].





%% @since Version 633
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

paper_3d_basic_depth(X, Z, SliderPos, DepthConstant) 

    when Z > 0 ->

    {(X - (SliderPos * DepthConstant) )/Z, (X + (SliderPos * DepthConstant) )/Z}.





%% @since Version 634
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

paper_3d_render(Bitmap3dList) ->

    paper_3d_render(Bitmap3dList, 1.0, 1.0, fun paper_3d_basic_depth/4).





%% @since Version 634
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

paper_3d_render(Bitmap3dList, DepthConstant) ->

    paper_3d_render(Bitmap3dList, 1.0, DepthConstant, fun paper_3d_basic_depth/4).





%% @since Version 634
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

paper_3d_render(Bitmap3dList, SliderPos, DepthConstant) ->

    paper_3d_render(Bitmap3dList, SliderPos, DepthConstant, fun paper_3d_basic_depth/4).





%% @since Version 634
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

paper_3d_render(Bitmap3dList, SliderPos, DepthConstant, DepthFun) ->

     lists:flatten([
         { DepthFun(X, Z, SliderPos, DepthConstant), Y }
     ||
         { X, Y, Z } <- lists:keysort(3, Bitmap3dList)
     ]).





%% @since Version 636
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

terminate_loop() ->

    receive

        terminate ->
            ok;

        { PID, terminate } when is_pid(PID) ->
            PID ! { terminate_loop_terminating, self() },
            ok;

        _ ->
            terminate_loop()

    end.





%% @since Version 637
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

show(X) ->

    io:format("~w~n",[X]).





%% @since Version 638
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

to_list(X) ->

    lists:flatten(io_lib:format("~w",[X])).





%% @since Version 640
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

bin_to_hex_list(Bin)

    when is_binary(Bin) ->

    lists:flatten([ sc:byte_to_hex(Byte) || Byte <- binary_to_list(Bin) ]).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Stretches a hash with a list of salts.  Some people incorrect refer to this as key strentghening.  The process is a simple key-derivation function: repeat the application of a hash with a different pre-pend salt each time.```1> Res = sc:stretch_hash("abc", fun erlang:md5/1, ["def", "ghi", "jkl", "mno"]).
%% <<129,166,92,224,108,140,78,205,151,136,77,203,166,229,62,186>>
%%
%% 2> sc:bin_to_hex_list(Res).
%% "81a65ce06c8c4ecd97884dcba6e53eba"
%%
%% C:\projects\scutil\res>php
%% <?php echo md5("mno" . md5("jkl" . md5("ghi" . md5("defabc", true), true), true)); ?>
%% ^Z
%% 81a65ce06c8c4ecd97884dcba6e53eba'''
%%
%% Thanks Josh, Davr, Vat.
%%
%% @since Version 641

-spec stretch_hash(State::list_or_binary(), HashFun::function(), ListOfSalts::list()) -> binary().

stretch_hash(State, HashFun, [LastSalt])

    when is_function(HashFun) ->

    HashFun(LastSalt ++ State);





stretch_hash(State, HashFun, [ThisSalt|RemainingSalts])

    when is_function(HashFun) ->

    stretch_hash(HashFun(ThisSalt ++ State), HashFun, RemainingSalts).






%% @since Version 644

null_postpad_bin_to(Bin, ToLength)

    when is_binary(Bin),
         is_integer(ToLength),
         size(Bin) >= ToLength ->

    Bin;





null_postpad_bin_to(Bin, ToLength)

    when is_binary(Bin),
         is_integer(ToLength),
         ToLength >= 0 ->

    Pad = list_to_binary( lists:duplicate(ToLength - size(Bin), 0) ),

    << Bin/binary, Pad/binary >>.





%% @doc <span style="color:orange;font-style:italic">Semi-Untested</span> An implementation of <a href="http://tools.ietf.org/html/rfc2104/">RFC 2104</a>, HMAC generic hash extension for any hash function and any key size.
%%
%% The reason this exists is to bring HMAC access to any hashing algorithm, as was the RFC's purpose.  There are HMAC functions in Erlang's `crypto:' module, but they are bound to specific hashers which are beginning to show their age, and they fix block size.
%%
%% The block size should be at most the block size of the hashing algorithm, but may be reduced (to the detriment of the safety of the result.)  Ideally, the block size should be the same as the hashing algorithm's block size, but many systems use variously truncated block sizes, so we support them all.  Jerks.
%%
%% This implementation was
%%
%% The key should be at least as long as the hash residue.  For example, if you're using MD5, which has 16-byte residues, the key should be at least 16 bytes.  As the specification requires, if the key is larger than the algorithm selected block size, the key will be hashed then null post-padded to the algorithm selected block size.```1> sc:bin_to_hex_list(sc:hmac(fun erlang:md5/1, "hello", "world", 64)).
%% "0e2564b7e100f034341ea477c23f283b"
%%
%% 2> sc:bin_to_hex_list(crypto:md5_mac("hello","world")).
%% "0e2564b7e100f034341ea477c23f283b"
%%
%% C:\Users\John>php
%% <?php /* php api is reversed of erlang's */
%%   echo hash_hmac('md5', 'world','hello');
%% ?> ^Z
%% 0e2564b7e100f034341ea477c23f283b
%%
%% % Also, one of the RFC test sets
%%
%% 3> sc:bin_to_hex_list(sc:hmac(fun erlang:md5/1, "Jefe", "what do ya want for nothing?", 64)).
%% "750c783e6ab0b503eaa86e310a5db738"
%%
%% 4> sc:bin_to_hex_list(crypto:md5_mac("Jefe", "what do ya want for nothing?")).
%% "750c783e6ab0b503eaa86e310a5db738"
%%
%% C:\Users\John>php
%% <?php echo hash_hmac('md5', 'what do ya want for nothing?', 'Jefe'); ?> ^Z
%% 750c783e6ab0b503eaa86e310a5db738'''
%%
%% @since Version 645

hmac(HashFun, Key, Data, BlockSize)

    when is_list(Data) ->

    hmac(HashFun, Key, list_to_binary(Data), BlockSize);





hmac(HashFun, Key, Data, BlockSize)

    when is_list(Key) ->

    hmac(HashFun, list_to_binary(Key), Data, BlockSize);





hmac(HashFun, Key, Data, BlockSize) ->

    K = null_postpad_bin_to(

        case size(Key) > BlockSize of

            true ->
                HashFun(Key);

            false ->
                Key

        end,

    BlockSize),

    IKey = crypto:exor(K, list_to_binary(lists:duplicate(BlockSize, 16#36))),
    MKey = HashFun(<<IKey/binary, Data/binary>>),

    OKey = crypto:exor(K, list_to_binary(lists:duplicate(BlockSize, 16#5C))),

    HashFun(<<OKey/binary, MKey/binary>>).





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Shorthands for algorithms so you don't need to know block sizes.
%%
%% @since Version 646

hmac("md4", Key, Data) ->

    hmac(fun erlang:md4/1, Key, Data, 64);





hmac("md5", Key, Data) ->

    hmac_md5(Key, Data);





hmac("sha", Key, Data) ->

    hmac_sha1(Key, Data);





hmac("sha1", Key, Data) ->

    hmac_sha1(Key, Data);





hmac("sha-1", Key, Data) ->

    hmac_sha1(Key, Data).





%% @doc <span style="color:red;font-style:italic">Obsolete - legacy support only - do not use in new code</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> HMAC wrapper built around MD4 as the core hash, frequently known as `hmac-md4' or `md4-hmac'.
%%
%% @since Version 647

hmac_md4(Key, Data) ->

    hmac(fun crypto:md4/1, Key, Data, 64).





%% @doc <span style="color:red;font-style:italic">Obsolete - legacy support only - do not use in new code</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> HMAC wrapper built around MD5 as the core hash, frequently known as `hmac-md5' or `md5-hmac'.
%%
%% @since Version 647

hmac_md5(Key, Data) ->

    hmac(fun erlang:md5/1, Key, Data, 64).





%% @doc Should be obsolete, but Erlang's standard library does not include SHA-2, and neither does `scutil' (yet) - <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> HMAC wrapper built around SHA1-160 (the only SHA-1) as the core hash, frequently known as `hmac-sha1', `hmac-sha' or `sha1-hmac'.
%%
%% @since Version 647

hmac_sha1(Key, Data) ->

    hmac(fun crypto:sha/1, Key, Data, 120).





%% @since Version 651
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculates the likelihood that all items in a list of probabilities expressed on the real interval will occur.  ```1> sc:probability_all([ 0.5, 0.4, 0.3 ]).
%% 0.06
%% 2> sc:probability_all([ 0.5, 0.5, 0.5 ]).
%% 0.125
%% 3> sc:probability_all([ 0.9, 0.9, 0.9, 0.9, 0.9 ]).
%% 0.5904900000000002'''
%%
%%  Notice the accumulated float rounding error.
%%
%%  And then, a probability result which surprises most people: ```4> sc:probability_all([ 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]).
%% 0.2621440000000001'''
%%
%% That's right, six 0.8s is 1 in 4.  Do the math.
%%
%% Thanks for the idea, Forest.


probability_all(ListOfProbabilities) 

    when is_list(ListOfProbabilities) ->

    case out_of_range(ListOfProbabilities, 0, 1) of

        [] ->
            list_product(ListOfProbabilities);

        _AnythingElse ->
            { error, "All members of the list of probabilities must be numbers on the interval [0,1] inclusive." }

    end.





%% @since Version 652
%%
%% @doc <span style="color:red;font-style:italic">Incomplete Todo Comeback</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

probability_any(ListOfProbabilities) 

    when is_list(ListOfProbabilities) ->

    case out_of_range(ListOfProbabilities, 0, 1) of

        [] ->
            todo;

        _AnythingElse ->
            { error, "All members of the list of probabilities must be numbers on the interval [0,1] inclusive." }

    end.





%% @since Version 649

in_range(List, Lo, Hi) ->

    [ I || I <- List, I =< Hi, I >= Lo ].





%% @since Version 650

out_of_range(List, Lo, Hi) ->

    [ I || I <- List, I > Hi orelse I < Lo ].





%% @since Version 650

is_repeated_list([]) ->

    true;





is_repeated_list([_R]) ->

    true;





is_repeated_list([First|Rem] = List)

    when is_list(List) ->

    case [ R || R <- Rem, R =/= First ] of

        [] ->
            true;

        _AnythingElse ->

            false

    end.





%% @since Version 651

ascii_alphanum_list_subset(List)

    when is_list(List) ->

    [
        I
    ||
        I <- List,

        (I >= $a andalso I =< $z) orelse
        (I >= $A andalso I =< $Z) orelse
        (I >= $0 andalso I =< $9)
    ].





%% @since Version 652

ngrams(List) ->

    ngrams(List, list).





%% @since Version 652

ngrams(List, list)

    when is_list(List) ->

    [ I || I <- re:split(string:to_lower(List), "[^\\pL\\pN]", [{return,list}]), I =/= [] ];





ngrams(List, binary)

    when is_list(List) ->

    [ I || I <- re:split(string:to_lower(List), "[^\\pL\\pN]"),                  I =/= <<>> ].





% todo comeback

%find_difference_sequence([_X]) ->
%
%    { error, "Not an Nth Difference sequence." };





% todo comeback

%find_difference_sequence(Seq)
%
%    when is_list(Seq) ->
%
%    Diffs = differences(Seq),
%
%    case is_repeated_list(Seq) of
%        true ->
%        false ->
%
%    end.





%% @since Version 653

country_codes() ->

    [ {"af", "Afghanistan"},
      {"ax", "Aland Islands"},
      {"al", "Albania"},
      {"dz", "Algeria"},
      {"as", "American Samoa"},
      {"ad", "Andorra"},
      {"ao", "Angola"},
      {"ai", "Anguilla"},
      {"aq", "Antarctica"},
      {"ag", "Antigua and Barbuda"},
      {"ar", "Argentina"},
      {"am", "Armenia"},
      {"aw", "Aruba"},
      {"au", "Australia"},
      {"at", "Austria"},
      {"az", "Azerbaijan"},
      {"bs", "Bahamas"},
      {"bh", "Bahrain"},
      {"bd", "Bangladesh"},
      {"bb", "Barbados"},
      {"by", "Belarus"},
      {"be", "Belgium"},
      {"bz", "Belize"},
      {"bj", "Benin"},
      {"bm", "Bermuda"},
      {"bt", "Bhutan"},
      {"bo", "Plurinational State of Bolivia"},
      {"bq", "Bonaire, Saint Eustatius and Saba"},
      {"ba", "Bosnia and Herzegovina"},
      {"bw", "Botswana"},
      {"bv", "Bouvet Island"},
      {"br", "Brazil"},
      {"io", "British Indian Ocean Territory"},
      {"bn", "Brunei Darussalam"},
      {"bg", "Bulgaria"},
      {"bf", "Burkina Faso"},
      {"bi", "Burundi"},
      {"kh", "Cambodia"},
      {"cm", "Cameroon"},
      {"ca", "Canada"},
      {"cv", "Cape Verde"},
      {"ky", "Cayman Islands"},
      {"cf", "Central African Republic"},
      {"td", "Chad"},
      {"cl", "Chile"},
      {"cn", "China"},
      {"cx", "Christmas Island"},
      {"cc", "Cocos (Keeling) Islands"},
      {"co", "Colombia"},
      {"km", "Comoros"},
      {"cg", "Congo"},
      {"cd", "The Democratic Republic of the Congo"},
      {"ck", "Cook Islands"},
      {"cr", "Costa Rica"},
      {"ci", "Cote d'Ivoire"},
      {"hr", "Croatia"},
      {"cu", "Cuba"},
      {"cw", "Curacao"},
      {"cy", "Cyprus"},
      {"cz", "Czech Republic"},
      {"dk", "Denmark"},
      {"dj", "Djibouti"},
      {"dm", "Dominica"},
      {"do", "Dominican Republic"},
      {"ec", "Ecuador"},
      {"eg", "Egypt"},
      {"sv", "El Salvador"},
      {"gq", "Equatorial Guinea"},
      {"er", "Eritrea"},
      {"ee", "Estonia"},
      {"et", "Ethiopia"},
      {"fk", "Falkland Islands (Malvinas)"},
      {"fo", "Faroe Islands"},
      {"fj", "Fiji"},
      {"fi", "Finland"},
      {"fr", "France"},
      {"gf", "French Guiana"},
      {"pf", "French Polynesia"},
      {"tf", "French Southern Territories"},
      {"ga", "Gabon"},
      {"gm", "Gambia"},
      {"ge", "Georgia"},
      {"de", "Germany"},
      {"gh", "Ghana"},
      {"gi", "Gibraltar"},
      {"gr", "Greece"},
      {"gl", "Greenland"},
      {"gd", "Grenada"},
      {"gp", "Guadeloupe"},
      {"gu", "Guam"},
      {"gt", "Guatemala"},
      {"gg", "Guernsey"},
      {"gn", "Guinea"},
      {"gw", "Guinea-Bissau"},
      {"gy", "Guyana"},
      {"ht", "Haiti"},
      {"hm", "Heard Island and McDonald Islands"},
      {"va", "Holy See"},
      {"hn", "Honduras"},
      {"hk", "Hong Kong"},
      {"hu", "Hungary"},
      {"is", "Iceland"},
      {"in", "India"},
      {"id", "Indonesia"},
      {"ir", "Islamic Republic of Iran"},
      {"iq", "Iraq"},
      {"ie", "Ireland"},
      {"im", "Isle of Man"},
      {"il", "Israel"},
      {"it", "Italy"},
      {"jm", "Jamaica"},
      {"jp", "Japan"},
      {"je", "Jersey"},
      {"jo", "Jordan"},
      {"kz", "Kazakhstan"},
      {"ke", "Kenya"},
      {"ki", "Kiribati"},
      {"kp", "Democratic People's Republic of Korea"},
      {"kr", "Republic of Korea"},
      {"kw", "Kuwait"},
      {"kg", "Kyrgyzstan"},
      {"la", "Lao People's Democratic Rrepublic"},
      {"lv", "Latvia"},
      {"lb", "Lebanon"},
      {"ls", "Lesotho"},
      {"lr", "Liberia"},
      {"ly", "Libyan Arab Jamahiriya"},
      {"li", "Liechtenstein"},
      {"lt", "Lithuania"},
      {"lu", "Luxembourg"},
      {"mo", "Macao"},
      {"mk", "The Former Yugoslav Republic of Macedonia"},
      {"mg", "Madagascar"},
      {"mw", "Malawi"},
      {"my", "Malaysia"},
      {"mv", "Maldives"},
      {"ml", "Mali"},
      {"mt", "Malta"},
      {"mh", "Marshall Islands"},
      {"mq", "Martinique"},
      {"mr", "Mauritania"},
      {"mu", "Mauritius"},
      {"yt", "Mayotte"},
      {"mx", "Mexico"},
      {"fm", "Federated States of Micronesia"},
      {"md", "Republic of Moldova"},
      {"mc", "Monaco"},
      {"mn", "Mongolia"},
      {"me", "Montenegro"},
      {"ms", "Montserrat"},
      {"ma", "Morocco"},
      {"mz", "Mozambique"},
      {"mm", "Myanmar"},
      {"na", "Namibia"},
      {"nr", "Nauru"},
      {"np", "Nepal"},
      {"nl", "Netherlands"},
      {"nc", "New Caledonia"},
      {"nz", "New Zealand"},
      {"ni", "Nicaragua"},
      {"ne", "Niger"},
      {"ng", "Nigeria"},
      {"nu", "Niue"},
      {"nf", "Norfolk Island"},
      {"mp", "Northern Mariana Islands"},
      {"no", "Norway"},
      {"om", "Oman"},
      {"pk", "Pakistan"},
      {"pw", "Palau"},
      {"ps", "Palestinian Territory, Occupied"},
      {"pa", "Panama"},
      {"pg", "Papua New Guinea"},
      {"py", "Paraguay"},
      {"pe", "Peru"},
      {"ph", "Philippines"},
      {"pn", "Pitcairn"},
      {"pl", "Poland"},
      {"pt", "Portugal"},
      {"pr", "Puerto Rico"},
      {"qa", "Qatar"},
      {"re", "Reunion"},
      {"ro", "Romania"},
      {"ru", "Russian Federation"},
      {"rw", "Rwanda"},
      {"bl", "Saint Barthelemy"},
      {"sh", "Saint Helena, Ascension and Tristan da Cunha"},
      {"kn", "Saint Kitts and Nevis"},
      {"lc", "Saint Lucia"},
      {"mf", "Saint Martin (French Part)"},
      {"pm", "Saint Pierre and Miquelon"},
      {"vc", "Saint Vincent and the Grenadines"},
      {"ws", "Samoa"},
      {"sm", "San Marino"},
      {"st", "Sao Tome and Principe"},
      {"sa", "Saudi Arabia"},
      {"sn", "Senegal"},
      {"rs", "Serbia"},
      {"sc", "Seychelles"},
      {"sl", "Sierra Leone"},
      {"sg", "Singapore"},
      {"sx", "Sint Maarten (Dutch Part)"},
      {"sk", "Slovakia"},
      {"si", "Slovenia"},
      {"sb", "Solomon Islands"},
      {"so", "Somalia"},
      {"za", "South Africa"},
      {"gs", "South Georgia and the South Sandwich Islands"},
      {"es", "Spain"},
      {"lk", "Sri Lanka"},
      {"sd", "Sudan"},
      {"sr", "Suriname"},
      {"sj", "Svalbard and Jan Mayen"},
      {"sz", "Swaziland"},
      {"se", "Sweden"},
      {"ch", "Switzerland"},
      {"sy", "Syrian Arab Republic"},
      {"tw", "Taiwan"},
      {"tj", "Tajikistan"},
      {"tz", "United Republic of Tanzania"},
      {"th", "Thailand"},
      {"tl", "Timor-Leste"},
      {"tg", "Togo"},
      {"tk", "Tokelau"},
      {"to", "Tonga"},
      {"tt", "Trinidad and Tobago"},
      {"tn", "Tunisia"},
      {"tr", "Turkey"},
      {"tm", "Turkmenistan"},
      {"tc", "Turks and Caicos Islands"},
      {"tv", "Tuvalu"},
      {"ug", "Uganda"},
      {"ua", "Ukraine"},
      {"ae", "United Arab Emirates"},
      {"gb", "United Kingdom"},
      {"us", "United States"},
      {"um", "United States Minor Outlying Islands"},
      {"uy", "Uruguay"},
      {"uz", "Uzbekistan"},
      {"vu", "Vanuatu"},
      {"ve", "Bolivarian Republic of Venezuela"},
      {"vn", "Viet Nam"},
      {"vg", "Virgin Islands, British"},
      {"vi", "Virgin Islands, US"},
      {"wf", "Wallis and Futuna"},
      {"eh", "Western Sahara"},
      {"ye", "Yemen"},
      {"zm", "Zambia"},
      {"zw", "Zimbabwe"}
    ].





%% @since Version 660

random_unicode_char() ->

    rand(16#10ffff).





%% @since Version 661

file_to_binary_literal_as_string(PathAndFilename) ->

    { ok, F } = file:read_file(PathAndFilename),

    lists:flatten(io_lib:format("~p", [F])).





%% @since Version 665

months_as_short_atoms() ->

    [jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec].





%% @since Version 666
%%
%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span>

% % todo comeback figure out a way to do this
% % @ flow {module_is_loaded, {lists,member}}

module_is_loaded(ModuleName) 

    when is_atom(ModuleName) ->

    lists:member(ModuleName, erlang:loaded()).





% % @ since Version 667

% % @ doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Inspired by http://chrisdone.com/posts/2011-08-07-flo-flow-diagrams-from-your-codebase.html

% comeback

% oh no!  module attributes can't be among the functions.  that means the beautiful decoration scheme i had in mind
% won't work.  i have to think of a different one before continuing.  le sad.

% module_flow(ModuleNameList) when is_list(ModuleNameList) ->
%
%     case [ I || { I, Ir } <- [ { MN, module_is_loaded(MN) } || MN <- ModuleNameList ], Ir =/= true] of
%
%         [] ->
%             "digraph G { " ++ lists:flatten([ atom_to_list(MN) ++ "; " || MN <- ModuleNameList ]) ++ "}";
%
%         Any ->
%             { "Some modules not loaded", Any }
%
%     end.



% todo comeback

%% @ since Version 676

replace(Source, Pattern, Replacement) ->

    implode(Replacement, explode(Pattern, Source)).





%% @private

%% @ since Version 677

counter_process() ->

    receive


        shutdown ->
            ok;


        {Caller, get_counter, Name} ->

            case get(Name) of

                undefined ->
                    Caller ! {counter_at, Name, 0},
                    put(Name,0),
                    counter_process();

                Defined ->
                    Caller ! {counter_at, Name, Defined},
                    counter_process()

            end;


        {Caller, adjust_by_counter, Name, By} ->

            case get(Name) of

                undefined ->
                    Caller ! {counter_at, Name, By},
                    put(Name,By),
                    counter_process();

                Defined ->
                    New = Defined+By,
                    Caller ! {counter_at, Name, New},
                    put(Name,New),
                    counter_process()

            end;


        {Caller, set_counter, Name, To} ->

            Caller ! {counter_at, Name, To},

            case To of

                0 ->
                    erase(Name);

                T ->
                    put(Name,T)

            end,

            counter_process()


    end.





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Sets a counter's value to a specific value. ```1> sc:counter_at(hello).
%% 0
%%
%% 2> sc:set_counter_value(hello,4).
%% 4
%%
%% 3> sc:counter_at(hello).
%% 4
%%
%% 4> sc:reset_counter(hello).
%% 0
%%
%% 5> sc:counter_at(hello).
%% 0'''

%% @since Version 678

-spec set_counter_value(Name::any(), To::number()) -> To::number.

set_counter_value(Name, To)

    when is_number(To) ->

    start_register_if_not_running(sc_counter_counter_process, fun counter_process/0),
    sc_counter_counter_process ! {self(), set_counter, Name, To},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv set_counter_value(Name, 0)

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Resets a counter's value to zero. ```1> sc:counter_at(hello).
%% 0
%%
%% 2> sc:set_counter_value(hello,4).
%% 4
%%
%% 3> sc:counter_at(hello).
%% 4
%%
%% 4> sc:reset_counter(hello).
%% 0
%%
%% 5> sc:counter_at(hello).
%% 0'''


%% @since Version 679

reset_counter(Name) ->

    set_counter_value(Name, 0).





%% @doc Adds to a counter's value; if the counter was not already defined, it will become the value in the `By' argument. ```1> sc:counter_at(hello).
%% 0
%%
%% 2> sc:inc_counter(hello).
%% 1
%%
%% 3> sc:adjust_by(hello, 3).
%% 4'''

%% @since Version 680

-spec adjust_counter_by(Name::any(), By::number()) -> number().

adjust_counter_by(Name, By)

    when is_number(By) ->

    start_register_if_not_running(sc_counter_counter_process, fun counter_process/0),
    sc_counter_counter_process ! {self(), adjust_by_counter, Name, By},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv adjust_counter_by(Name,1)

%% @since Version 681

inc_counter(Name) ->

    adjust_counter_by(Name, 1).





%% @equiv adjust_counter_by(Name,By)

%% @since Version 681

inc_counter(Name,By) ->

    adjust_counter_by(Name, By).





%% @equiv adjust_counter_by(Name,-1)

%% @since Version 681

dec_counter(Name) ->

    adjust_counter_by(Name, -1).





%% @equiv adjust_counter_by(Name,-1*By)

%% @since Version 681

dec_counter(Name,By) ->

    adjust_counter_by(Name, -1*By).





%% @doc Checks a counter's value; if the counter was not already defined, it will report zero. ```1> sc:counter_at(hello).
%% 0
%%
%% 2> sc:inc_counter(hello).
%% 1
%%
%% 3> sc:inc_counter(hello).
%% 2
%%
%% 4> sc:inc_counter(hello).
%% 3
%%
%% 5> sc:counter_at(hello).
%% 3
%%
%% 6> sc:reset_counter(hello).
%% 0
%%
%% 7> sc:counter_at(hello).
%% 0'''

%% @since Version 682

-spec counter_at(Name::any()) -> number().

counter_at(Name) ->

    start_register_if_not_running(sc_counter_counter_process, fun counter_process/0),
    sc_counter_counter_process ! {self(), get_counter, Name},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @since Version 683

neighbors(cartesian_no_corners, {X,Y}) ->

    [ {X,   Y-1},  % N
      {X+1, Y},    % E
      {X,   Y+1},  % S
      {X-1, Y}     % W
    ];





%% @since Version 684

neighbors(cartesian_with_corners, {X,Y}) ->

    [ {X,   Y-1},  % N
      {X+1, Y-1},  % NE
      {X+1, Y},    % E
      {X+1, Y+1},  % SE
      {X,   Y+1},  % S
      {X-1, Y+1},  % SW
      {X-1, Y},    % W
      {X-1, Y-1}   % NW
    ].





%% @since Version 685

is_between(X, A, B) -> is_between(X, A, B, exclusive).





%% @since Version 685

is_between(X, A, B, exclusive) when X > A,  X < B  -> true;
is_between(_, _, _, exclusive)                     -> false;

is_between(X, A, B, inclusive) when X >= A, X =< B -> true;
is_between(_, _, _, inclusive)                     -> false.





% % @ since Version 686

%bounded_neighbors(cartesian_no_corners, {X,Y}, {XC,YC,W,H}) ->
%
%    [ {X,   Y-1},  % N
%      {X+1, Y},    % E
%      {X,   Y+1},  % S
%      {X-1, Y}     % W
%    ];






%% @since Version 691

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Reverse a marketing funnel, to go from goal needed to input needed.  ```1> % Using the data from http://www.forentrepreneurs.com/lessons-from-leaders/jboss-example/
%% 1> sc:unfunnel(300, [{1/4,"Web activity scoring"}, {1/3,"Telemarketing"}, {1/4,"Inside Sales"}]).
%% [ { 14400, "Input Needed" },
%%   { 3600,  "Web activity scoring", 0.25 },
%%   { 1200,  "Telemarketing",        0.3333333333333333 },
%%   { 300,   "Inside Sales",         0.25 },
%%   { 300,   "Result" } ]'''

unfunnel(Tgt, ProbPropList) ->

    unfunnel(Tgt, ProbPropList, ceil).





unfunnel(Tgt, ProbPropList, MaybeCeil)

    when is_number(Tgt),
         is_list(ProbPropList) ->

    unfunnel(Tgt, [ { Tgt, "Result" } ], lists:reverse(ProbPropList), MaybeCeil).





unfunnel(Counter, Output, [], _WhoCaresIfCeil) ->

    [{Counter, "Input Needed"}]++Output;





unfunnel(Counter, Output, [{Scale,Label} | RemWork], no_ceil) ->

    unfunnel(Counter/Scale, [{Counter, Label, Scale}]++Output, RemWork, no_ceil);





unfunnel(Counter, Output, [{Scale,Label} | RemWork], ceil) ->

    unfunnel(sc:ceil(Counter/Scale), [{Counter, Label, Scale}]++Output, RemWork, ceil).





%% @since Version 703

%% @doc Generates a markhov chain from a list of lists.

markhov_chain(Depth, Sources)

    when
        Depth > 0,
        is_integer(Depth),
        is_list(Sources) ->

    Numbered = lists:zip(lists:seq(1,length(Sources)), Sources),

    sc:histograph(lists:append([ markhov_chain(N, Depth, Source) || {N, Source} <- Numbered ])).





markhov_starts(Depth, Source) ->

    {Start,_} = lists:split(Depth, Source),

    [{markhov_start,Start}].





markhov_ends(Depth, Source) ->

    {Start,_} = lists:split(Depth, lists:reverse(Source)),

    [{markhov_end,lists:reverse(Start)}].





markhov_chain(N, Depth, Source)

    when
        Depth > 0,
        is_integer(Depth),
        is_list(Source) ->

    case length(Source) > Depth of

        false ->
            { error, "Source for " ++ integer_to_list(N) ++ " must be at least one token longer than the precursor depth." };

        true ->
            markhov_chain(N, Depth, Source, [])   ++
            markhov_starts(Depth, Source) ++
            markhov_ends(Depth, Source)

    end.





markhov_chain(N, Depth, Source, Work) ->

    case length(Source) > Depth of

        false ->

            lists:reverse(Work);

        true ->

            {Precursor, Successor} = lists:split(Depth, Source),
            [SFirst | _]           = Successor,
            [_ | Postcursor]       = Source,

            markhov_chain(N, Depth, Postcursor, [ {Precursor, SFirst} ] ++ Work)

    end.





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Cuts a string according to any of the three newline conventions (even mixed), and discards empty strings.  Mostly convenience and documentary. ```1> sc:to_lines("one\rtwo\nthree\r\nfour\r\r\rfive").
%% ["one","two","three","four","five"]
%%
%% 2> sc:to_lines("a\nb").
%% ["a","b"]
%%
%% 3> sc:to_lines("a\n\n\n\n\n\nb").
%% ["a","b"]
%%
%% 4> sc:to_lines("a\r\nb").
%% ["a","b"]
%%
%% 5> sc:to_lines("a\rb").
%% ["a","b"]
%%
%% 6> sc:to_lines("a\rb\nc\r\nd\n\r\r\ne")
%% ["a","b","c","d","e"]
%%
%% 7> sc:to_lines("").
%% []
%%
%% 8> sc:to_lines("\r\n\r\r\n\n\r").
%% []'''
%%
%% Unit and doc tested.

%% @since Version 705

-spec to_lines(Text::string()) -> string_list().

to_lines(Text) ->

    string:tokens(Text, "\r\n"). % yay convenience functions





%% @since Version 706

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculate the Flesch-Kincaid readability score of a set of text metrics.  See <a href="http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test">Wikipedia</a> and <a href="http://www.readabilityformulas.com/graphics/fleschresults.gif">readabilityformulas.com</a>.

fk_readability(Words, Sentences, Syllables) ->

    206.835 - (1.015 * (Words/Sentences)) - (84.6 * (Syllables/Words)).





%% @since Version 707

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Provides mandated human-readable labels for Flesch-Kincaid readability calculations.  @see http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test and http://www.readabilityformulas.com/graphics/fleschresults.gif .

labelled_fk_readability(R) when R > 100 -> { "Easy before 11 years",     R };
labelled_fk_readability(R) when R >  90 -> { "Easy at 11 years",         R };
labelled_fk_readability(R) when R >  70 -> { "Easy for 11 to 13 years",  R };
labelled_fk_readability(R) when R >  60 -> { "Easy for 13 to 15 years",  R };
labelled_fk_readability(R) when R >  30 -> { "Appropriate for 15 years", R };
labelled_fk_readability(R) when R >   0 -> { "Appropriate for college",  R };
labelled_fk_readability(R)              -> { "Difficult",                R }.





%% @since Version 708

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculate the Flesch-Kincaid readability score of a block of text.  Also takes three lambdas to do text parsing.  @see http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test

% comeback todo
% fk_readability(Data) -> fk_readability(Data, fun count_words/1, fun count_sentences/1, fun count_syllables/1).

fk_readability(Data, WordCounter, SentenceCounter, SyllableCounter) ->

    Words     = WordCounter(Data),
    Sentences = SentenceCounter(Data),
    Syllables = SyllableCounter(Data),

    labelled_fk_readability(
      fk_readability(Words, Sentences, Syllables)
    ).





%% @todo finish me

% dissimilar_charset(english, lowercase) -> "abcdefghjklmnopqrstuwxyz";
% dissimilar_charset(english, mixedcase) -> "abcdefghjklmnopqrstuwxyzABDEFGHRT";
% dissimilar_charset(english, alphanum)  -> "abcdefghjklmnopqrstuwxyzABDEFGHRT34679".

% similarize_charset   a10OZ2B8 -> aloozzBB




%% @since Version 716

%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Remove the baseline of a dataset, normalizing a waveform or other signal to its bottom peak.

isolate_waveform(Waveform) ->

    Baseline = lists:min(Waveform),

    [ Sample - Baseline ||
        Sample <- Waveform
    ].





%% @since Version 720

unit_scale(Waveform) ->

    { Baseline, MaxObserved } = sc:extrema(Waveform),
    SignalMax                 = MaxObserved - Baseline,

    [ (Sample - Baseline) / SignalMax ||
        Sample <- Waveform
    ].





%% @doc <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Fetch the type of the argument.  Valid for any term.  Fails before erlang 12, due to use of `is_bitstring()' . ```1> sc:type_of(1).
%% integer
%%
%% 2> sc:type_of({hello,world}).
%% tuple'''

%% @since Version 722

-spec type_of(Argument::any()) -> type_label().

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





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculates the cross product of two vectors (<span style="color:red">Incomplete</span> represented as {@type three_vector()}s - no support yet for seven). ```1> sc:dot_product([1,1,1],[2,2,2]).
%% 6
%%
%% 2> sc:dot_product([1,1,1],[3,3,3]).
%% 9
%%
%% 3> sc:dot_product([-1,0,1],[3,3,3]).
%% 0
%%
%% 4> sc:dot_product([-1,1,1],[3,3,3]).
%% 3
%%
%% 5> sc:dot_product([0.5,1,2],[1,1,1]).
%% 3.5'''<span style="color:red">TODO: Implement seven-dimensional cross product</span>

%% @since Version 80

%% @todo implement 7-dimensional variation, http://en.wikipedia.org/wiki/Seven-dimensional_cross_product

-spec cross_product(VX::three_vector(T), VY::three_vector(T)) -> three_vector(T).

cross_product( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->

    { (Y1*Z2) - (Z1*Y2) , (Z1*X2) - (X1*Z2), (X1*Y2) - (Y1*X2) };





cross_product( [X1,Y1,Z1], [X2,Y2,Z2] ) ->

    [ (Y1*Z2) - (Z1*Y2) , (Z1*X2) - (X1*Z2), (X1*Y2) - (Y1*X2) ].





% removed when length(VX) == length(VY) because it's implied by lists:zip

%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Calculates the dot product of two vectors (<span style="color:red">Incomplete</span> represented as numeric lists; tuples not yet supported). ```1> sc:dot_product([1,1,1],[2,2,2]).
%% 6
%%
%% 2> sc:dot_product([1,1,1],[3,3,3]).
%% 9
%%
%% 3> sc:dot_product([-1,0,1],[3,3,3]).
%% 0
%%
%% 4> sc:dot_product([-1,1,1],[3,3,3]).
%% 3
%%
%% 5> sc:dot_product([0.5,1,2],[1,1,1]).
%% 3.5'''<span style="color:red">TODO: The tuple variation of vectors has not yet been implemented in this function.</span>

%% @since Version 80

%% @todo implement tuple variation

-spec dot_product(VX::numeric_list(), VY::numeric_list()) -> number().

dot_product(VX, VY) ->

    lists:sum( [ X*Y || {X,Y} <- lists:zip(VX,VY) ] ).





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the magnitude of a vector.  A vector's magnitude is the length of its hypoteneuse.  A vector can be seen as the product of its unit vector and its magnitude; as such many people see a vector's magnitude as its scale.  The normal of the zero vector is undefined, in the way that dividing by zero is undefined, and will throw an arithmetic exception. ```1> sc:vector_normalize([0,3,4]).
%% [0.0,0.6,0.8]'''<span style="color:red">TODO: When tuple comprehensions are introduced to the language, convert this to using them.</span>

%% @since Version 725

-spec vector_normalize(Vector::vector()) -> unit_vector().

vector_normalize(VX) when is_list(VX) ->

    VM = vector_magnitude(VX),
    [ X / VM || X <- VX ];



vector_normalize(VX) when is_tuple(VX) ->

    list_to_tuple(vector_normalize(tuple_to_list(VX))).





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Takes the quadratic scalar product average of a vector `W' and a list of vectors `X'.  The QSP Average
%% is the arithmetic mean of the result set Y, where Y is generated as the square of the magnitude of the dot product
%% of W and each individual vector in X. @see http://www.inf.fu-berlin.de/inst/ag-ki/rojas_home/documents/1996/NeuralNetworks/K5.pdf
%% pdf-page 15.  ```1> sc:qsp_average([1,2,3], [[0,0,0],[0,0,0]]).
%% 0.0
%%
%% 2> sc:qsp_average([1,2,3], [[0,0,1],[0,0,0]]).
%% 4.5
%%
%% 3> sc:qsp_average([1,2,3], [[0,1,0],[0,0,0]]).
%% 2.0
%%
%% 4> sc:qsp_average([1,2,3], [[1,0,0],[0,0,0]]).
%% 0.5
%%
%% 5> sc:qsp_average([1,2,3], [[1,1,1],[0,0,0]]).
%% 18.0
%%
%% 6> sc:qsp_average([1,2,3], [[0,0,0],[1,1,1]]).
%% 18.0
%%
%% 7> sc:qsp_average([1,2,3], [[1,1,1],[1,1,1]]).
%% 36.0'''The linked documentation incorrectly uses the notation ||Foo|| instead of |Foo| to
%% present the algorithm.  ||Foo|| is the vector magnitude - the root sum square of vector elements - but as the input is the
%% dot product of two 1d vectors, which will always be a single number, the vector magnitude serves no purpose other than to
%% normalize the sign slowly and counterintuitively; thus we switch to abs despite the documentation.  {@section Thanks} to Steve
%% Stair for helping straighten this out.  Thanks to the following for help with qsp_average and dependencies: Asterick, Chile,
%% John Sensebe, PfhorSlayer, Raleigh.

%% @since Version 726

-spec qsp_average(W::numeric_list(), InputVecs::vector_list()) -> float().

qsp_average(W, InputVecs) ->

    GetSqVnDp = fun(Xi) ->
        VnDp = abs(dot_product(W, Xi)),
        VnDp * VnDp
    end,

    sc:arithmetic_mean([ GetSqVnDp(Xi) || Xi <- InputVecs ]).





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Checks whether a given bit is on in a sufficiently sized unsigned two's compliment integer representation of `Num'.  ```1> sc:has_bit(5,0).
%% true
%%
%% 2> scutil:has_bit(5,1).
%% false'''

%% @since Version 727

-spec has_bit(Number::non_neg_integer(), Bit::non_neg_integer()) -> true | false.

has_bit(Num, Bit)

    when is_integer(Num),
         is_integer(Bit),
         Num > 0,
         Bit >= 0,
         Bit < 64 ->

    (Num band (1 bsl Bit)) > 0.





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Counts the number of bits turned on in a sufficiently sized unsigned two's compliment integer representation of `Num'.  ```1> sc:count_bits(5).
%% 2'''

%% @since Version 727

-spec count_bits(Number::non_neg_integer()) -> non_neg_integer().

count_bits(Num)

    when is_integer(Num),
         Num > 0 ->

    length( [S || S <- lists:seq(0,63), has_bit(Num, S) == true] ).





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the list of colors which are used, in order, as the standard back colors of a series of decks for {@link multi_deck/2}.  Each color is presented as an atom.  ```1> sc:standard_card_backs().
%% [ red, blue, green, black, purple, orange, brown, yellow,
%%   teal, gray, cyan, indigo, pink, white, tan, maroon,
%%   navy, forest, leaf, sky, brick ]
%%
%% 2> length(sc:standard_card_backs()).
%% 24'''

%% @since Version 728

-spec standard_card_backs() -> list().

standard_card_backs() ->

    [red, blue, green, black, purple, orange, brown, yellow, teal, gray, cyan, indigo, pink, white, tan, maroon, navy, forest, leaf, sky, brick, emerald, steel, turquoise].





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Returns the front of the list of colors which are used, in order, as the standard back colors of a series of decks for {@link multi_deck/2}.  Each color is presented as an atom.  If you request more colors than are in the list, the list `[1,2...Count]' is provided instead.  ```1> sc:standard_card_backs(5).
%% [ red, blue, green, black, purple ]
%%
%% 2> sc:standard_card_backs(29).
%% [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29]'''

%% @since Version 728

-spec standard_card_backs(Count::pos_integer()) -> list().

standard_card_backs(Count) ->

    CList = standard_card_backs(),

    case Count > length(CList) of

        true  ->
            lists:seq(1,Count);

        false ->
            {Front, _Back} = lists:split(Count, CList),
            Front

    end.





%% @doc <span style="color:red">Incomplete</span> <span style="color:red;font-style:italic">Untested</span> <span style="color:orange;font-style:italic">Stoch untested</span> Makes a number of instances of a deck, and applies a different back to each.  The first parameter may be a {@type positive_integer()}, at which point the color sequence from {@link standard_backs/0} will be used; otherwise, a list may be used, which will be used as the card backs (there is no requirement regarding their type or uniqueness, only that they be presented as a list.)  The second parameter may be a {@type function()}, which will be called to generate a list of cards, or a {@type list} of cards which will be used directly.

%% @since Version 729

-spec multi_deck(Backs::positive_integer_or_list(), DeckGenerator::function_or_list()) -> list().

multi_deck(Backs, DeckGenerator) when is_function(DeckGenerator) ->

    multi_deck(Backs, DeckGenerator());





multi_deck(BackCount, Deck) when is_integer(BackCount) ->

    [Backs, _] = standard_card_backs(BackCount),
    multi_deck(Backs, Deck);





multi_deck(Backs, Deck) ->

    [ list_to_tuple([Back] ++ tuple_to_list(Card)) ||

        Back <- Backs,
        Card <- Deck

    ].





%% @since Version 730

columns(RowCount, List) ->

    columns(List, lists:duplicate(RowCount, []), 0).



columns( [], Output, Unrotate) ->

    sc:rotate_list(
        Unrotate,
        [ lists:reverse(Column) ||
            Column <- Output
        ]
    );



columns( [Item|ListRem], [Output|OutRem], Unrotate) ->

    columns(ListRem, OutRem ++ [[Item]++Output], Unrotate-1).





%% @since Version 731

columnated_rows(ColumnCount, List) ->

    columns(ceiling(length(List) / ColumnCount), List).





%% @since Version 732

first_or_nothing( [H|T] ) ->

    {H,T};



first_or_nothing( [] ) ->

    { [], [] }.





%% @todo needs spec, doc, since

%% @since Version 733

first_row(Columns) ->

     first_row(Columns, [], []).



%% @private

first_row( [], OutCols, Work) ->

    { lists:reverse(Work), lists:reverse(OutCols) };



first_row( [ThisCol|RemCols], OutCols, Work) ->

    {Item,ColRem} = first_or_nothing(ThisCol),
    first_row(RemCols, [ColRem]++OutCols, [Item]++Work).





%% @equiv columnate(List, 2, 3)
%% @since Version 734

columnate(List) ->

    columnate(List, []).



%% @since Version 734

columnate(List, Options) ->

    Settings = lists:ukeymerge(1, Options, [{align, center}, {columns, 2}, {margin, 3}] ),

    [ColumnCount, Margin, Align] = [ proplists:get_value(X, Settings) || X <- [columns,margin,align] ],

    Columns = columns(ColumnCount, List),

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

    Format = implode( lists:duplicate(Margin, $ ), [ "~" ++ integer_to_list(Width) ++ "s" || Width <- MinWidths ] ),                                    %"% (sorry, bad highlighter; grumble mumble SUBLIME)

    columnate_each_row(Aligned, Format, []).





%% @since Version 735

columnated_text(List, Options) ->

    implode("\r\n", columnate(List, Options)).





%% @since Version 736

columnate_each_row( [ [] | _ ], _Format, Output) ->

    lists:reverse(Output);



columnate_each_row(Columns, Format, Output) ->

    {ThisRow, RemRows} = first_row(Columns),
    ThisOut = lists:flatten(io_lib:format(Format, ThisRow)),
    columnate_each_row(RemRows, Format, [ThisOut]++Output).





%% @since Version 737

is_numeric_char(Ch) -> is_numeric_char(Ch, decimal).

is_numeric_char(Ch, decimal) when $0 =< Ch, Ch =< $9; Ch == $-; Ch == $. -> true;
is_numeric_char(_, _)                                                    -> false.





%% @doc Returns the difference, in seconds as a float, between two erlang timestamps as returned by `os:timestamp()'.  Negative differences are returned if the latter timestamp `B' is earlier than the former timestamp `A'.  This is different than `timer:now_diff/2' in that this works in floating point seconds, rather than integer microseconds. `os:timestamp/0' should be used rather than `erlang:now/0' because `erlang:now/0' is massaged time - gaps are smoothed, reversals are prevented, reads are forced monotonic increasing, et cetera (thanks MononcQc,) whereas `os:timestamp/0' is raw.  ```1> A = os:timestamp().
%% {1232,947675,340000}
%%
%% 2> B = os:timestamp().
%% {1232,947679,412000}
%%
%% 3> sc:time_diff(A,B).
%% 4.072
%%
%% 4> sc:time_diff(B,A).
%% -4.072'''

%% @since Version 742

-spec time_diff(A::timestamp(), B::timestamp()) -> float().

time_diff( {AM,AS,AU}, {BM,BS,BU}) ->

    ((BM-AM) * 1000000) + (BS-AS) + ((BU-AU)/1000000).





%% @since Version 743

%% @doc Benchmark a lambda call.  `os:timestamp/0' is used rather than `erlang:now/0' because `erlang:now/0' is massaged time - gaps are smoothed, reversals are prevented, reads are forced monotonic increasing, et cetera (thanks MononcQc,) whereas `os:timestamp/0' is raw. ```1> Delay1s = fun() -> receive after 1000 -> mississippi end end.
%% #Fun<erl_eval.20.21881191>
%%
%% 2> Delay1s().
%% mississippi
%%
%% 3> sc:benchmark(Delay1s).
%% {1.014,mississippi}
%%
%% 4> {GenTime, Dataset} = sc:benchmark(fun() -> sc:shuffle(lists:seq(1,100000)) end).
%% { 0.203,
%%   [ 51251,43276,49951,60293,85795,53354,81523,80855,35387,
%%     63856,17466,50722,88409,33148,98511,43668,12019,6187,63052,
%%     80796,80444,68758,60669,74765,31900,66920,82825 | ... ] }
%%
%% 5> {SortTime, _Sorted} = sc:benchmark(fun() -> lists:sort(Dataset) end).
%% { 0.078,
%%   [ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
%%     23,24,25,26,27 | ... ] }'''

benchmark(BareLambda) ->

    Start  = os:timestamp(),
    Result = BareLambda(),
    End    = os:timestamp(),

    { time_diff(Start,End), Result }.





%% @since Version 743

benchmark(Fun, Args) ->

    Start  = os:timestamp(),
    Result = apply(Fun, Args),
    End    = os:timestamp(),

    { time_diff(Start,End), Result }.





benchmark(Module, Func, Args) ->

    Start  = os:timestamp(),
    Result = apply(Module, Func, Args),
    End    = os:timestamp(),

    { time_diff(Start,End), Result }.





%% @since Version 744

now_str_utc24() ->

    { {Y,M,D}, {H,MM,S} }     = calendar:now_to_universal_time(now()),
    [Ys, Ms, Ds, Hs, MMs, Ss] = [ integer_to_list(X) || X <- [ Y, M, D, H, MM, S ] ],

    Ys++"/"++Ms++"/"++Ds++" "++Hs++":"++MMs++":"++Ss++" UTC24".





%% @since Version 748
%% @private

private_notebook_name_to_table_name(NotebookName) ->

    "sc_notebooks/" ++ NotebookName ++ ".dets".





%% @since Version 745
%% @private

private_open_notebook_table(TableName)

    when is_list(TableName) ->

    filelib:ensure_dir("sc_notebooks/"),
    { ok, ResultTableName } = dets:open_file(private_notebook_name_to_table_name(TableName), [{type, set}] ),
    { sc_notebook_handle, ResultTableName }.





%% @since Version 745
%% @private

private_close_notebook_table( { sc_notebook_handle, TableName } )

    when is_list(TableName) ->

    dets:close(TableName).





%% @since Version 750
%% @private

private_write_to_notebook_table( { sc_notebook_handle, TableName }, Key, Value )

    when is_list(TableName) ->

    dets:insert(TableName, {Key, Value}).





%% @since Version 751
%% @private

private_read_from_notebook_table( { sc_notebook_handle, TableName }, Key )

    when is_list(TableName) ->

    case dets:match(TableName, {Key, '$1'}) of

        []  ->
            undefined;

        [[X]] ->
            { value, X }

    end.





%% @since Version 753
%% @private

private_remove_from_notebook_table( { sc_notebook_handle, TableName }, Key )

    when is_list(TableName) ->

    dets:delete(TableName, Key).





%% @since Version 757
%% @private

private_get_contents_from_notebook_table( { sc_notebook_handle, TableName } )

    when is_list(TableName) ->

    [ I || [{I,_}] <- dets:match(TableName, '$1') ].





%% @since Version 747

notebook_create(NotebookName)

    when is_list(NotebookName) ->

    private_close_notebook_table(private_open_notebook_table(NotebookName)).





%% @since Version 748

notebook_destroy(NotebookName)

    when is_list(NotebookName) ->

    file:delete(private_notebook_name_to_table_name(NotebookName)).





%% @since Version 749

%% @todo This could be - will be - much stronger.

notebook_validate(NotebookName)

    when is_list(NotebookName) ->

    case dets:is_dets_file(private_notebook_name_to_table_name(NotebookName)) of

        { error, _ } -> false;
        _            -> { sc_notebook, [ todo, details ]}  % comeback todo

    end.





%% @since Version 750

notebook_write(NotebookName, Key, Value)

    when is_list(NotebookName) ->

    NT = private_open_notebook_table(NotebookName),
         private_write_to_notebook_table(NT, Key, Value),
         private_close_notebook_table(NT).





%% @since Version 751

notebook_read(NotebookName, Key)

    when is_list(NotebookName) ->

    NT  = private_open_notebook_table(NotebookName),
    Res = private_read_from_notebook_table(NT, Key),
    private_close_notebook_table(NT),

    Res.





%% @since Version 752

notebook_contains(NotebookName, Key)

    when is_list(NotebookName) ->

    case notebook_read(NotebookName, Key) of

        undefined    -> false;
        { value, _ } -> true

    end.





%% @doc Removes an item from a notebook.  Like most other notebook functions, if the referred notebook does not already exist, it will be created.  {@link notebook_write/3} for details.  ```1> sc:notebook_write("Test", "test", "test").
%% ok
%%
%% 2> sc:notebook_read("Test", "test").
%% {value,"test"}
%%
%% 3> sc:notebook_remove("Test", "test").
%% ok
%%
%% 4> sc:notebook_read("Test", "test").
%% undefined
%%
%% 5> sc:notebook_read("Not an existing notebook", "test").
%% ok'''

%% @since Version 753

notebook_remove(NotebookName, Key)

    when is_list(NotebookName) ->

    NT  = private_open_notebook_table(NotebookName),
    Res = private_remove_from_notebook_table(NT, Key),
    private_close_notebook_table(NT),

    Res.





%% @since Version 757

notebook_contents(NotebookName)

    when is_list(NotebookName) ->

    NT  = private_open_notebook_table(NotebookName),
    Res = private_get_contents_from_notebook_table(NT),
    private_close_notebook_table(NT),

    Res.





%% @equiv ensure_started(App, [])

%% @since Version 758

ensure_started(App) ->

    ensure_started(App, []).





%% @since Version 770

ensure_started(App, Opts)

    when is_atom(App) ->

    case apply(App, start, Opts) of
        ok                              -> ok;
        {error, {already_started, App}} -> ok;
        Other                           -> Other
    end.





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Creates a tuple of fixed width, repeating the given element the given number of times.  This is equivalent to `lists:duplicate'.  ```1> sc:tuple_duplicate(3,hi).
%% {hi,hi,hi}
%%
%% 2> sc:tuple_duplicate(0,hi).
%% {}
%%
%% Unit, doc, spec and stochastic (correct length, is tuple, first item is correct) tested.'''

%% @since Version 809

-spec tuple_duplicate(N::non_neg_integer(), Item::any()) -> tuple().

tuple_duplicate(N, Item) when is_integer(N), N >= 0 ->

    list_to_tuple(lists:duplicate(N, Item)).





% 30> sc:segment_size([{"dogs",8},{"cats",11},{"badgers",1},{"forever alone",0}]).
% { {population,20,3},
%   [ {"dogs",40.0,8},
%     {"cats",55.0,11},
%     {"badgers",5.0,1} ] }

%% @since Version 824

-spec segment_size(List::list(tuple(any(), number()))) -> tuple(tuple(list(), number(), non_neg_integer()), list(tuple(any(), float(), number()))).

segment_size(List) when is_list(List) ->

    Pop     = [ Count || {_Name, Count} <- List, Count > 0 ],
    PopSize = lists:sum(Pop),

    { {population, PopSize, length(Pop)}, [ {Name, (Count*100)/PopSize, Count} || {Name, Count} <- List, Count > 0 ] }.





%% @since Version 825

-spec unique_send_receive(ToWhom::pid()|atom(), What::any(), HowLong::non_neg_integer()|infinity) -> { got, Result::any() } | timeout.

unique_send_receive(ToWhom, Query, HowLong) ->

    R = make_ref(),
    ToWhom ! { match_return, self(), R, Query },

    receive

        { match_returned, R, Response } ->
            { got, Response }

    after

        HowLong ->
            timeout

    end.





%% @since Version 825

-spec unique_receive_respond(RespondFun::function()) -> responded | nothing_there.

unique_receive_respond(RespondFun) ->

    unique_receive_respond(RespondFun, 0).





unique_receive_respond(RespondFun, Timeout) ->

    receive

        { match_return, Sender, R, Query } when is_reference(R) ->
            Sender ! { match_returned, R, RespondFun(Query) }

    after
        Timeout -> nothing_there

    end.





%% @since Version 826

key_bucket(List) ->

    key_bucket(1, List).





%% @since Version 826

key_bucket(Index, List) ->

    [ {K, [ Val || { _Key, Val } <- Vals ]} || { K, Vals } <- key_cluster(Index, List) ].





%% @since Version 827

parallelize(Fun, DataSets) ->

    ResultHandle = make_ref(),
    HomeBase     = self(),
    Len          = length(DataSets),

    [ 
        spawn( 
            fun() -> 
                HomeBase ! { ResultHandle, Id, apply(Fun,[DataSet]) } 
            end 
        ) 
    || 
        { Id, DataSet } <- lists:zip( lists:seq(1,Len), DataSets ) 
    ],

    parallelize_gather([], Len, ResultHandle).





%% @since Version 827

parallelize_gather(Results, 0, _ResultHandle) ->

    [ Datum || { _Key, Datum } <- lists:keysort(1, Results) ];





parallelize_gather(Results, Remaining, ResultHandle) ->

    receive

        { ResultHandle, Id, Result } ->
            parallelize_gather( [{ Id, Result }] ++ Results, Remaining - 1, ResultHandle )

    end.





% todo enumerate_list -> lists:zip( seq(length), data )

% todo sorted_gather( [ {N, X}, ... ] ) -> [X, ...] for asc N

%% @doc <span style="color: green; font-weight: bold;">Tested</span> Returns the Solarized palette.  ```1>'''

%% Since Version 828

solarized() ->

    [ { base03,  16#002b36 },
      { base02,  16#073642 },
      { base01,  16#586e75 },
      { base00,  16#657b83 },
      { base0,   16#839496 },
      { base1,   16#93a1a1 },
      { base2,   16#eee8d5 },
      { base3,   16#fdf6e3 },
      { yellow,  16#b58900 },
      { orange,  16#cb4b16 },
      { red,     16#dc322f },
      { magenta, 16#d33682 },
      { violet,  16#6c71c4 },
      { blue,    16#268bd2 },
      { cyan,    16#2aa198 },
      { green,   16#859900 } ].





%% Since Version 828

%% @doc <span style="color: green; font-weight: bold;">Tested</span> Returns members of the Solarized palette.  ```1> sc:solarized(yellow).
%% 11897088
%%
%% 2> io:format("#~.16b~n", [sc:solarized(yellow)]).
%% #b58900'''


solarized(base03) ->  16#002b36;
solarized(base02) ->  16#073642;
solarized(base01) ->  16#586e75;
solarized(base00) ->  16#657b83;
solarized(base0) ->   16#839496;
solarized(base1) ->   16#93a1a1;
solarized(base2) ->   16#eee8d5;
solarized(base3) ->   16#fdf6e3;
solarized(yellow) ->  16#b58900;
solarized(orange) ->  16#cb4b16;
solarized(red) ->     16#dc322f;
solarized(magenta) -> 16#d33682;
solarized(violet) ->  16#6c71c4;
solarized(blue) ->    16#268bd2;
solarized(cyan) ->    16#2aa198;
solarized(green) ->   16#859900.






%% @since Version 829

has_debug_info(ModuleName) ->

    lists:member(debug_info,
        proplists:get_value(options,
            proplists:get_value(compile, erlang:get_module_info(ModuleName))
        )
    ).





%% @since Version 831

ad_rate(Searches, CPC, CTR, Conversion, RPC) when is_number(Searches), is_number(CPC), is_number(CTR), is_number(Conversion) ->

    Clicks    = Searches * CTR,
    Price     = Clicks * CPC,
    Customers = Clicks * Conversion,
    Net       = Customers * RPC,

    [ { price, Price }, { net, Net }, { profit, Net - Price } ].





%% @since Version 832

grab_first([X|_]) -> 

    X;





grab_first(T) when is_tuple(T) ->

    element(1, T).





%% @since Version 833

ww(Lim, Terms) ->

    ww(Lim, Terms, "", []).





ww(_Lim, [], CL, Acc) ->

    lists:reverse([CL] ++ Acc);





ww(Lim, [Next|Terms]=Whole, CL, Acc) ->

    NL = length(CL) + length(Next) + 1,

    case NL > Lim of

        true ->
            ww(Lim, Whole, "", [CL]++Acc);

        false ->
            ww(Lim, Terms, case CL of "" -> Next; Exist -> Exist ++ " " ++ Next end, Acc)

    end.





% predicate rate - [true,true,false,true,false] yields 0.6, and non-bool yields error

%% @doc The predicate rate of a list - counts a list of boolean values, and returns their rate of true values.  Also called `truth_density/1', pred_rate
%% returns the fraction of the list which is true, or 1.0 if they're all true.  The function caps at 1.0 for asymptopes (pure-true lists.)  ```1> sc:pred_rate( [true,false] ).
%% 0.5
%%
%% 2> sc:pred_rate( [false,false,false] ).
%% 0.0
%%
%% 3> sc:pred_rate( [true,true,true] ).
%% 1.0
%%
%% 4> sc:pred_rate( [] ).
%% undefined
%%
%% 5> sc:pred_rate( [ true,true,true,true, false,false,false ] ).
%% 0.5714285714285714'''

%% @since Version 834

-spec pred_rate( [boolean()] ) -> number().

pred_rate(L) when is_list(L) ->

    pred_rate(L, 0, 0).





pred_rate([], 0, 0) ->

    undefined;    % the predicate rate of the empty list is undefined





pred_rate([], _For, 0) ->

    1.0;          % hand-set any against-0 empty count to 1.0 to prevent divide by zero at the asymptope





pred_rate([], For, Against) ->

    For / (For + Against);





pred_rate([true|Lr], For, Against) ->

    pred_rate(Lr, For+1, Against);





pred_rate([false|Lr], For, Against) ->

    pred_rate(Lr, For, Against+1).





%% @since Version 834

%% @equiv pred_rate(L)

-spec truth_density( [boolean()] ) -> number().

truth_density(L) ->

    pred_rate(L).





%% @since Version 835

ms_wang() ->

    [ {r,y,g,b}, {g,b,g,b}, {r,y,r,y}, {g,b,r,y}, {r,b,g,y}, {g,y,g,y}, {r,b,r,b}, {g,y,r,b} ].





%% @since Version 835

wang_row(Width) ->

    wang_row(Width, top_row, 1, '_', []).





%% @since Version 835

wang_row(Width, PriorRow) ->

    wang_row(Width, PriorRow, 1, '_', []).





%% @since Version 835

wang_row(0, top_row, _At, _Prev, Work) ->

    list_to_tuple(lists:reverse(Work));





wang_row(Remaining, top_row, At, Prev, Work) ->

    N = wang_such_that(case Prev of '_' -> '_'; {_,R,_,_} -> R end, '_'),

    wang_row(Remaining-1, top_row, At+1, N, [N] ++ Work);





wang_row(0, _OldTopRow, _At, _Prev, Work) ->

    list_to_tuple(lists:reverse(Work));





wang_row(Remaining, OldTopRow, At, Prev, Work) ->

    T = element(At, OldTopRow),
    N = wang_such_that(case Prev of '_' -> '_'; {_,R,_,_} -> R end, case T of '_' -> '_'; {_,_,B,_} -> B end),

    wang_row(Remaining-1, OldTopRow, At+1, N, [N] ++ Work).





%% @since Version 835

wang_such_that(Left, Top) ->

    wang_such_that(Left, Top, ms_wang()).





%% @since Version 835

wang_such_that('_', '_', Tiles) ->

    sc:random_from(Tiles);





wang_such_that('_', Top, Tiles) ->

    sc:random_from([ Satisfied || { T,_R,_B,_L}=Satisfied <- Tiles, T =:= Top  ]);





wang_such_that(Left, '_', Tiles) ->

    sc:random_from([ Satisfied || {_T,_R,_B, L}=Satisfied <- Tiles, L =:= Left  ]);





wang_such_that(Left, Top, Tiles) ->

    sc:random_from([ Satisfied || { T,_R,_B, L}=Satisfied <- Tiles, T =:= Top, L =:= Left  ]).





%% @since Version 835

-spec wang_carpet( integer(), integer() ) -> tuple( tuple( integer() ) ).

wang_carpet(X, Y) ->

    wang_carpet(X,Y, []).





%% @since Version 835

wang_carpet(X, Y, Tiles) ->

    wang_carpet(X, Y, Tiles, top_row, []).





%% @since Version 835

wang_carpet(_Width, 0, _Tiles, _LastRow, Work) ->

    list_to_tuple(lists:reverse(Work));





wang_carpet(Width, RowsLeft, Tiles, LastRow, Work) ->

    NewRow = wang_row(Width, LastRow),

    wang_carpet(Width, RowsLeft-1, Tiles, NewRow, [NewRow] ++ Work).






%% @since Version 836

unixtime() ->

    erlang:universaltime_to_posixtime(erlang:universaltime()).






%% @since Version 836

unixday() ->

    sc:floor(unixtime() / 86400).






%% @since Version 836

unixday(FromTime) ->

    sc:floor(FromTime / 86400).





%% @since Version 836

unixtime_daybase() ->

    sc:floor(unixtime() / 86400) * 86400.





%% @since Version 836

unixtime_daybase(FromTime) ->

    sc:floor(FromTime / 86400) * 86400.





%% @since Version 837

frand() ->

    case is_rand_seeded() of
        true  -> srand();
        false -> ok
    end,

    random:uniform().





%% @since Version 837

frand_between(X,Y) when X < Y ->

    X + ((Y-X) * frand()).





%% @since Version 837

is_rand_seeded() ->

    case get(random_seed) of

        {A,B,C} when is_integer(A), is_integer(B), is_integer(C) -> true;
        _Defined                                                 -> false

    end.





%% @since Version 838

funnel(Base, Percents) ->

    funnel([Base], Base, Percents).





funnel(Work, _Last, []) ->

    lists:reverse( Work );





funnel(Work, Last, [NextPct | Pcts]) ->

    Step = Last * NextPct,

    funnel([Step]++Work, Step, Pcts).





%% @since Version 839

list_cross_multiply(L) -> 

    [ sc:list_product(tuple_to_list(Li)) || Li  <- sc:zip_n(L) ].






%% @since Version 840

outcomes(Opts, Scorer) ->

    sc:histograph([ Scorer(Poss) || Poss <- sc:every_member_representation(Opts) ]).





%% @since Version 840

histo_2d(Data) ->

    [ sc:histograph(tuple_to_list(Cols)) || Cols <- sc:zip_n(Data) ].





%% @since Version 841

logb(Base, Value) ->

    math:log(Value) / math:log(Base).





%% @since Version 842

bucket(Position, [First|_] = ListOfLists) when is_list(First) ->

    {Cats,_} = lists:unzip(sc:histograph([ lists:nth(Position, Col) || Col <- ListOfLists ])),

    [ { Cat, [ Row || Row <- ListOfLists, lists:nth(Position, Row) =:= Cat ] } || Cat <- Cats ].





%% @since Version 844

rand_between(Lo, Hi) when is_number(Lo), is_number(Hi), Lo =< Hi ->

    rand(Hi - Lo) + Lo.





%% @since Version 844

htget(Thing) ->

    inets:start(),

    case httpc:request(Thing) of
        
        {ok,{{_,RCode,_},_,Ret}} -> {RCode, Ret};
        Other                    -> {error, Other}

    end.
