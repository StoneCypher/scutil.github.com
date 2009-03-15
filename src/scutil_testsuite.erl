
-module(scutil_testsuite).
-behaviour(testerl_testsuite_behaviour).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-license( {mit_license, "http://crunchyd.com/scutil/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=7").
-publicforum("http://crunchyd.com/forum/scutil-discussion/").

-svn_id("$Id: scutil_testsuite.erl 60 2008-11-27 06:04:39Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/scutil_testsuite.erl $").
-svn_revision("$Revision: 60 $").

-testerl_export( this_is_a_testsuite ).

-description("The testerl testsuite for scutil").





-export([
    run/2
]).





test_type_of() ->

    [APort | _Discard] = erlang:ports(),

    [   testerl:must_equal("integer type_of()",   fun scutil:type_of/1, [1],               integer),
        testerl:must_equal("float type_of()",     fun scutil:type_of/1, [1.1],             float),
        testerl:must_equal("list type_of()",      fun scutil:type_of/1, ["1"],             list),
        testerl:must_equal("tuple type_of()",     fun scutil:type_of/1, [{1}],             tuple),
%       testerl:must_equal("binary type_of()",    fun scutil:type_of/1, [<>],              bitstring),
        testerl:must_equal("binary type_of()",    fun scutil:type_of/1, [<<1>>],           binary),
        testerl:must_equal("boolean type_of()",   fun scutil:type_of/1, [true],            boolean),
        testerl:must_equal("function type_of()",  fun scutil:type_of/1, [fun(X) -> X end], function),
        testerl:must_equal("pid type_of()",       fun scutil:type_of/1, [self()],          pid),
        testerl:must_equal("port type_of()",      fun scutil:type_of/1, [APort],           port),
        testerl:must_equal("reference type_of()", fun scutil:type_of/1, [make_ref()],      reference),
        testerl:must_equal("atom type_of()",      fun scutil:type_of/1, [lol],             atom)
    ].





test_get_module_attribute() ->

    [   testerl:must_equal("existing attribute", fun scutil:get_module_attribute/2, [scutil_testsuite, testerl_export],  this_is_a_testsuite),
        testerl:must_equal("missing attribute",  fun scutil:get_module_attribute/2, [scutil_testsuite, testerl_seaport], {error, no_such_attribute}),
        testerl:must_equal("nonexistant module", fun scutil:get_module_attribute/2, [scutil_roflsuite, testerl_export],  {error, no_such_module})
%       testerl:must_crash("bunk data",          fun scutil:get_module_attribute/2, [{la,te,[da]},     <<"rofl">>])
    ].





%test_byte_to_hex() ->
%
%    [   testerl:must_equal("





test_sanitize_tokens() ->

    [   testerl:must_equal("strings",     fun scutil:sanitize_tokens/2, ["abcdedcbabcde",       "ace"],                   "acecace"),
        testerl:must_equal("atom lists",  fun scutil:sanitize_tokens/2, [[north,south,up,down], [north,south,east,west]], [north,south]),
        testerl:must_equal("mixed types", fun scutil:sanitize_tokens/2, [[north,2,"de",{t}],    [north,{t}]],             [north,{t}]),
        testerl:must_equal("predicate",   fun scutil:sanitize_tokens/2, [[a,1,b,2,c,3,d,4,e,5], fun erlang:is_integer/1], [1,2,3,4,5])
    ].





run(_Hooks, _Options) ->

    [   testerl:assemble("type_of()",              test_type_of()),
        testerl:assemble("get_module_attribute()", test_get_module_attribute()),
        testerl:assemble("sanitize_tokens()",      test_sanitize_tokens())
%       testerl:assemble("byte_to_hex()",          test_byte_to_hex()),
%       testerl:assemble("nybble_to_hex()",        test_nybble_to_hex()),
%       testerl:assemble("io_list_to_hex()",       test_io_list_to_hex())
    ].
