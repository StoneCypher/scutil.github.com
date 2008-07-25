
-module(scutil_testsuite).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-license( {mit_license, "http://crunchyd.com/scutil/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=7").
-publicforum("http://crunchyd.com/forum/scutil-discussion/").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

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
        testerl:must_equal("binary type_of()",    fun scutil:type_of/1, [<<1>>],           binary),
        testerl:must_equal("boolean type_of()",   fun scutil:type_of/1, [true],            boolean),
        testerl:must_equal("function type_of()",  fun scutil:type_of/1, [fun(X) -> X end], function),
        testerl:must_equal("pid type_of()",       fun scutil:type_of/1, [self()],          pid),
        testerl:must_equal("port type_of()",      fun scutil:type_of/1, [APort],           port),
        testerl:must_equal("reference type_of()", fun scutil:type_of/1, [make_ref()],      reference),
        testerl:must_equal("atom type_of()",      fun scutil:type_of/1, [lol],             atom)
    ].





run(_Hooks, _Options) ->

    [
        testerl:assemble("type_of()", test_type_of())
%        testerl:assemble("get_module_attribute()", test_get_module_attribute())
    ].
