
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

    [   testerl:must_equal("integer type_of()",  fun scutil:type_of/1, [1],               integer),
        testerl:must_equal("float type_of()",     fun scutil:type_of/1, ["1"],             list),
        testerl:must_equal("list type_of()",     fun scutil:type_of/1, ["1"],             list),
        testerl:must_equal("binary type_of()",   fun scutil:type_of/1, [<<1>>],           binary),
        testerl:must_equal("function type_of()", fun scutil:type_of/1, [fun(X) -> X end], function)

type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_number(X)    -> number;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
%type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X)    -> binary;     % redundant in e12
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;

    ].





run(_Hooks, _Options) ->

    [
        testerl:assemble("type_of()", test_type_of())
%        testerl:assemble("get_module_attribute()", test_get_module_attribute())
    ].
