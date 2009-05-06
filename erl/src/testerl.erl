
-module(testerl).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://testerl.com/").
-twitter("JohnHaugeland").
-license( {mit_license, "http://crunchyd.com/testerl/license.html"} ).

-publicsvn("svn://crunchyd.com/testerl/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=4").
-publicforum("http://crunchyd.com/forum/testerl-discussion/").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-testerl_export( { [], testerl_testsuite } ).
-library_requirements( [ {scutil,55} ] ).

-description("The testerl erlang testsuite system").





-export([

    test/1, test/2,
    assemble/2,
    get_suite_bindings/1,

    must_equal/4,
    must_equal_epsilon/5,
    must_fail/2,

    check_library_requirements/1

]).





test(Module) -> test(Module, []).

test(Module, Options) ->

    case check_library_requirements(Module) of

       {fail,_,X} -> {fail, "Library requirements for module not satisfied", X};
       {pass,_}   ->

            case get_suite_bindings(Module) of
                {error, E}                              -> {error, E};
                {testerl_bindings, Hooks, TestModule}   -> start_tests(Hooks, TestModule, Options);
                {testerl_bindings, no_testsuite}        -> {warning, {no_testsuite, Module}};
                {testerl_bindings, this_is_a_testsuite} -> {pass, {this_is_a_testsuite, Module}};
                Other                                   -> {error, {cannot_parse, Other}}
            end

    end.





assemble(Name, Calls) when is_list(Calls) -> assemble_worker(Name, Calls, [], [], []);
assemble(Name, Calls)                     -> assemble(Name, [Calls]).

assemble_worker(Name, [], Pass, Warn, Fail) ->

    Status = if
        length(Fail) > 0 -> fail;
        length(Warn) > 0 -> warn;
        true             -> pass
    end,

    {Status, Name, { {pass,Pass}, {warn,Warn}, {fail,Fail} } };

assemble_worker(Name, [Item|Rem], Pass,Warn,Fail) ->

    case Item of
        {pass,Title,Data} -> assemble_worker(Name, Rem, [{Title,Data}]++Pass, Warn,                 Fail);
        {warn,Title,Data} -> assemble_worker(Name, Rem, Pass,                 [{Title,Data}]++Warn, Fail);
        {fail,Title,Data} -> assemble_worker(Name, Rem, Pass,                 Warn,                 [{Title,Data}]++Fail);
        Other             -> { error, { illegal_assemble_item, Other } }
    end.





start_tests(Hooks, TestModule, Options) ->

    assemble(TestModule, TestModule:run(Hooks, Options)).





get_suite_bindings(Module) when is_atom(Module) ->

    case scutil:get_module_attribute(Module, testerl_export) of
        [{error, no_such_attribute}] ->
            case scutil:get_module_attribute(Module, testerl) of
                {error, no_such_attribute}      -> {error, {no_test_suite}};
                OldSuite when is_atom(OldSuite) -> {error, {update_test_format, {Module,OldSuite}}};
                Other                           -> {error, {cannot_parse, Other}}
            end;
        [{Hooks, TestModule}] -> {testerl_bindings, Hooks, TestModule};
        [no_testsuite]        -> {testerl_bindings, no_testsuite};
        Other                 -> {error, {cannot_parse, Other}}
    end.





must_equal_epsilon(Title, Func, Args, Value, Epsilon) ->

    Result = erlang:apply(Func, Args),
    case math:abs(Value - Result) < Epsilon of
        true  -> {pass, Title, []};
        false -> {fail, Title, {must_equal_epsilon, Value, Epsilon, got, Result}}
    end.





must_equal(Title, Func, Args, ExpectedResult) ->

    case erlang:apply(Func, Args) of
        ExpectedResult -> {pass, Title, []};
        OtherVal       -> {fail, Title, {must_equal, ExpectedResult, got, OtherVal}}
    end.





must_fail(Title, {fail,_,_})      -> {pass, Title, []};
must_fail(Title, Other)           -> {fail, Title, {must_fail, Other}}.





check_library_requirements(Lib) ->

    case scutil:get_module_attribute(Lib, library_requirements) of
        {error, no_such_module}    -> {error, no_such_module};
        {error, no_such_attribute} -> {error, {out_of_date_testerl_api, library_requirements_not_specified}};
        Reqs                       -> check_each_library_requirement(Reqs,[])
    end.





check_each_library_requirement([],                  [])   -> { pass, "Library prerequisites satisfied" };
check_each_library_requirement([],                  Work) -> { fail, "Library prerequisites not satisfied", Work };
check_each_library_requirement([ {Lib,Ver} | Rem ], Work) ->

    LocalVer = scutil:scan_svn_revision(Lib),
    case LocalVer >= Ver of
        true  -> check_each_library_requirement(Rem, Work);
        false -> check_each_library_requirement(Rem, [{{requires,Lib,Ver}, {current,LocalVer}}]++Work)
    end.
