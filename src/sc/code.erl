
% can't be called module because it's a keyword, which sucks





-module(sc.mod).

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

    abstract/1,            % needs tests
    abstract/2             % needs tests

] ).





%% @since Version 130

% was `scutil:abstract_code/1'

abstract(Module) ->

    abstract_code(Module, unstripped).





%% @since Version 130

% was `scutil:abstract_code/2'

abstract(Module, DoStrip) ->

    case get_module_feature(Module, abstract_code) of

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





