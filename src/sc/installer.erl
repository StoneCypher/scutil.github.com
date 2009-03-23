
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



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





-module(sc.installer).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Installer for the scutil library collection.").

-testerl_export( { [], sc_install_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    contained_modules/0,      % needs tests
    gen_docs/1,               % needs tests

    compile_all/1,            % needs tests
      compile_all/2,          % needs tests

    install/1,                % needs tests
      install/2,              % needs tests

    verify_install/0          % needs tests

] ).





%% @since Version 246

contained_modules() ->

    [   string,

        bayes,
        bit,
       [bridges,     local,       cpp],
       [bridges,     net,         actionscript],
       [bridges,     net,         javascript],
        code,
       [code,        metrics],
        columns,
        convert,
       [convert,     weight],
        counter,
       [distance,    euclidean],
        file,
        is,
       [irc,         client],
        i18n,
        list,
        math,
       [math,        vector],
        message,
        module,
        net,
        note,
        parallelism,
        process,
        purity,
        random,
        record,
        regex,
        serialism,
        signal,
        stats,
       [stats,       cluster],
       [stats,       correlation],
       [stats,       ranks],
        text,
        time,
        tuple
    ].





%% @since Version 152

gen_docs( [From, To] ) ->

    .edoc:application(scutil, From, [{dir,To},{new,true}] ).





%% @private

%% @since Version 260

standard_compile_options() ->

    [debug_info, return, warn_export_all, warn_export_vars, warn_obsolete_guard, warn_unused_import].





%% @since Version 244

compile_all(From) ->

    compile_all(From, standard_compile_options()).



compile_all(From, Options) ->



    ReportOnCompile = fun
        ( Module, error)                                 -> { error, Module };
        ( Module, {error, ErrorList, WarningList})       -> { error, Module, ErrorList, WarningList };
        (_Module, {ok,AtomName})                         -> AtomName;
        (_Module, {ok,AtomName, Warnings})               -> { AtomName, Warnings }
    end,

    IsFailureCase = fun
        ({ error,_Module })                         -> true;
        ({ error,_Module,_ErrorList,_WarningList }) -> true;
        (_)                                         -> false
    end,

    IsWarningCase = fun
        ({ ModuleAtom, [] })       -> false;
        ({ ModuleAtom, Warnings }) -> true
    end,

    ToFilename = fun
        (List) when is_list(List) -> .sc.string:implode("/", [ atom_to_list(Item) || Item <- List ]) ++ ".erl";
        (Atom) when is_atom(Atom) -> atom_to_list(Atom) ++ ".erl"
    end,

    Report = [ ReportOnCompile(From ++ ToFilename(File), .compile:file(From ++ ToFilename(File), Options)) ||
        File <- contained_modules()
    ],

    { Fail, PassWarn } = .lists:partition(IsFailureCase, Report),
    { Warn, Pass     } = .lists:partition(IsWarningCase, PassWarn),

    { { pass, Pass }, { warn, Warn }, { fail, Fail } }.





%% @since Version 244

install(From) ->

    install(From, standard_compile_options()).




%% @since Version 246

install(From, CompileOptions) ->

    Compiled = compile_all(From ++ "src/sc/", CompileOptions),
    % gen_docs([From ++ "src/", From ++ "doc/"]),

    case verify_install() of

        correct ->
            { ok, installed, Compiled };

        { broken, Reason } ->
            { broken, Reason, Compiled }

    end.





%% @since Version 244

verify_install() ->

    % check module dependencies
    % run module tests

    { broken, "The install verifier has not yet been written, so install verification cannot yet succeed." }.
