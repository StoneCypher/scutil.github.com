
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-04-05 16:16:32 -0600 (Sun, 05 Apr 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc.code).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("StoneCypher's utility library.").

-testerl_export( { [], sc_code_testsuite } ).

-library_requirements( [] ).





-export( [

    abstract/1,            % needs tests
    abstract/2,            % needs tests

    eval/1,                % needs tests
    eval/2                 % needs tests

] ).





%% @since Version 130

% was `scutil:abstract_code/1'

abstract(Module) ->

    abstract(Module, unstripped).





%% @since Version 130

% was `scutil:abstract_code/2'

abstract(Module, DoStrip) ->

    case .sc.module:feature(Module, abstract_code) of

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





%% @since Version 146
% modified from http://www.trapexit.org/String_Eval

eval(S) ->

    eval(S,erl_eval:new_bindings()).





%% @since Version 146
% from http://www.trapexit.org/String_Eval

eval(S, Environ) ->

    {ok, Scanned,_} = erl_scan:string(S),
    {ok, Parsed}    = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed,Environ).
