
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 250 $
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





-module(sc.regex).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: regex.erl 250 2009-03-22 04:45:49Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/src/sc/regex.erl $").
-svn_revision("$Revision: 250 $").

-description("Regular expression convenience tools.").

-testerl_export( { [], sc_regex_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [
    regex_read_matches/2,
      regex_read_matches/3,
      regex_read_matches/4
] ).





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

    case .regexp:matches(String, Reg) of

        { match, Matches } ->
            [ .string:substr(String, Start+TrimFront, End-(TrimLength+1)) || {Start,End} <- Matches ];

        { error, E } ->
            { error, E }

    end.
