
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 262 $
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





-module(sc.i18n).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: i18n.erl 262 2009-03-23 02:28:23Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/src/sc/i18n.erl $").
-svn_revision("$Revision: 262 $").

-description("Internationalization utilities for erlang.").

-testerl_export( { [], sc_i18n_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    is_numeric_char/1,
      is_numeric_char/2

] ).





% count_words(Source) -> count_words(Source, keep_hyphens).
%
% count_words(Source, keep_hyphens) -> count_words(Source, keep_hyphens, 0, not_word).
%
% count_words([],    _Hyphens,      Count,_WordState) -> Count;
% count_words(Source, keep_hyphens, Count, not_word)  ->





%% @since Version 219

is_numeric_char(Ch) -> is_numeric_char(Ch, decimal).

is_numeric_char(Ch, decimal) when $0 =< Ch, Ch =< $9; Ch == $-; Ch == $. -> true;
is_numeric_char(_, _)                                                    -> false.
