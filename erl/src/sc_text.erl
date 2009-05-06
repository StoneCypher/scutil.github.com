
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date$</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(sc_text).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Encoding ignorant text and text processing facilities.").

-testerl_export( { [], sc_text_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [

    calc_fk_readability/3,
    fk_readability/4,
    labelled_fk_readability/1,
    to_lines/1

] ).





% text facilities.  all are language independant, based on i18n.erl .





% http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test
% http://www.readabilityformulas.com/graphics/fleschresults.gif

%% @since Version 131

calc_fk_readability(Words, Sentences, Syllables) ->

    206.835 - (1.015 * (Words/Sentences)) - (84.6 * (Syllables/Words)).





% http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test

%% @since Version 131

% todo
% fk_readability(Data) -> fk_readability(Data, fun count_words/1, fun count_sentences/1, fun count_syllables/1).

fk_readability(Data, WordCounter, SentenceCounter, SyllableCounter) ->

    Words     = WordCounter(Data),
    Sentences = SentenceCounter(Data),
    Syllables = SyllableCounter(Data),

    labelled_fk_readability(
      calc_fk_readability(Words, Sentences, Syllables)
    ).





%% @since Version 131

labelled_fk_readability(R) when R > 100 -> { easy_before_11_years,     R };
labelled_fk_readability(R) when R >  90 -> { easy_at_11_years,         R };
labelled_fk_readability(R) when R >  70 -> { easy_for_11_to_13_years,  R };
labelled_fk_readability(R) when R >  60 -> { easy_for_13_to_15_years,  R };
labelled_fk_readability(R) when R >  30 -> { appropriate_for_15_years, R };
labelled_fk_readability(R) when R >   0 -> { appropriate_for_college,  R };
labelled_fk_readability(R)              -> { difficult,                R }.





%% @type stringlist() = list().  Every member of a stringlist() is a string().

%% @spec to_lines(Text::string()) -> stringlist()

%% @doc {@section String} Cuts a string according to any of the three newline conventions (even mixed), and discards empty strings. ```1> scutil:to_lines("one\rtwo\nthree\r\nfour\r\r\rfive").
%% ["one","two","three","four","five"]'''

%% @since Version 2

to_lines(Text) ->

    string:tokens(Text, "\r\n"). % yay convenience functions





%% @todo finish me

% dissimilar_charset(english, lowercase) -> "abcdefghjklmnopqrstuwxyz";
% dissimilar_charset(english, mixedcase) -> "abcdefghjklmnopqrstuwxyzABDEFGHRT";
% dissimilar_charset(english, alphanum)  -> "abcdefghjklmnopqrstuwxyzABDEFGHRT34679".

% similarize_charset   a10OZ2B8 -> aloozzBB
