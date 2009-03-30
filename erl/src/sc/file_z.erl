
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision: 251 $
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p>This module is currently called file_z because a defect in the Erlang VM means that if this module is compiled as file, it will override the official file module during the boot process, breaking the VM.</p>



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





-module(sc.file_z).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id: file.erl 251 2009-03-22 06:14:12Z john $").
-svn_head("$HeadURL: svn://crunchyd.com/scutil/src/sc/file.erl $").
-svn_revision("$Revision: 251 $").

-description("File and filesystem utilities.").

-testerl_export( { [], sc_file_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [
    sanitize_filename/1
] ).





%% @spec sanitize_filename(Filename::string()) -> string()

%% @doc {@section String} Sanitize an arbitrary string to be appropriate for Windows and Unix filesystems, and URLs. ```1> scutil:sanitize_filename("\h/e~l%lo! w^o@r#l*d.").
%% "helloworld"'''

%% @see sanitize_tokens/2

%% @since Version 31

sanitize_filename(Filename) ->

    .sc.list:sanitize_tokens(
        Filename,
        .lists:seq($a,$z) ++
          .lists:seq($A,$Z) ++
          .lists:seq($0,$9) ++
          "-_()[]"
    ).
