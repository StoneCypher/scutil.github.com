
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





-module(sc_message).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-twitter({"JohnHaugeland", "http://twitter.com/JohnHaugeland"}).
-twitter({"ScUtil", "http://twitter.com/ScUtil"}).
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker(none).
-publicforum(none).
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").
-svn_date("$Date$").

-description("Utilities for message passing and receiving.").

-todo([unit_tests,stochastic_tests,lib_test_rig_integrate,docs,doc_examples,doc_extraction]).

-testerl_export( { [], sc_message_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [
    receive_one/0,
    send_receive/3
] ).





%% @spec receive_one() -> { item, any() } | nothing_there

%% @doc {@section Utility} Pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> scutil:receive_one().
%% nothing_there
%%
%% 2> self() ! message.
%% message
%%
%% 3> scutil:receive_one().
%% {item,message}
%%
%% 4> scutil:receive_one().
%% nothing_there'''

%% @since Version 2

receive_one() ->

    receive (X) ->
        { item, X }
    after 0 ->
        nothing_there
    end.





%% @equiv send_receive(ToWhom, What, infinity)

send_receive(ToWhom, What) ->

    send_receive(ToWhom, What, infinity).





%% @spec send_receive(ToWhom::pid()|atom(), What::any(), HowLong::non_negative_integer()|infinity) -> { item, any() } | nothing_there

%% @doc {@section Utility} First send a message to an entity.  Then pop the front of the message queue and return it as `{item,X}', or return nothing_there for empty queues; do not block.  ```1> scutil:send_receive(self(), message).
%% {item,message}'''

%% @since Version 434

send_receive(ToWhom, What, HowLong) ->

    ToWhom ! What,

    receive (X) ->
        { item, X }
    after HowLong ->
        nothing_there
    end.
