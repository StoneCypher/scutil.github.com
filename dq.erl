
% part of scutil, see scutil.erl for documentation generation instructions
% don't have the whole package?  get it at http://scutil.com/
% MIT license because the GPL is a menace - http://WhyIHateTheGPL.com/

%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since March 3, 2009

%% @doc <p>Docs pending.  TODO</p>

%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Direct link to zip archive</span><a href="http://crunchyd.com/release/scutil.zip">Current version</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Public SVN at</span><a href="svn://crunchyd.com/scutil/">svn://crunchyd.com/scutil/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Discussion forum at</span><a href="http://crunchyd.com/forum/scutil-discussion/">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Bugtracker at</span><a href="http://crunchyd.com/forum/project.php?projectid=7">CrunchyD Forums</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-03-03 23:27:18 -0700 (Tue, 03 Mar 2009) $</tt></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Part of</span><a href="http://scutil.com/" title="The ScUtil Library">The ScUtil Library</a></span>
%% @reference <span style="margin-top:1em;padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Test sets require min. version 16</span><a href="http://testerl.com/">TestErl</a></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add warnings re: spearman, pearson, kendall use on lists containing repetitions
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





-module(dq).

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
-library_requirements( [ {scutil,141}, {testerl,16} ] ).

-export([

    new/0, new/1, new/2,
    queue_size/1,

    is_dq_queue/1,
    is_empty/1,

    normalize/1,  push/2,  pop/1,  peek/1,
    rnormalize/1, rpush/2, rpop/1, rpeek/1,

%   split/1, split/2,   % 1,2,3,4 -> 12, 34
%   dole/1,  dole/2,    % 1,2,3,4 -> 13, 24
    set_dole/2

%   merge/2,
%   filter/2,
%   fold/2,

%   sort/1,
%   usort/1,

%   from_list/1,   to_list/1,
%   from_binary/1, to_binary/1,

]).





-record(dq_queue, {inlist, outlist, dole_count}).





%% @since Version 141
new() ->

    #dq_queue{ inlist=[], outlist=[], dole_count=2 }.




%% @since Version 141
new(X) ->

    #dq_queue{ inlist=[], outlist=[X], dole_count=2 }.





%% @since Version 141
new(X, DoleCount) ->

    #dq_queue{ inlist=[], outlist=[X], dole_count=DoleCount }.





%% @since Version 142
is_dq_queue(Queue) when is_record(Queue, dq_queue) -> true;
is_dq_queue(_Queue)                                -> false.





%% @since Version 145
is_empty({dq_queue, [], [], _}) -> true;
is_empty(_)                     -> false.





%% @since Version 141
normalize(Queue) when is_record(Queue, dq_queue) ->

    #dq_queue{ inlist=[], outlist=Queue#dq_queue.outlist ++ lists:reverse(Queue#dq_queue.inlist) }.





%% @since Version 144
rnormalize(Queue) when is_record(Queue, dq_queue) ->

    #dq_queue{ outlist=[], inlist=Queue#dq_queue.inlist ++ lists:reverse(Queue#dq_queue.outlist) }.





%% @since Version 143
set_dole(Queue, DoleCount) when is_record(Queue, dq_queue), is_integer(DoleCount) ->

    Queue#dq_queue{ dole_count=DoleCount }.





%% @since Version 141
push(Item, Queue) when is_record(Queue, dq_queue) ->

    Queue#dq_queue{inlist=[Item]++Queue#dq_queue.inlist}.





%% @since Version 144
rpush(Item, Queue) when is_record(Queue, dq_queue) ->

    Queue#dq_queue{outlist=[Item]++Queue#dq_queue.outlist}.





%% @since Version 141
pop(Queue) when is_record(Queue, dq_queue) ->

    case Queue#dq_queue.outlist of

        [] ->
            case Queue#dq_queue.inlist of
                [] -> empty;
                In ->
                    [Head|Rem] = In,
                    { Head, #dq_queue{ inlist=[], outlist=lists:reverse(Rem) } }
            end;

        [Out|OutRem] ->
            { Out, Queue#dq_queue{outlist=OutRem} }

    end.





%% @since Version 144
rpop(Queue) when is_record(Queue, dq_queue) ->

    case Queue#dq_queue.inlist of

        [] ->
            case Queue#dq_queue.outlist of
                []  -> empty;
                Out ->
                    [Head|Rem] = Out,
                    { Head, #dq_queue{ outlist=[], inlist=lists:reverse(Rem) } }
            end;

        [In|InRem] ->
            { In, Queue#dq_queue{inlist=InRem} }

    end.





%% @since Version 141
peek(Queue) when is_record(Queue, dq_queue) ->

    case Queue#dq_queue.outlist of

        [] ->
            case Queue#dq_queue.inlist of
                [] -> empty;
                In -> lists:last(In)
            end;

        [OutHead|_OutRem] ->
            OutHead

    end.





%% @since Version 144
rpeek(Queue) when is_record(Queue, dq_queue) ->

    case Queue#dq_queue.inlist of

        [] ->
            case Queue#dq_queue.outlist of
                []  -> empty;
                Out -> lists:last(Out)
            end;

        [InHead|_InRem] ->
            InHead

    end.





%% @since Version 141
queue_size(Queue) when is_record(Queue, dq_queue) ->

    length(Queue#dq_queue.inlist) +
    length(Queue#dq_queue.outlist).
