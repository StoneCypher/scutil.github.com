
-module(dq).

-export([

    new/0, new/1, new/2,
    queue_size/1,

    normalize/1,  push/2,  pop/1,  peek/1
%   rnormalize/1, rpush/2, rpop/1, rpeek/2,

%   split/1, split/2,   % 1,2,3,4 -> 12, 34
%   dole/1,  dole/2,    % 1,2,3,4 -> 13, 24
    set_dole/2,

%   merge/2,
%   filter/2

]).





-record(dq_queue, {inlist, outlist, dole_count}).





%% @since Version 1
new() ->

    #dq_queue{ inlist=[], outlist=[], dole_count=2 }.




%% @since Version 1
new(X) ->

    #dq_queue{ inlist=[], outlist=[X], dole_count=2 }.





%% @since Version 1
new(X, DoleCount) ->

    #dq_queue{ inlist=[], outlist=[X], dole_count=DoleCount }.





%% @since Version 1
normalize(Queue) when is_record(Queue, dq_queue) ->

    #dq_queue{ inlist=[], outlist=Queue#dq_queue.outlist ++ lists:reverse(Queue#dq_queue.inlist) }.





%% @since Version 1
push(Item, Queue) when is_record(Queue, dq_queue) ->

    Queue#dq_queue{inlist=[Item]++Queue#dq_queue.inlist}.





%% @since Version 1
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





%% @since Version 1
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





%% @since Version 1
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





%% @since Version 1
queue_size(Queue) when is_record(Queue, dq_queue) ->

    length(Queue#dq_queue.inlist) +
    length(Queue#dq_queue.outlist).
