
-module(sc.list).

-export( [
    extrema/1
] ).




%% @since Version 221

% was `scutil:extrema_of/1'
extrema(List) ->

    [First | _] = List,

    lists:foldl(

        fun(Next,T) ->

            io:format("~w ~w~n", [T,Next]),
            {Hi, Lo} = T,

            Lo2 = if
                Next < Lo ->
                    Next;
                true ->
                    Lo
            end,

            Hi2 = if
                Next > Hi ->
                    Next;
                true ->
                    Hi
            end,

            {Hi2, Lo2}

        end,

        {First,First},
        List
    ).





% Like binary_to_term, but not so much for binaries
% thanks dizzyd (modified for error reporting)

%% @since Version 216

list_to_term(List) ->

    case catch erl_scan:string(List) of

        { ok, Tokens, _ } ->

            case erl_parse:parse_term( Tokens ++ [{ dot, 1 }] ) of
                { ok,Term } -> Term;
                Error       -> { error, Error }
            end;

        Error -> { error, Error }

    end.





%% @since Version 200

key_duplicate(KeyList) ->

    lists:flatten( [ lists:duplicate(Key, Value) || {Key,Value} <- KeyList ] ).





%% @since Version 168

list_rotate(0, List) ->

    List;



list_rotate(By, List) when By =< (-(length(List))) ->
    
    list_rotate(By rem length(List), List);



list_rotate(By, List) when By < 0 ->

    list_rotate(length(List) + By, List);



list_rotate(By, List) when By >= length(List) ->

    list_rotate(By rem length(List), List);



list_rotate(By, List) ->

    { Front, Rear } = lists:split(By, List),
    Rear ++ Front.





%% @since Version 169

index_of_first(Item, List) ->

    index_of_first(Item, List, 1).



index_of_first(_Item, [], _Pos) ->

    undefined;



index_of_first(Item, [Item|_ListRem], Pos) ->

    Pos;



index_of_first(Item, [_OtherItem|ListRem], Pos) ->

    index_of_first(Item, ListRem, Pos+1).





%% @since Version 170

rotate_to_first(Item, List) ->

    list_rotate(index_of_first(Item, List)-1, List).





%% @since Version 170

rotate_first_to_end(Item, List) ->

    list_rotate(index_of_first(Item, List), List).
