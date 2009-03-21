
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
