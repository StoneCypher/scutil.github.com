




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
