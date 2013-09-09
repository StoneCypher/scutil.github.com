
%% @doc This is a REST get/set toy implementation.  This should not be used in
%% production: it makes no attempt to protect against XSS/XSLT, render 
%% injections, storage overflow attacks, url faking, et cetera.
%%
%% But this shows a trivial implementation of a useful-ish REST server.

% todo add to doc generation





-module(rest_getset).





-export([

    rest_server/3

]).





rest_response(X) ->

% todo this is where we put in the text/javascript header, etc
% tired though

    { 200, [{"Content-Type","application/json"}], X }.





rest_server(Method, Root, _) 

    when Root == <<"">>;
         Root == <<"/">>;
         Root == <<"/index">>;
         Root == <<"/index.htm">>;
         Root == <<"/index.html">>
    ->

    case Method of

        get ->
            <<"todo UI here">>;

        _Other -> 
            { 405, [{"Allow","GET"}], <<"Not supported">> }

    end;





rest_server(get, Path, _) -> 

    case get(Path) of

        { _Time, Data } ->
            { 200, [{"Cache-Control","no-cache"}], << Data / binary >> };

        undefined ->
            rest_response(<<"No such record">>)       % todo should have a fail header code

    end;





rest_server(put, Path, Args) -> 

    put(Path, { sc:unixtime(), << <<"todo get put data from Args">>/binary, (list_to_binary(lists:flatten(io_lib:format("~w",[Args]))))/binary>> }),
    rest_response(<<"put">>);





rest_server(delete, Path, _) -> 

    erase(Path),
    rest_response(<<"deleted">>);





rest_server(_, _, _) -> 
    
    { 405, [{"Allow","GET,POST,PUT"}], <<"Not supported">> }.
