
-module(htstub_core).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://htstub.com/").
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

-description("The HtStub application webserver core implementation").

-todo([unit_tests,stochastic_tests,lib_test_rig_integrate,dialyzer,docs,doc_examples,doc_extraction,build_system]).





-export([

    create_server/1,
      create_server/2,

    to_statusline/1,

    package_result/3,

    header_datestring/0,

    parse_getline/1,



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Internal functions, do not use

    construct_server_loop/2,
      construct_server_loop/4,

    handle_socket/3

]).





%% @since Version 390

parse_getline(GetLine) ->

    case string:tokens(GetLine, " ") of
        [Method, Request, "HTTP/" ++ HttpVersion] -> {getline, Method, Request, HttpVersion};
        Other                                     -> {bad_getline, GetLine, Other}
    end.





%% @since Version 390

create_server(Options) ->

    % todo add something like sc_debug:chatter
    create_server(Options, no_supervisor).



%% @since Version 390

create_server(Options, Supervisor) ->

    { ok, spawn(?MODULE, construct_server_loop, [Options, Supervisor]) }.





%% @since Version 390

construct_server_loop(Options, Supervisor) when is_pid(Supervisor) ->

    link(Supervisor),
    construct_server_loop(Options, no_supervisor);



%% @since Version 390

construct_server_loop(Options, no_supervisor) ->

    case [proplists:get_value(X, Options, missing) || X <- [port, ip, timeout, handler]] of

        [missing,_,_,_] ->
            { error, "The listening port {port,Int()} was not set in Options" };

        [_,missing,_,_] ->
            { error, "The listening IP address {ip,{IpType,IpAddress}} was not set in Options" };

        [_,_,missing,_] ->
            { error, "The listening timeout {timeout,Millisec()} was not set in Options" };

        [_,_,_,missing] ->
            { error, "The handling function/pid {handler,FunOrPid()} was not set in Options" };

        [Port, Ip, Timeout, Handler] ->
            construct_server_loop(Port, Ip, Timeout, Handler)

    end.



%% @since Version 390

construct_server_loop(Port, {IpType,IpAddress}, Timeout, Handler) ->

    ListenResult = gen_tcp:listen(Port, [list, IpType, {ip,IpAddress}, {active,false}, {packet,raw}]),

    case ListenResult of

        { ok, ListeningSocket } ->

            server_loop(ListeningSocket, Timeout, Handler);

        { error, E } ->

            { error, { "Could not open a listening socket", E }}

    end.





%% @since Version 390

server_loop(ListeningSocket, Timeout, Handler) ->

    case gen_tcp:accept(ListeningSocket) of

        { ok, ConnectedSocket } ->
            spawn(?MODULE, handle_socket, [ConnectedSocket, Timeout, Handler]),
            server_loop(ListeningSocket, Timeout, Handler);

        { error, closed } ->
            ok;

        { error, _E } ->
            server_loop(ListeningSocket, Timeout, Handler)

    end.





%% @since Version 390

handle_socket(ConnectedSocket, Timeout, Handler) ->

%   inet:setopts(ConnectedSocket, [{active, true}]),

    case block_on_parse_http(ConnectedSocket, Timeout) of

        { ok, {Req, Headers, Args} } ->
            return_result(ConnectedSocket, Handler(Req, Headers, Args)),

            % todo debug this
            % it's not entirely clear why I need to do this, but if i don't, the socket can close before send() pushes its data
            % possibly related to the timeout options i used to set >:(
            receive after 10 -> ok end,

            gen_tcp:close(ConnectedSocket),
            ok;

        { error, _E } ->
            gen_tcp:close(ConnectedSocket),
            ok

    end.





%% @since Version 390

block_on_body(ConnectedSocket, Timeout, Body, Path, Protocol, Method, PHeaders) ->

    FinalBodyLength = list_to_integer(proplists:get_value("Content-Length", PHeaders, "0")),
    block_on_body(ConnectedSocket, Timeout, Body, Path, Protocol, Method, PHeaders, length(Body), FinalBodyLength).





%% @since Version 390

block_on_body(ConnectedSocket, Timeout, Body, Path, Protocol, Method, PHeaders, CurrentLength, BodyLength) when CurrentLength < BodyLength ->

    {ok,NewRecv} = gen_tcp:recv(ConnectedSocket, 0),
    block_on_body(ConnectedSocket, Timeout, Body ++ NewRecv, Path, Protocol, Method, PHeaders, CurrentLength+length(NewRecv), BodyLength);



%% @since Version 390

block_on_body(_ConnectedSocket,_Timeout,_Body,_Path,_Protocol,_Method,_PHeaders, CurrentLength, BodyLength) when CurrentLength > BodyLength ->

    { error, body_length_longer_than_expected };



%% @since Version 390

block_on_body(_ConnectedSocket, _Timeout, Body, Path, Protocol, Method, PHeaders, BodyLength, BodyLength) ->

    block_on_reformat(Body, Path, Protocol, Method, PHeaders, BodyLength).



%% @since Version 390

block_on_reformat(Body, Path, Protocol, Method, PHeaders, BodyLength) ->

    { Site, Port } = case proplists:get_value("Host", PHeaders) of

        undefined -> { "http://127.0.0.1/", 80 };

        Defined ->

            case sc_string:explode(":", Defined) of

                [ ISite, IPort ] ->
                    { ISite, list_to_integer(IPort) };

                [ ISite ] ->
                    { ISite, 80 }

            end
    end,

    "HTTP/" ++ PVer  = Protocol,
    [LMajor, LMinor] = sc_string:explode(".",PVer),  % todo harden

    ["/" ++ Resource, Args] = case sc_string:explode("?", Path) of

        [Rs, ArgL] ->
            [Rs, [ list_to_tuple([ htstub:url_decode(I) || I <- sc_string:explode("=", Arg)]) || Arg <- sc_string:explode("&", ArgL) ]];

        [Rs] ->
            [Rs, []]

    end,

    { ok, { {Site,Port,{http,list_to_integer(LMajor),list_to_integer(LMinor),Method}}, PHeaders, {Resource,Args,{BodyLength,Body}} } }.





%% @since Version 390

block_on_http_headers(ConnectedSocket, Timeout, [], PendingWork, Path, Protocol, Method) ->

    case gen_tcp:recv(ConnectedSocket, 0) of

        { ok, Data } ->
            block_on_http_headers(ConnectedSocket, Timeout, Data, PendingWork, Path, Protocol, Method);

        { error, E } ->
            { error, E }

    end;



%% @since Version 390

block_on_http_headers(ConnectedSocket, Timeout, Rem, PendingWork, Path, Protocol, Method) ->

    Unified = PendingWork ++ Rem,

    case sc_string:explode("\r\n\r\n", Unified, 2) of

        [ Headers, BodyRem ] ->
            ProcessedHeaders = [ list_to_tuple(sc_string:explode(": ",H)) || H <- sc_string:explode("\r\n", Headers) ],
            block_on_body(ConnectedSocket, Timeout, BodyRem, Path, Protocol, Method, ProcessedHeaders);

        [ NotYet ] ->
            block_on_http_headers(ConnectedSocket, Timeout, [], NotYet, Path, Protocol, Method)

    end.





%% @since Version 390

block_on_parse_http(ConnectedSocket, Timeout) ->

    % todo support timeout
    block_on_http_request(ConnectedSocket, Timeout, [], []).



%% @since Version 390

block_on_http_request(ConnectedSocket, Timeout, [], PendingWork) ->

    case gen_tcp:recv(ConnectedSocket, 0) of

        { ok, Data } ->
            block_on_http_request(ConnectedSocket, Timeout, Data, PendingWork);

        { error, E } ->
            { error, E }

    end;



%% @since Version 390

block_on_http_request(ConnectedSocket, Timeout, NewWork, PendingWork) ->

    case sc_string:explode("\r\n", PendingWork ++ NewWork, 2) of

        [ReqLine, Rem] ->

            case sc_string:explode(" ", ReqLine) of

                [ Method, Path, Protocol ] ->
                    block_on_http_headers(ConnectedSocket, Timeout, Rem, [], Path, Protocol, Method);

                Other ->
                    { error, { malformed_request_line, ReqLine, Other }}

            end;

        [NotYet] ->
            block_on_http_request(ConnectedSocket, Timeout, [], NotYet)

    end.





%% @since Version 390

to_statusline(100) -> "100 Continue";
to_statusline(101) -> "101 Switching Protocols";

to_statusline(200) -> "200 OK";
to_statusline(201) -> "201 Created";
to_statusline(202) -> "202 Accepted";
to_statusline(203) -> "203 Non-Authoritative Information";
to_statusline(204) -> "204 No Content";
to_statusline(205) -> "205 Reset Content";
to_statusline(206) -> "206 Partial Content";

to_statusline(300) -> "300 Multiple Choices";
to_statusline(301) -> "301 Moved Permanently";
to_statusline(302) -> "302 Found";
to_statusline(303) -> "303 See Other";
to_statusline(304) -> "304 Not Modified";
to_statusline(305) -> "305 Use Proxy";
to_statusline(306) -> "306 (Unused)";
to_statusline(307) -> "307 Temporary Redirect";

to_statusline(400) -> "400 Bad Request";
to_statusline(401) -> "401 Unauthorized";
to_statusline(402) -> "402 Payment Required";
to_statusline(403) -> "403 Forbidden";
to_statusline(404) -> "404 Not Found";
to_statusline(405) -> "405 Method Not Allowed";
to_statusline(406) -> "406 Not Acceptable";
to_statusline(407) -> "407 Proxy Authentication Required";
to_statusline(408) -> "408 Request Timeout";
to_statusline(409) -> "409 Conflict";
to_statusline(410) -> "410 Gone";
to_statusline(411) -> "411 Length Required";
to_statusline(412) -> "412 Precondition Failed";
to_statusline(413) -> "413 Request Entity Too Large";
to_statusline(414) -> "414 Request-URI Too Long";
to_statusline(415) -> "415 Unsupported Media Type";
to_statusline(416) -> "416 Requested Range Not Satisfiable";
to_statusline(417) -> "417 Expectation Failed";

to_statusline(500) -> "500 Internal Server Error";
to_statusline(501) -> "501 Not Implemented";
to_statusline(502) -> "502 Bad Gateway";
to_statusline(503) -> "503 Service Unavailable";
to_statusline(504) -> "504 Gateway Timeout";
to_statusline(505) -> "505 HTTP Version Not Supported".





%% @since Version 390

return_result(ConnectedSocket, Response) when is_list(Response) ->

    return_result(ConnectedSocket, {200, [], Response});



%% @since Version 390

return_result(ConnectedSocket, {Status, Response}) when is_integer(Status), is_list(Response) ->

    return_result(ConnectedSocket, {Status, [{"Date",header_datestring()},{"Content-Type","text/html"},{"Content-Length",integer_to_list(length(Response))}], Response});



%% @since Version 390

return_result(ConnectedSocket, {Status, Headers, Response}) ->

    gen_tcp:send(ConnectedSocket, package_result(Status, Headers, Response)).





%% @since Version 390

package_result(Status, Headers, Response) ->

    "HTTP/1.1 " ++ to_statusline(Status) ++
    lists:flatten([ "\r\n" ++ Field ++ ": " ++ Value || {Field,Value} <- Headers ]) ++
    "\r\n" ++ "\r\n" ++
    Response.





%% @since Version 390

header_datestring() ->

    { {Y,M,D}, {H,Mn,S} } = erlang:universaltime(),

    MonthLabel = case M of
        1  -> "Jan";
        2  -> "Feb";
        3  -> "Mar";
        4  -> "Apr";
        5  -> "May";
        6  -> "Jun";
        7  -> "Jul";
        8  -> "Aug";
        9  -> "Sep";
        10 -> "Oct";
        11 -> "Nov";
        12 -> "Dec"
    end,

    Day = case calendar:day_of_the_week(Y,M,D) of
        1 -> "Mon";
        2 -> "Tue";
        3 -> "Wed";
        4 -> "Thu";
        5 -> "Fri";
        6 -> "Sat";
        7 -> "Sun"
    end,

    lists:flatten(io_lib:format("~s, ~b ~s ~b ~2.10.0b:~2.10.0b:~2.10.0b GMT",[Day,D,MonthLabel,Y,H,Mn,S])).
