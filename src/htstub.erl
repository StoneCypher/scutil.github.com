
%  reading the source?
%  -------------------
%
%  go to a console, and assuming you put your files
%  in a root level directory called "/wherever" ,
%  which is also where you want your docs, type:
%
%  c("/wherever/sc.erl").
%  c("/wherever/htstub.erl").
%  sc:gen_docs("/wherever").
%
%  bang: you now have readable html documentation in /wherever/doc/erl.





%%%%%%%%%%%
%%
%%  @doc HtStub - web development in Erlang as trivial as it <i>could</i> be
%%
%%  This is the 2013 rewrite of the original library.
%%  
%%  <h2>Quick Start</h2>
%%
%%  <dl>
%%    <dt>Step One - Compile the util lib and server lib</dt>
%%    <dd>Assuming "wherever" is the directory in which you've placed the source:<pre>
%%1> c("/wherever/sc.erl").
%%{ok,sc}<br/>
%%2> c("/wherever/htstub.erl").
%%{ok,htstub}
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Two - Good to go, try it out</dt>
%%    <dd><pre>
%%3> Handle = htstub:serve().
%%&lt;0.50.0>
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Three - Aaaaand test</dt>
%%    <dd>Pull up your web browser and go to <a href="http://127.0.0.1/" target="_blank">127.0.0.1</a>.</dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Four - One with your code?</dt>
%%    <dd><pre>
%%3> htstub:stop(Handle).
%%terminate<br/>
%%4> MyServer = fun(_) -> "&lt;!doctype html>&lt;html>Y helo thar&lt;/html>" end.
%%#Fun&lt;erl_eval.6.17052888><br/>
%%5> htstub:serve(MyServer).
%%&lt;0.76.0>
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Five - Aaaaand test</dt>
%%    <dd>Pull up your web browser and go to <a href="http://127.0.0.1/" target="_blank">127.0.0.1</a>.  ... again.</dd>
%%  </dl>





%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2013 - current John Haugeland, All Rights Reserved
%% @since September July 5, 2013

%% @todo distinguish -opaque types from -types (opaque are not meant to be understood by the outside world, merely tracked, eg handles)

%% @todo module attributes and documentation attributes for license name, license url
%% @todo Every documentation example should be enforced as a unit test, to keep the docs up to date
%% @todo Automate the version back into the docs
%% @todo Automate the version into the .app.src





-module(htstub).

-include_lib("eunit/include/eunit.hrl").





-export([

    test/0,

    lib_version/0,
    running_version/1,
    get_boot_options/1,

    listen_on/4,
    stop_listening_on/4,

    serve/0,
      serve/1,

    start/0,
      start/1,

    stop/1,

    verbose/1,
    quiet/1,

    int_to_status/1,

    parse_uri/1,

    rest/1,

    standard_datestring/0,
    config_from_plist/1,
    default_handler/1,



    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Private exports, do not use

    loop_upgrade/2

]).





-include("htstub.hrl").





% http://blog.lunatech.com/2009/02/03/what-every-web-developer-must-know-about-url-encoding
% http://doriantaylor.com/policy/http-url-path-parameter-syntax

% comeback todo whargarbl this is naaaaaasty

parse_uri(Uri) when is_binary(Uri) ->

    parse_uri(binary_to_list(Uri));

%    [[ list_to_binary(Chunks) || Chunks <- parse_uri(binary_to_list(Uri)) ]];





parse_uri(Uri) ->

    { ok, {Scheme, UserInfo, Host, Port, Path, Query} } = http_uri:parse(Uri),



    % first let's fix the username and password up

    [Username, Password] = case sc:explode(":", UserInfo, 2) of

        [[]]    -> [undefined,         undefined];
        [U]     -> [list_to_binary(U), undefined];
        [[],[]] -> [undefined,         undefined];
        [ U,[]] -> [list_to_binary(U), undefined];
        [[], P] -> [undefined,         list_to_binary(P)];
        [ U, P] -> [list_to_binary(U), list_to_binary(P)]

    end,



    PPP = fun(PP) -> % parse path params

        [
            case sc:explode("=",P,2) of
                [Key] -> 
                    {Key};
                [Key,RawVal] ->
                    {Key,sc:explode(",",RawVal)}
            end
        ||
            P <- PP
        ]

    end,

    [FixedPath, Params] = case sc:explode(";", Path) of
        [[]]  -> [ "", []     ];     
        [T]   -> [  T, []     ];                  % first unique rule is T for paTh, R for paRams
        [T|R] -> [  T, PPP(R) ]
    end,



    FixQueries = fun(QueryFront) ->
        io:format("QueryFront: ~p~n", [QueryFront]),
        { Single, Multi } = lists:partition(
            fun({_X}) -> true; (_) -> false end,
            [ list_to_tuple(sc:explode("=",Param)) || Param <- sc:explode(";", QueryFront), Param =/= [] ]
        ),
        Single ++ sc:key_bucket(Multi)
    end,

    NoQmQuery = case Query of "" -> ""; Q -> "?" ++ NQ = Q, NQ end,

    [QFront, Fragment] = case sc:explode("#", NoQmQuery, 2) of

        [] ->
            [[],      undefined];

        [IQFront] ->
            [IQFront, undefined];

        [IQFront, IFragment] ->
            [IQFront, IFragment]

    end,

    QueryTerms = FixQueries(QFront),




    % todo fixme comeback ugh whargarbl

    BinQT = [
        case QT of
            {Str}      -> {list_to_binary(Str)};
            {Str,List} -> {list_to_binary(Str), [list_to_binary(L) || L <- List]}
        end
    ||
        QT <- QueryTerms
    ],




    OurParse      = #htstub_uri{ 
                        scheme       = Scheme, 
                        user         = Username, 
                        password     = Password, 
                        host         = list_to_binary(Host),
                        port         = Port, 
                        path         = list_to_binary(FixedPath),
                        path_params  = Params,
                        query_params = BinQT,
                        fragment     = Fragment
                     },

    OurParse.





mwrite(false,  _Msg, _Args) ->
    ok;

mwrite(quiet,  _Msg, _Args) ->
    ok;

mwrite(verbose, Msg, Args) ->
    io:format(Msg, Args),
    ok;

mwrite(true,    Msg, Args) ->
    mwrite(verbose, Msg, Args).





return_result(ConnectedSocket, Response) when is_list(Response) ->

    return_result(ConnectedSocket, {200, Response});





return_result(ConnectedSocket, {Status, Response}) when is_integer(Status), is_list(Response) ->

    return_result(ConnectedSocket, {Status, [{"Date",standard_datestring()},{"Content-Type","text/html"},{"Content-Length",integer_to_list(length(Response))}], Response});





return_result(ConnectedSocket, {Status, Headers, Response}) ->

    gen_tcp:send(ConnectedSocket, package_result(Status, Headers, Response)).





package_result(Status, Headers, Response) ->

    "HTTP/1.1 " ++ int_to_status(Status) ++
    lists:flatten([ "\r\n" ++ Field ++ ": " ++ Value || {Field,Value} <- Headers ]) ++
    "\r\n" ++ "\r\n" ++
    Response.





loop_upgrade(Verbose, Handler) ->

    mwrite(Verbose, "upgrading: htstub core loop ~p now version ~p~n~n", [self(), lib_version()]),
    loop(Verbose, Handler).





block_on_parse_http(ConnectedSocket) ->

    { Method, Path, Protocol, Rem } = block_on_http_request(ConnectedSocket, [], []),
    { BodyRem, ProcessedHeaders }   = block_on_http_headers(ConnectedSocket, Rem, [], Path, Protocol, Method),
    parse_body(ConnectedSocket, BodyRem, Path, Protocol, Method, ProcessedHeaders).





parse_body(ConnectedSocket, Body, Path, Protocol, Method, PHeaders) ->

    FinalBodyLength = list_to_integer(proplists:get_value("Content-Length", PHeaders, "0")),
    parse_body(ConnectedSocket, Body, Path, Protocol, Method, PHeaders, size(Body), FinalBodyLength).





parse_body(ConnectedSocket, Body, Path, Protocol, Method, PHeaders, CurrentLength, BodyLength) when CurrentLength < BodyLength ->

    {ok,NewRecv} = gen_tcp:recv(ConnectedSocket, 0),
    parse_body(ConnectedSocket, Body ++ NewRecv, Path, Protocol, Method, PHeaders, CurrentLength+length(NewRecv), BodyLength);





parse_body(_ConnectedSocket,_Body,_Path,_Protocol,_Method,_PHeaders, CurrentLength, BodyLength) when CurrentLength > BodyLength ->

    { error, body_length_longer_than_expected };





parse_body(_ConnectedSocket, Body, Path, Protocol, Method, PHeaders, BodyLength, BodyLength) ->

    body_reformat(Body, Path, Protocol, Method, PHeaders, BodyLength).





body_reformat(Body, Path, Protocol, Method, PHeaders, BodyLength) ->

%   io:format("~nxxxxxxxxxxxxxxxxx~nbody_reformat~n  ~p~nxxxxxxxxxxxxxxxxx", [PHeaders]),
%   io:format("~n------------------------~nbody_reformat~n  ~p~n------------------------", [[Body, Path, Protocol, Method, PHeaders, BodyLength]]),

    { Site, Port } = case proplists:get_value("Host", PHeaders) of

        undefined -> { <<"127.0.0.1">>, 80 };

        Defined ->

            case sc:explode(<<":">>, Defined) of

                [ ISite, IPort ] ->
                    { ISite, IPort };

                [ ISite ] ->
                    { ISite, "" }

            end
    end,

    <<"HTTP/", PVer/binary>> = Protocol,

    [LMajor,  LMinor] = sc:explode(<<".">>, PVer),  % todo harden

%   io:format("LM: ~p, lm: ~p~nSite: ~p, Port: ~p, Path ~p~n~n", [LMajor, LMinor, Site, Port, Path]),

    PPath = << 
               <<"http://">>/binary, 
               Site/binary, 
               (if Port == <<"">> -> <<"">>; true -> << <<":">>/binary, (integer_to_binary(Port))/binary>> end)/binary, 
               Path/binary
            >>,

%   io:format("PPath: ~p~n~n", [PPath]),

    { ok, #htstub_request{ 
            request=PPath, 
            http_ver={binary_to_integer(LMajor),binary_to_integer(LMinor)},
            parsed=parse_uri(PPath), 
            method=Method, 
            pheaders=PHeaders, 
            body=Body, 
            body_length=BodyLength 
          } 
    }.

    % todo this shouldn't just assume http; it could be https, spdy, etc
%   { ok, { {Site,Port,{http,list_to_integer(LMajor),list_to_integer(LMinor),Method}}, PHeaders, {Resource,Args,{BodyLength,Body}} } }.





block_on_http_headers(ConnectedSocket, [], PendingWork, Path, Protocol, Method) ->

    case gen_tcp:recv(ConnectedSocket, 0) of

        { ok, Data } ->
            block_on_http_headers(ConnectedSocket, Data, PendingWork, Path, Protocol, Method);

        { error, E } ->
            { error, E }

    end;





block_on_http_headers(ConnectedSocket, Rem, PendingWork, Path, Protocol, Method) ->

    Unified = PendingWork ++ Rem,

    case sc:explode(<<"\r\n\r\n">>, Unified, 2) of

        [ Headers, BodyRem ] ->
            ProcessedHeaders = [ list_to_tuple(sc:explode(<<": ">>,H)) || H <- sc:explode(<<"\r\n">>, Headers) ],
            { BodyRem, ProcessedHeaders };

        [ NotYet ] ->
            block_on_http_headers(ConnectedSocket, [], NotYet, Path, Protocol, Method)

    end.






block_on_http_request(ConnectedSocket, [], PendingWork) ->

    case gen_tcp:recv(ConnectedSocket, 0) of

        { ok, Data } ->
            block_on_http_request(ConnectedSocket, Data, PendingWork);

        { error, E } ->
            { error, E }

    end;





block_on_http_request(ConnectedSocket, NewWork, PendingWork) ->

%   io:format("~p~n~p~n~n",[NewWork,PendingWork]),

    case sc:explode(<<"\r\n">>, PendingWork ++ NewWork, 2) of

        [ReqLine, Rem] ->

            case sc:explode(<<" ">>, ReqLine) of

                [ Method, Path, Protocol ] ->
                    { Method, Path, Protocol, Rem };

                Other ->
                    { error, { malformed_request_line, ReqLine, Other }}

            end;

        [NotYet] ->
            block_on_http_request(ConnectedSocket, [], NotYet)

    end.





handle_new_socket(ConnectedSocket, Handler) ->

    case block_on_parse_http(ConnectedSocket) of

        { ok, Parsed } ->
            return_result(ConnectedSocket, Handler(Parsed)),

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





server_listener_loop(ListeningSocket, Handler) ->

    case gen_tcp:accept(ListeningSocket) of

        { ok, ConnectedSocket } ->
            spawn(fun() -> handle_new_socket(ConnectedSocket, Handler) end),
            server_listener_loop(ListeningSocket, Handler);

        { error, closed } ->
            ok;

        { error, _E } ->
            server_listener_loop(ListeningSocket, Handler)

    end.





new_listener_loop(Address, AddressType, Port, Handler) ->

    { ok, LSock } = gen_tcp:listen(Port, [binary, {active, false}, {ip, Address}, {packet, raw}, AddressType]),
    server_listener_loop(LSock, Handler).





spawn_link_new_listener(Address, AddressType, Port, Handler) ->

    spawn_link(fun() -> new_listener_loop(Address, AddressType, Port, Handler) end).





loop(Verbose, Handler) ->

    receive

        { 'EXIT', FromPid, Reason } ->
            mwrite(Verbose, "TRAPPED AN EXIT: htstub core loop ~p trapped from ~p~n  ~p~n~n", [self(), FromPid, Reason]),
            loop(Verbose, Handler);

        { now_listening_on, _NewPid } ->
            % from auto-listen; discard
            loop(Verbose, Handler);

        { ReqPid, get_running_version } ->
            ReqPid ! { now_running, lib_version() },
            loop(Verbose, Handler);

        { ReqPid, get_boot_options } ->
            mwrite(Verbose, "listing options: htstub core loop ~p~n~n", [self()]),
            ReqPid ! { boot_options, get(boot_options) },
            loop(Verbose, Handler);

        { ReqPid, listen_on, Address, AddressType, Port } ->
            NewPid = spawn_link_new_listener(Address, AddressType, Port, Handler),
            mwrite(Verbose, "spawning: htstub new listener ~w for ~w ~w ~w~n~n", [NewPid, Address, AddressType, Port]),
            ReqPid ! { now_listening_on, NewPid },
            put({listening_on, Address, AddressType, Port}, NewPid),
            loop(Verbose, Handler);

        { ReqPid, stop_listening_on, Address, AddressType, Port } ->
            case get({listening_on, Address, AddressType, Port}) of

                undefined ->
                    ReqPid ! { no_such_stop_listen, Address, AddressType, Port },
                    mwrite(Verbose, "!!! error: htstub requested halt listener ~w ~w ~w; no such listener~n~n", [Address, AddressType, Port]);

                Pid when is_pid(Pid) ->
                    ReqPid ! { stopped_listening_on, Address, AddressType, Port },
                    mwrite(Verbose, "stopping: htstub halt listener ~w for ~w ~w ~w", [Pid, Address, AddressType, Port]),
                    Pid ! terminate

            end,
            loop(Verbose, Handler);

        terminate ->
            mwrite(Verbose, "terminating: htstub core loop ~p~n~n", [self()]),
            ok;

        upgrade ->
            mwrite(Verbose, "upgrading: htstub core loop ~p from version ~p ~n", [self(), lib_version()]),
            htstub:loop_upgrade(Verbose, Handler);

        verbose ->
            mwrite(verbose, "set to verbose: htstub core loop ~p~n~n", [self()]),
            loop(verbose, Handler);

        quiet ->
            mwrite(verbose, "set to quiet: htstub core loop ~p received misunderstood message~n  ~p~n~n", [self()]),
            loop(quiet, Handler);

        Other ->
            mwrite(Verbose, "warning: htstub core loop ~p received misunderstood message~n  ~p~n~n", [self(), Other]),
            loop(Verbose, Handler)

    end.





lib_version() -> 6.





running_version(ServerPid) ->

    ServerPid ! { self(), get_running_version },
    receive
    
        { now_running, V } ->
            V
    
        after 1000 ->
            timeout
    
    end.





get_boot_options(ServerPid) ->

    ServerPid ! { self(), get_boot_options },
    receive
    
        { boot_options, B } ->
            B
    
        after 1000 ->
            timeout
    
    end.





default_handler(Request) ->

    lists:flatten(io_lib:format("<!doctype html><html><head><style type=\"text/css\">p{margin:0;padding:0;}p+p{margin-top:1em;}body{padding:1em;margin:0;font-size:150%;font-family:helvetica,arial,sans-serif;background-color:#cdf;color:#060;}.sig{position:fixed;bottom:1em;right:1em;color:#333;}</style></head><body><p>This is a default page.</p><p>Your webserver is working.</p><div class=\"sig\">Served by <a href=\"http://htstub.com/\">htstub</a>, a micro-webserver for <a href=\"http://erlang.org/\">Erlang</a> made by <a href=\"http://fullof.bs/\">John Haugeland</a>.</div><pre>~n~p~n</pre></body></html>", [Request])).





%% @doc Serve is just a synonym for start.
serve()    -> start().

%% @doc Serve is just a synonym for start.
serve(X)   -> start(X).





start() -> 

    start(fun default_handler/1).





start(Handler) when is_function(Handler) -> 

    start(#htstub_config{handler=Handler});





start(PropListOptions) when is_list(PropListOptions) -> 

    start(config_from_plist(PropListOptions));





start(Options) -> 

    spawn(fun() -> bootstrap_loop(Options) end).





stop(StubPid) ->

    StubPid ! terminate.





bootstrap_loop(Options) ->

    Verbose = Options#htstub_config.verbose,
    mwrite(Verbose, "\\ Entering htstub bootstrap loop as ~w~n", [self()]),

    if Options#htstub_config.start_immediate == true -> 
        mwrite(Verbose, " - Starting immediate~n", []),
        listen_on(self(), Options#htstub_config.ip, Options#htstub_config.addrtype, Options#htstub_config.port) 
    end,

    Handler = Options#htstub_config.handler,
    mwrite(Verbose, " - Setting handler to ~w~n", [Handler]),

    process_flag(trap_exit, true),
    put(boot_options, Options),

    mwrite(Verbose, " - Bootstrapped! Loop begins.~n~n", []),
    loop(Verbose, Handler).
%   loop(proplists:get_value(verbose, Options, false)).





verbose(StubPid) ->

    StubPid ! verbose,
    ok.





quiet(StubPid) ->

    StubPid ! quiet,
    ok.





listen_on(StubPid, Address, AddressType, Port) ->

    StubPid ! { self(), listen_on, Address, AddressType, Port },
    receive
        { now_listening_on, NewPid } ->
            { now_listening_on, NewPid }
    after 1000 ->
        timeout
    end.





%% @doc (not tested) . ``` '''
%%
%% @since 2.0.3

-spec stop_listening_on(_StubPid, Address, AddressType, Port) -> 
    stopped_listening                                   |
    { no_such_stop_listen, Address, AddressType, Port } | 
    timeout.

stop_listening_on(StubPid, Address, AddressType, Port) ->

    StubPid ! { self(), stop_listening_on, Address, AddressType, Port },
    receive

        { no_such_stop_listen, Address, AddressType, Port } ->
            { no_such_stop_listen, Address, AddressType, Port };

        { stopped_listening_on, Address, AddressType, Port } ->
            stopped_listening

    after 1000 ->
        timeout
    end.





rest(RestishHandler) -> 

    fun(Result) -> RestishHandler(Result#htstub_request.method, Result#htstub_request.parsed#htstub_uri.path, Result) end.





int_to_status(100) -> "100 Continue";
int_to_status(101) -> "101 Switching Protocols";

int_to_status(200) -> "200 OK";
int_to_status(201) -> "201 Created";
int_to_status(202) -> "202 Accepted";
int_to_status(203) -> "203 Non-Authoritative Information";
int_to_status(204) -> "204 No Content";
int_to_status(205) -> "205 Reset Content";
int_to_status(206) -> "206 Partial Content";

int_to_status(300) -> "300 Multiple Choices";
int_to_status(301) -> "301 Moved Permanently";
int_to_status(302) -> "302 Found";
int_to_status(303) -> "303 See Other";
int_to_status(304) -> "304 Not Modified";
int_to_status(305) -> "305 Use Proxy";
int_to_status(306) -> "306 (Unused)";
int_to_status(307) -> "307 Temporary Redirect";

int_to_status(400) -> "400 Bad Request";
int_to_status(401) -> "401 Unauthorized";
int_to_status(402) -> "402 Payment Required";
int_to_status(403) -> "403 Forbidden";
int_to_status(404) -> "404 Not Found";
int_to_status(405) -> "405 Method Not Allowed";
int_to_status(406) -> "406 Not Acceptable";
int_to_status(407) -> "407 Proxy Authentication Required";
int_to_status(408) -> "408 Request Timeout";
int_to_status(409) -> "409 Conflict";
int_to_status(410) -> "410 Gone";
int_to_status(411) -> "411 Length Required";
int_to_status(412) -> "412 Precondition Failed";
int_to_status(413) -> "413 Request Entity Too Large";
int_to_status(414) -> "414 Request-URI Too Long";
int_to_status(415) -> "415 Unsupported Media Type";
int_to_status(416) -> "416 Requested Range Not Satisfiable";
int_to_status(417) -> "417 Expectation Failed";

int_to_status(500) -> "500 Internal Server Error";
int_to_status(501) -> "501 Not Implemented";
int_to_status(502) -> "502 Bad Gateway";
int_to_status(503) -> "503 Service Unavailable";
int_to_status(504) -> "504 Gateway Timeout";
int_to_status(505) -> "505 HTTP Version Not Supported".





standard_datestring() ->

    { {Y,M,D}, {H,Mn,S} } = erlang:universaltime(),

    MonthLabel = element(M, {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
    Day = element(calendar:day_of_the_week(Y,M,D), {"Mon","Tue","Wed","Thu","Fri","Sat","Sun"}),

    lists:flatten(io_lib:format("~s, ~b ~s ~b ~2.10.0b:~2.10.0b:~2.10.0b GMT",[Day,D,MonthLabel,Y,H,Mn,S])).





config_from_plist(PList) ->

    config_from_plist(#htstub_config{}, PList).





config_from_plist(Config, []) ->

    Config;





config_from_plist(Config, PList) ->

    [ PItem | PRem ] = PList,

    case PItem of

        { start_immediate, NSI } ->
            config_from_plist(Config#htstub_config{ start_immediate=NSI }, PRem);

        { ip, NIP } ->
            config_from_plist(Config#htstub_config{ ip=NIP }, PRem);

        { addrtype, NAT } ->
            config_from_plist(Config#htstub_config{ addrtype=NAT }, PRem);

        { port, NPT } ->
            config_from_plist(Config#htstub_config{ port=NPT }, PRem);

        quiet ->
            config_from_plist(Config#htstub_config{ verbose=quiet }, PRem);

        verbose ->
            config_from_plist(Config#htstub_config{ verbose=verbose }, PRem);

        { verbose, NVB } ->
            config_from_plist(Config#htstub_config{ verbose=NVB }, PRem);

        { server_name, NSN } ->
            config_from_plist(Config#htstub_config{ server_name=NSN }, PRem);

        { handler, NHN } ->
            config_from_plist(Config#htstub_config{ handler=NHN }, PRem);

        { middleware, NMW } ->
            config_from_plist(Config#htstub_config{ middleware=NMW }, PRem);

        Other ->
            { error, { unrecognized_config, Other }}

    end.





%% @doc (not testworthy) Runs the test suite in terse form. ``` c("wherever/htstub.erl").
%% {ok,htstub}
%%
%% 2> c("wherever/htstub_tests.erl").
%% {ok,htstub_tests}
%%
%% 3> htstub:test().
%%   All 9 tests passed.
%% ok'''
%%
%% @since 2.0.3

-spec test() -> 
    ok   | 
    error.

test() ->

    eunit:test(htstub).
