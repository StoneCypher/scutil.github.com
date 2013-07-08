
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
%%{ok,sc}
%%2> c("/wherever/htstub.erl").
%%{ok,htstub}
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Two - Good to go, try it out</dt>
%%    <dd>Assuming you aren't already running a webserver on this machine:<pre>
%%3> htstub:serve(fun(_) -> "&lt;!doctype html>&lt;html>&lt;body>&lt;p>Hello, world from htstub!&lt;/p>&lt;/body>&lt;/html>" end).
%%&lt;0.2050.0>
%%    </pre></dd>
%%  </dl>
%%
%%  <dl>
%%    <dt>Step Three - Aaaaand test</dt>
%%    <dd>Pull up your web browser and go to <a href="http://127.0.0.1/" target="_blank">127.0.0.1</a>.</dd>
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
      serve/2,

    start/0,
      start/1,
      start/2,

    stop/1,

    verbose/1,
    quiet/1,

    default_options/0,

    int_to_status/1,

    rest/1,

    parse_uri/1,

    standard_datestring/0,



    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Private exports, do not use

    loop_upgrade/2

]).





-include("htstub.hrl").





% http://blog.lunatech.com/2009/02/03/what-every-web-developer-must-know-about-url-encoding
% http://doriantaylor.com/policy/http-url-path-parameter-syntax

parse_uri(Uri) ->

    { ok, {Scheme, UserInfo, Host, Port, Path, Query} } = http_uri:parse(Uri),



    % first let's fix the username and password up

    [Username, Password] = case sc:explode(":", UserInfo, 2) of

        [[]]    -> [undefined, undefined];
        [U]     -> [        U, undefined];
        [[],[]] -> [undefined, undefined];
        [ U,[]] -> [        U, undefined];
        [[], P] -> [undefined,         P];
        [ U, P] -> [        U,         P]

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
        { Single, Multi } = lists:partition(
            fun({X}) -> true; (_) -> false end,
            [ list_to_tuple(sc:explode("=",Param)) || Param <- sc:explode(";", QueryFront) ]
        ),
        Single ++ sc:key_bucket(Multi)
    end,

    "?" ++ NoQmQuery   = Query,
    [QFront, Fragment] = sc:explode("#", NoQmQuery, 2),
    QueryTerms         = FixQueries(QFront),

    OurParse      = #htstub_uri{ 
                        scheme       = Scheme, 
                        user         = Username, 
                        password     = Password, 
                        host         = Host, 
                        port         = Port, 
                        path         = FixedPath, 
                        path_params  = Params,
                        query_params = QueryTerms,
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





loop_upgrade(Verbose, Handler) ->

    mwrite(Verbose, "upgrading: htstub core loop ~p now version ~p~n~n", [self(), lib_version()]),
    loop(Verbose, Handler).





server_listener_loop(ListeningSocket, Handler) ->

    case gen_tcp:accept(ListeningSocket) of

        { ok, ConnectedSocket } ->
            spawn(?MODULE, handle_socket, [ConnectedSocket, Handler]),
            server_listener_loop(ListeningSocket, Handler);

        { error, closed } ->
            ok;

        { error, _E } ->
            server_listener_loop(ListeningSocket, Handler)

    end.





new_listener_loop(Address, AddressType, Port, Handler) ->

    { ok, LSock } = gen_tcp:listen(Port, [binary, {ip, Address}, {packet, raw}, AddressType]),
    server_listener_loop(LSock, Handler).





spawn_link_new_listener(Address, AddressType, Port, Handler) ->

    spawn_link(fun() -> new_listener_loop(Address, AddressType, Port, Handler) end).





loop(Verbose, Handler) ->

    receive

        { 'EXIT', FromPid, Reason } ->
            mwrite(Verbose, "TRAPPED AN EXIT: htstub core loop ~p trapped from ~p~n  ~p~n~n", [self(), FromPid, Reason]),
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





lib_version() -> 5.





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




default_options() ->

    [ { start_immediate, true            },
      { port,            80              }, 
      { ip,              {0,0,0,0}       },
      { addrtype,        inet            },   % inet for ipv4, inet6 for ipv6 with 4 fallback 
      { server_name,     "htstub server" },
      { verbose,         quiet           }
    ].





default_handler(_Request) ->

    "<!doctype html><html><head></head><body><p>This is <a href=\"http://htstub.com/\">htstub</a>, a micro-webserver for <a href=\"http://erlang.org/\">Erlang</a>.</p></body></html>".




%% @doc Serve is just a synonym for start.
serve()    -> start().

%% @doc Serve is just a synonym for start.
serve(X)   -> start(X).

%% @doc Serve is just a synonym for start.
serve(X,Y) -> start(X,Y).





start() -> 

    start(fun default_handler/1, default_options()).





start(Handler) -> 

    start(Handler, default_options()).





start(Handler, Options) -> 

    spawn(fun() -> bootstrap_loop(Options) end).





stop(StubPid) ->

    StubPid ! terminate.





bootstrap_loop(Options) ->

    process_flag(trap_exit, true),
    put(boot_options, Options),

    loop(Options#htstub_config.verbose, Options#htstub_config.handler).
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





rest(_) -> 

    todo.





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
