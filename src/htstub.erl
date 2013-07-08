
-module(htstub).

-include_lib("eunit/include/eunit.hrl").





-export([

    test/0,

    lib_version/0,
    running_version/1,
    get_boot_options/1,

    listen_on/4,
    stop_listening_on/4,

    start/0,
      start/1,
      start/2,

    stop/1,

    verbose/1,
    quiet/1,

    default_options/0,

    rest/1,

    parse_uri/1,



    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Private exports, do not use

    loop_upgrade/1

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





loop_upgrade(Verbose) ->

    mwrite(Verbose, "upgrading: htstub core loop ~p now version ~p~n~n", [self(), lib_version()]),
    loop(Verbose).





listener_loop(LSock) ->

    receive

        terminate ->
            ok

    end.





new_listener_loop(Address, AddressType, Port) ->

    { ok, LSock } = gen_tcp:listen(Port, [binary, {ip, Address}, {packet, raw}, AddressType]),
    listener_loop(LSock).





spawn_link_new_listener(Address, AddressType, Port) ->

    spawn_link(fun() -> new_listener_loop(Address, AddressType, Port) end).





loop(Verbose) ->

    receive

        { 'EXIT', FromPid, Reason } ->
            mwrite(Verbose, "TRAPPED AN EXIT: htstub core loop ~p trapped from ~p~n  ~p~n~n", [self(), FromPid, Reason]),
            loop(Verbose);

        { ReqPid, get_running_version } ->
            ReqPid ! { now_running, lib_version() },
            loop(Verbose);

        { ReqPid, get_boot_options } ->
            mwrite(Verbose, "listing options: htstub core loop ~p~n~n", [self()]),
            ReqPid ! { boot_options, get(boot_options) },
            loop(Verbose);

        { ReqPid, listen_on, Address, AddressType, Port } ->
            NewPid = spawn_link_new_listener(Address, AddressType, Port),
            mwrite(Verbose, "spawning: htstub new listener ~w for ~w ~w ~w~n~n", [NewPid, Address, AddressType, Port]),
            ReqPid ! { now_listening_on, NewPid },
            put({listening_on, Address, AddressType, Port}, NewPid),
            loop(Verbose);

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
            loop(Verbose);

        terminate ->
            mwrite(Verbose, "terminating: htstub core loop ~p~n~n", [self()]),
            ok;

        upgrade ->
            mwrite(Verbose, "upgrading: htstub core loop ~p from version ~p ~n", [self(), lib_version()]),
            htstub:loop_upgrade(Verbose);

        verbose ->
            mwrite(verbose, "set to verbose: htstub core loop ~p~n~n", [self()]),
            loop(verbose);

        quiet ->
            mwrite(verbose, "set to quiet: htstub core loop ~p received misunderstood message~n  ~p~n~n", [self()]),
            loop(quiet);

        Other ->
            mwrite(Verbose, "warning: htstub core loop ~p received misunderstood message~n  ~p~n~n", [self(), Other]),
            loop(Verbose)

    end.





lib_version() -> "2.0.3".





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

    loop(proplists:get_value(verbose, Options, false)).





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
