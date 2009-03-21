




%% @since Version 211

make_node(Name, Host) ->

    list_to_atom(Name ++ "@" ++ Host).





%% @spec standard_listener(Handler, Port, SocketOptions) -> { ok, WorkerPid, ListeningPort } | { error, E }

%% @doc {@section Network} Listens on a socket and manages the fast packet loss problem.
%%
%% There is a defect in the canonical listener, where under extreme load a packet could be delivered before the socket has been traded off to the handler process.  This would mean that the socket could deliver one (or, theoretically, more) packets to the wrong process.  `{active,false}' is immune to this problem, but very inconvenient and in some ways against the erlang mindset.
%%
%% `standard_listener' resolves the default `{active,true}' into the configuration if missing, then if active is not `{active,false}', strips the active status out, handles the socket `{active,false}', then passes to an internal handling function which re-engages the expected active status (erlang sockets when switched to active true or once immediately deliver their backlogs), and passes off to the user specified handler which receives its expected behavior without change.  (Also, this takes some of the repeat grunt work out of making listeners.)
%%
%% The function in Handler should be a 2-ary function which accepts a socket and the list of options the socket used, augmented with the tuple `{from_port,Port}', where `Port' is the listening port from which the connection was accepted.<span style="color:red">TODO: Needs code example</span>
%%
%% {@section Thanks} to MisterN for counsel, noticing several embarrassing bugs, and challenging me to refine my approach from several directions.  Thanks to Steve Vinoski for pointing out that I'd neglected to set the controlling process, that the port closed signal was not being caught, and that ephemeral ports could nicely be supported by opening port 0 then reporting the listening port.

%% @since Version 96

standard_listener(Handler, Port, SocketOptions) ->


    ActiveStatus = case proplists:get_value(active, SocketOptions) of

        undefined ->
            true;

        Other ->
            Other

    end,


    FixedOptions = proplists:delete(active, SocketOptions) ++ [{active, false}],


    case gen_tcp:listen(Port, FixedOptions) of

        { ok, ListeningSocket } ->

            ListeningPort = case Port of

                0 -> 
                    {ok, LP} = inet:port(ListeningSocket),
                    LP;

                _ ->
                    Port

            end,

            { ok, spawn(?MODULE, standard_listener_controller, [Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, 0]), ListeningPort };

        { error, E } ->
            { error, E }

    end.





%% @private

standard_listener_controller(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, AcceptCount) ->

    ListenLoop = spawn_link(?MODULE, standard_listener_accept_loop, [Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, self()]),
    standard_listener_controller(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, AcceptCount, ListenLoop).




%% @private

standard_listener_controller(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, AcceptCount, ListenLoop) ->

    receive

        terminate ->
            gen_tcp:close(ListeningSocket),
            { ok, terminating, { serviced, AcceptCount }};

        serviced ->
            standard_listener_controller(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, AcceptCount+1, ListenLoop);

        { Requester, count_serviced } ->
            Requester ! { count_serviced_response, AcceptCount },
            standard_listener_controller(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, AcceptCount,   ListenLoop);

        { Requester, setopts, Options } ->
            Requester ! { setopts_response, inet:setopts(ListeningSocket, Options) },
            standard_listener_controller(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, AcceptCount,   ListenLoop)

    end.





%% @private

standard_listener_accept_loop(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, Controller) ->

    case gen_tcp:accept(ListeningSocket) of

        { ok, ConnectedSocket } ->
            Controller ! serviced,
            spawn(?MODULE, standard_listener_shunt, [Handler, Port, FixedOptions, ConnectedSocket, ActiveStatus]),
            standard_listener_accept_loop(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, Controller);

        { error, closed } ->
            closed;

        { error, _E } ->
            standard_listener_accept_loop(Handler, Port, FixedOptions, ListeningSocket, ActiveStatus, Controller)

    end.





%% @private

standard_listener_shunt(Handler, Port, FixedOptions, ConnectedSocket, ActiveStatus) ->

    gen_tcp:controlling_process(ConnectedSocket, self()),
    CollectedOptions = proplists:delete(active, FixedOptions) ++ [{active, ActiveStatus}, {from_port, Port}],

    case ActiveStatus of

        false ->
            ok;

        NotFalse ->
            inet:setopts(ConnectedSocket, [{active, NotFalse}])

    end,

    Handler(ConnectedSocket, CollectedOptions).






%start() ->
%
%    case gen_tcp:listen(25,[]) of
%
%        { ok, ListeningSocket } -> { ok, listening_on_pid, spawn(?MODULE, accept_loop, [ListeningSocket]) };
%        { error, E }            -> { error, E }
%
%    end.
%
%
%
%
%
%accept_loop(ListeningSocket) ->
%
%    case gen_tcp:accept(ListeningSocket) of
%
%        { ok, ConnectedSocket } ->
%            spawn(?MODULE, handler_loop, [ConnectedSocket]),
%            accept_loop(ListeningSocket);
%
%        { error, E } ->
%            accept_loop(ListeningSocket)
%
%    end.
%
%
%
%
%
%handler_loop(ConnectedSocket) ->
%
%    receive
%
%        terminate              -> ok;
%        { tcp, Socket, Input } -> gen_tcp:send(Socket, "You said " ++ Input ++ "\r\n"), handler_loop(ConnectedSocket);
%        { error, E }           -> { error, E }
%
%    end.
