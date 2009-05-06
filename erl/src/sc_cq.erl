
-module(sc_cq).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=7").
-publicforum("http://crunchyd.com/forum/scutil-discussion/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Efficient circular queue.").

-testerl_export( { [], sc_cq_testsuite } ).
-library_requirements( [] ).





-export( [

    create/1,
      create/2,

    write/2,

    read/1,

    peek/1,
      peek/2,

    destroy/1,

    worker_process_of/1,

    is_destroyed/1,

%%%%%%%%%%%%%

    cq_loop/4

] ).





cq_loop(Position, FinalPos, Data, Len) ->

    receive

        { Sender, read } ->
            { error, not_yet_implemented };

        { Sender, write, Value } ->
            { error, not_yet_implemented };

        { Sender, peek } ->
            Sender ! { cq_value, element(Position, Data) },
            cq_loop(Position, FinalPos, Data, Len);

        { Sender, peek, At } ->
            { error, not_yet_implemented };

        { Sender, terminate } ->
            Sender ! { cq_ok, cq_terminating },
            ok

    end.





create(Size) ->

    create(Size, list_to_tuple( lists:duplicate(Size,0) )).





create(Size, InitialValues) when is_tuple(InitialValues), size(InitialValues) == Size ->

    {sc_cq, spawn(?MODULE, cq_loop, [1, 0, InitialValues, Size] )};





% create(Size, InitialValues) when is_tuple(InitialValues), size(InitialValues) < Size ->

%     list_to_tuple( [sc_cq, 1, list_to_tuple( lists:duplicate(Size,0) )] );





create(_Size, InitialValues) when is_tuple(InitialValues) ->

    { error, "Initial values supplied are too many for the queue size specified." }.





% create(Size, InitialValues) when is_list(InitialValues)





peek({sc_cq,Pid}) ->

    case is_process_alive(Pid) of

        true ->

            Pid ! { self(), peek },
            receive

                { cq_value, V } ->
                    { value, V };

                { error, E } ->
                    { error, E }

            end;

        false ->

            { error, "Process is dead." }

    end.





peek(At, {sc_cq,Pid}) ->


    case is_process_alive(Pid) of

        true ->

            Pid ! { self(), peek, At },
            receive

                { cq_value, V } ->
                    { value, V };

                { error, E } ->
                    { error, E }

            end;

        false ->

            { error, "Process is dead." }

    end.





write(Value, {sc_cq,Pid}) ->


    case is_process_alive(Pid) of

        true ->

            Pid ! { self(), write, Value },
            receive

                cq_ok ->
                    ok;

                { error, E } ->
                    { error, E }

            end;

        false ->

            { error, "Process is dead." }

    end.





read({sc_cq,Pid}) ->


    case is_process_alive(Pid) of

        true ->

            Pid ! { self(), read },
            receive

                { cq_value, V } ->
                    { value, V };

                { error, E } ->
                    { error, E }

            end;

        false ->

            { error, "Process is dead." }

    end.





destroy({sc_cq,Pid}) ->

    case is_process_alive(Pid) of

        true ->

            Pid ! { self(), terminate },
            receive

                { cq_ok, cq_terminating } ->
                    ok;

                { error, E } ->
                    { error, E }

            end;

        false ->

            { error, "Process is dead" }

    end.





is_destroyed({sc_cq,Pid}) ->

    case is_process_alive(Pid) of

        true ->
            false;

        false ->
            true

    end.





worker_process_of({sc_cq, Pid}) ->

    Pid.
