
% build persistence on note, which in turn can be dets or mnesia




%% @spec counter(Name::any()) -> number()

%% @doc {@section Counters} Checks a counter's value; if the counter was not already defined, it will report zero. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:inc_counter(hello).
%% 1
%%
%% 3> scutil:inc_counter(hello).
%% 2
%%
%% 4> scutil:inc_counter(hello).
%% 3
%%
%% 5> scutil:counter(hello).
%% 3
%%
%% 6> scutil:reset_counter(hello).
%% 0
%%
%% 7> scutil:counter(hello).
%% 0'''

%% @since Version 54

counter(Name) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), get_counter, Name},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @spec counters(Names::list()) -> list_of_integers()

%% @doc {@section Counters} Checks a counter's value; if the counter was not already defined, it will report zero. ```1> scutil:counter(hello).
%% 0'''

%% @since Version 138

counters(Names) -> 

    [ counter(X) || X <- Names ].





%% @equiv adjust_counter(Name,1)

inc_counter(Name) ->

    adjust_counter(Name, 1).



%% @equiv adjust_counter(Name,By)

inc_counter(Name,By) ->

    adjust_counter(Name, By).



%% @equiv adjust_counter(Name,-1)

dec_counter(Name) ->

    adjust_counter(Name, -1).



%% @equiv adjust_counter(Name,-1*By)

dec_counter(Name,By) ->

    adjust_counter(Name, -1*By).





%% @spec adjust_counter(Name::any(), By::number()) -> number()

%% @doc {@section Counters} Adds to a counter's value; if the counter was not already defined, it will become the value in the `By' argument. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:inc_counter(hello).
%% 1
%%
%% 3> scutil:adjust_counter(hello, 3).
%% 4'''

%% @since Version 54

adjust_counter(Name, By) when is_number(By) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), adjust_counter, Name, By},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @equiv set_counter(Name, 0)

%% @since Version 54

reset_counter(Name) -> 

    set_counter(Name, 0).





%% @spec set_counter(Name::any(), To::number()) -> 0

%% @doc {@section Counters} Sets a counter's value to a specific value. ```1> scutil:counter(hello).
%% 0
%%
%% 2> scutil:set_counter(hello,4).
%% 4
%%
%% 3> scutil:counter(hello).
%% 4
%%
%% 4> scutil:reset_counter(hello).
%% 0
%%
%% 5> scutil:counter(hello).
%% 0'''

%% @since Version 54

set_counter(Name, To) when is_number(To) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), set_counter, Name, To},

    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





%% @private

counter_process() ->
   
    receive
   

        shutdown ->
            ok;


        {Caller, get_counter, Name} ->

            case get(Name) of

                undefined ->
                    Caller ! {counter_at, Name, 0}, 
                    put(Name,0),
                    counter_process();

                Defined ->
                    Caller ! {counter_at, Name, Defined},
                    counter_process()

            end;


        {Caller, adjust_counter, Name, By} ->

            case get(Name) of

                undefined ->
                    Caller ! {counter_at, Name, By},
                    put(Name,By),
                    counter_process();

                Defined ->
                    New = Defined+By,
                    Caller ! {counter_at, Name, New},
                    put(Name,New),
                    counter_process()

            end;


        {Caller, set_counter, Name, To} ->

            Caller ! {counter_at, Name, To},

            case To of

                0 ->
                    erase(Name);

                T ->
                    put(Name,T)

            end,

            counter_process()


    end.
