




%% @spec tuple_member(E::any(), T::tuple()) -> true | false

%% @doc Checks whether E is a member element of tuple T, analogous to `lists::member(E, L)'. ```1> scutil:tuple_member(b, {a,b,c}).
%% true
%%
%% 2> scutil:tuple_member(d, {a,b,c}).
%% false
%%
%% 3> scutil:tuple_member([1,2], {[1,2]}).
%% true'''

%% @since Version 123

tuple_member(E, T) -> 

    tuple_member(E, T, 1, size(T)).



tuple_member(_E,_T, I, Sz) 

    when I > Sz -> false;
    
    

tuple_member(E, T, I, Sz) ->

    case element(I, T) == E of

        true  ->
            true;

        false ->
            tuple_member(E, T, I+1, Sz)

    end.
