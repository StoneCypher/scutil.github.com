




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





%% @type numeric_tuple() = tuple().  Every member of a {@type numeric_tuple()} must be a {@type number()}.
%% @type relaxed_numeric_tuple() = numeric_tuple().  Relaxed numeric tuples are allowed to contain non-numeric elements, which are treated as zero for purposes of computation.

%% @spec tuple_sum(T::relaxed_numeric_tuple()) -> number()

%% @doc {@section Math} Returns the sum of the numeric elements of a tuple, treating non-numeric elements as zero. ```1>'''

%% @since Version 86

tuple_sum(T) when is_tuple(T) -> 

    tuple_sum(T, 1, size(T), 0).



tuple_sum(_T, Which, Max, Work) when Which > Max -> 

    Work;



tuple_sum( T, Which, Max, Work) ->

     tuple_sum(T, Which+1, Max, Work+element(Which, T)).
