




%% @since Version 201

mersenne_prime(Which) -> mersenne_prime_worker(Which, 1).



%% @private

mersenne_prime_worker(0, Current) -> 

    Current - 1;
    
    

mersenne_prime_worker(Remain, Current) when Remain > 30 -> 

    mersenne_prime_worker(Remain-30, Current*1073741824);
    
    

mersenne_prime_worker(Remain, Current) -> 

    mersenne_prime_worker(Remain-1, Current*2).





%% @since Version 172

floor(X) ->

     floor_t(trunc(X), trunc(X)-X).



%% @private

floor_t(T, Td) when Td < 0 -> T;
floor_t(T, Td) when Td > 0 -> T-1;
floor_t(T,_Td)             -> T.





%% @since Version 172
%% @equiv ceiling(X)

ceil(X) -> 

     ceiling(X).





%% @since Version 172

ceiling(X) ->

     ceiling_t(trunc(X), trunc(X)-X).



%% @private

ceiling_t(T, Td) when Td < 0 -> T+1;
ceiling_t(T, Td) when Td > 0 -> T;
ceiling_t(T,_Td)             -> T.





%% @since Version 168

factorial(X) ->

    factorial(X, 1).



%% @private

factorial(0, _Counter) -> 

    0;
    
    

factorial(1, Counter) -> 

    Counter;



factorial(X, Counter) when is_integer(X), X > 1 ->

    factorial(X-1, Counter*X).





%% @spec square(Input::number()) -> number()

%% @doc {@section Math} Squares the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> scutil:square(2).
%% 4
%%
%% 2> scutil:square(2.5).
%% 6.25'''

%% @since Version 108

square(X) -> 
    
    X*X.





%% @spec cube(Input::number()) -> number()

%% @doc {@section Math} Cubes the input; convenient in list comprehensions to prevent recalculation, and clear in the fashion of documentary functions. ```1> scutil:cube(2).
%% 8
%%
%% 2> scutil:cube(2.5).
%% 6.25'''

%% @since Version 165

cube(X) -> 

    X*X*X.





%% @spec mod(Base::integer(), Range::integer()) -> integer()

%% @doc {@section Math} Takes the modulus of an integer by another integer.  Luckily, erlang calls what most languages refer to as modulus by its correct name, remainder (c's `%', erlang's `rem').  Modulus is implemented incorrectly in nearly every language, because chip vendors implement remainder and the wrong name stuck.  The difference is in how the operator reacts to a negative `Base': -10 modulo 3 is 2, whereas -10 rem 3 is -1.  Remainder takes the residue of dividing the base by the lowest (nearest negative infinity) integer N adjacent the real valued divisor; modulo returns the highest, which is less CPU efficient but always provides an answer on [0..Range-1]. ```1> scutil:mod(10,3).
%% 1
%%
%% 2> [ scutil:mod(X,4) || X <- lists:seq(-10,10) ].
%% [2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2]'''
%%
%% @since Version 29

mod(Base, Range) when is_integer(Base), is_integer(Range) ->

    case Base rem Range of

        X when X < 0 ->
            X + Range;

        Z ->
            Z

    end.
