




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
