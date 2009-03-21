




%% @equiv int_to_u32_iolist(X, little)

int_to_u32_iolist(X) ->

    int_to_u32_iolist(X, little).



%% @todo need spec, doc, since

int_to_u32_iolist(X, little) when X>=0, X<4294967296 ->

    binary_to_list( <<X:32/little>> );

int_to_u32_iolist(X, big)    when X>=0, X<4294967296 ->
    binary_to_list( <<X:32/big>> ).





%% @equiv u32_iolist_to_int(A,B,C,D, little)

u32_iolist_to_int( [A,B,C,D] ) ->
   u32_iolist_to_int(A,B,C,D, little).



%% @equiv u32_iolist_to_int(A,B,C,D, Endianness)

u32_iolist_to_int( [A,B,C,D], little ) ->
    u32_iolist_to_int(A,B,C,D, little);

u32_iolist_to_int( [A,B,C,D], big ) ->
    u32_iolist_to_int(A,B,C,D, big).





%% @equiv u32_iolist_to_int(A,B,C,D, little)

u32_iolist_to_int( A,B,C,D ) ->

    u32_iolist_to_int( A,B,C,D, little).


%% @todo need spec, doc, since

u32_iolist_to_int( A,B,C,D , little  ) ->

    <<X:32/little>> = list_to_binary([A,B,C,D]),
    X;

u32_iolist_to_int( A,B,C,D , big  )    ->

    <<X:32/big>> = list_to_binary([A,B,C,D]),
    X.





%% @equiv int_to_u64_iolist(X, little)

int_to_u64_iolist(X) ->
    int_to_u64_iolist(X, little).


%% @todo need spec, doc, since

int_to_u64_iolist(X, little) ->
    binary_to_list( <<X:64/little>> );

int_to_u64_iolist(X, big) ->
    binary_to_list( <<X:64/big>> ).





%% @equiv u64_iolist_to_int(A,B,C,D,E,F,G,H, little)

u64_iolist_to_int( [A,B,C,D,E,F,G,H] ) ->
    u64_iolist_to_int( A,B,C,D,E,F,G,H, little).


%% @equiv u64_iolist_to_int(A,B,C,D,E,F,G,H, Endianness)

u64_iolist_to_int( [A,B,C,D,E,F,G,H], little ) ->
    u64_iolist_to_int( A,B,C,D,E,F,G,H, little);

u64_iolist_to_int( [A,B,C,D,E,F,G,H], big ) ->
    u64_iolist_to_int( A,B,C,D,E,F,G,H, big).





%% @equiv u64_iolist_to_int(A,B,C,D,E,F,G,H, little)

u64_iolist_to_int( A,B,C,D,E,F,G,H ) -> 
    u64_iolist_to_int(A,B,C,D,E,F,G,H, little).


%% @todo need spec, doc, sinces
u64_iolist_to_int( A,B,C,D,E,F,G,H , little ) ->

    <<X:64/little>> = list_to_binary([A,B,C,D,E,F,G,H]),
    X;

u64_iolist_to_int( A,B,C,D,E,F,G,H , big ) ->

    <<X:64/big>> = list_to_binary([A,B,C,D,E,F,G,H]),
    X.





%% @todo need spec, doc, since

float_to_f32_iolist(X) ->
    float_to_f32_iolist(X, little).


%% @todo need spec, doc, since
float_to_f32_iolist(X, little) ->
    binary_to_list(<<X:32/float-little>>);

float_to_f32_iolist(X, big) ->
    binary_to_list(<<X:32/float-big>>).





%% @equiv f32_iolist_to_int(A,B,C,D, little)
f32_iolist_to_int( [A,B,C,D] ) -> 
    f32_iolist_to_int(A,B,C,D, little).


%% @equiv f32_iolist_to_int(A,B,C,D, Endianness)
f32_iolist_to_int( [A,B,C,D], little ) ->
    f32_iolist_to_int(A,B,C,D, little);

f32_iolist_to_int( [A,B,C,D], big ) ->
    f32_iolist_to_int(A,B,C,D, big).





%% @equiv f32_iolist_to_int(A,B,C,D, little)
f32_iolist_to_int( A,B,C,D  ) -> 
    f32_iolist_to_int(A,B,C,D, little).


%% @todo need spec, doc, since
f32_iolist_to_int( A,B,C,D , little ) ->
    <<X:32/float-little>> = list_to_binary([A,B,C,D]),
    X;

f32_iolist_to_int( A,B,C,D , big ) -> 
    <<X:32/float-big>> = list_to_binary([A,B,C,D]), 
    X.
