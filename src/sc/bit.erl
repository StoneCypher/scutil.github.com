




%% @spec has_bit(Number::non_negative_integer(), Bit::non_negative_integer()) -> true | false

%% @doc {@section Utility} Checks whether a given bit is on in a sufficiently sized unsigned two's compliment integer representation of `Num'.  ```1> scutil:has_bit(5,0).
%% true
%%
%% 2> scutil:has_bit(5,1).
%% false'''

%% @since Version 9

has_bit(Num, Bit) when is_integer(Num), is_integer(Bit), Num > 0, Bit >= 0, Bit < 64 -> 

    (Num band (1 bsl Bit)) > 0.





%% @spec count_bits(Number::non_negative_integer()) -> non_negative_integer()

%% @doc {@section Utility} Counts the number of bits turned on in a sufficiently sized unsigned two's compliment integer representation of `Num'.  ```1> scutil:count_bits(5).
%% 2'''

%% @since Version 9

count_bits(Num) when is_integer(Num), Num > 0 ->

    length( [S || S <- lists:seq(0,63), has_bit(Num, S) == true] ).
