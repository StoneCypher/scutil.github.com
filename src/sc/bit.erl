
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <p></p>


%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Website is</span><a href="http://scutil.com/">http://scutil.com/</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">Author's Website</span><a href="http://fullof.bs">Full of BS</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#eef;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This library is released under the</span><a href="http://scutil.com/license.html">MIT License</a></span>
%% @reference <span style="padding:0.1em 0.4em;background-color:#efe;display:inline-block;width:47em"><span style="display:inline-block;width:18em">This build was released</span><tt style="text-decoration:underline;background-color:#eee">$Date: 2009-03-15 13:47:01 -0600 (Sun, 15 Mar 2009) $</tt></span>

%% @todo add @see cross-references between related functions
%% @todo add thanks tables and cross-references
%% @todo add dependant libraries table
%% @todo add untested warnings to beginnings of @doc tags
%% @todo add defective warnings to beginnings of @doc tags
%% @todo add links to test data
%% @todo add sections to examples: descriptive text, code example, what's it for, related, thanks





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
