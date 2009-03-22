
%% @author John Haugeland <stonecypher@gmail.com>
%% @copyright 2007 - current John Haugeland, All Rights Reserved
%% @version $Revision$
%% @since Version 8

%% @doc <!-- google analytics --><script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script><script type="text/javascript">var pageTracker = _gat._getTracker("UA-4903191-10");pageTracker._trackPageview();</script>
%% <p></p>



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





%% @spec list_to_number(X::list()) -> number()

%% @doc {@section Conversion} Converts a list into a number; integers will be returned if there is no mantissa in the list representation. ```1> scutil:list_to_number("2").
%% 2
%%
%% 2> scutil:list_to_number("2.0").
%% 2.0
%%
%% 3> scutil:list_to_number("2.1").
%% 2.1'''

%% @since Version 8

list_to_number(X) ->

    case catch list_to_float(X) of

        {'EXIT',_} ->
            list_to_integer(X);

        Y ->
            Y

    end.





%% @type hexchar() = integer().  Integer must be in the range $0 - $9, the range $a - $f, or the range $A - $F, all inclusive, for inputs; outputs will always use lower case.
%% @type hexstring() = list().  All elements of the list must be of type {@type hexchar()}.

%% @spec hex_to_int(HexChar::hexstring() | hexchar()) -> integer()
%% @doc {@section Conversion} Convert a hexstring() or hexchar() into its numeric value. ```1> scutil:hex_to_int("c0ffEE").
%% 12648430
%%
%% 2> scutil:hex_to_int($e).
%% 14
%%
%% 3> scutil:hex_to_int("100").
%% 256'''

%% @since Version 18

hex_to_int(Hex) when is_integer(Hex), Hex >= $0, Hex =< $9 -> Hex - $0;
hex_to_int(Hex) when is_integer(Hex), Hex >= $a, Hex =< $f -> Hex - $a + 10;
hex_to_int(Hex) when is_integer(Hex), Hex >= $A, Hex =< $F -> Hex - $A + 10;

hex_to_int(Hex) when is_list(Hex) -> 
    hex_to_int(Hex, 0).

hex_to_int([],          Acc) -> Acc;
hex_to_int([Digit|Rem], Acc) -> hex_to_int(Rem, (Acc bsl 4) + hex_to_int(Digit)).





%% @type byte() = integer().  A byte must be an integer in the range 0-255, inclusive.  (Technically this is an octet, not a byte, but the word byte is extensively misused throughout the erlang documentation and standard library, which makes this an important concession, so we're when-in-Rome-ing.)

%% @spec byte_to_hex(TheByte::byte()) -> hexstring()

%% @doc {@section Conversion} Convert a byte() into a hexstring().  The hexstring() result will always be two characters (left padded with zero if necessary). ```1> scutil:byte_to_hex(7).
%% "07"
%%
%% 2> scutil:byte_to_hex(255).
%% "ff"'''

%% @since Version 20

byte_to_hex(TheByte) when is_integer(TheByte), TheByte >= 0, TheByte =< 255 -> 

    [ nybble_to_hex(TheByte bsr 4), nybble_to_hex(TheByte band 15) ].





%% @type nybble() = integer().  A nybble must be an integer in the range 0-15, inclusive.

%% @spec nybble_to_hex(Nyb::nybble()) -> integer()

%% @doc {@section Conversion} Convert a nybble() to a hexchar(). ```1> scutil:nybble_to_hex(7).
%% 55
%%
%% 2> scutil:nybble_to_hex(15).
%% 102'''

%% @since Version 19

nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 0,  Nyb < 10 ->

    $0 + Nyb;
    


nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 10, Nyb < 16 ->

    $a + Nyb - 10.





%% @type io_list() = list().  Every list member of an {@type io_list()} must be a {@type byte()}.

%% @spec io_list_to_hex(Input::io_list()) -> hexstring()

%% @doc {@section Conversion} Convert an io_list() to a hexstring().  ```1> scutil:io_list_to_hex("a").
%% "61"
%%
%% 2> scutil:io_list_to_hex("a08n408nbqa").
%% "6130386e3430386e627161"'''

%% @since Version 19

io_list_to_hex(Input) when is_list(Input) ->

    io_list_to_hex(Input, []).



io_list_to_hex([], Work) -> 

    lists:reverse(Work);
    


io_list_to_hex([Item|Remainder], Work) when is_integer(Item), Item >= 0, Item =< 255 -> 

    [A,B] = byte_to_hex(Item), 
    io_list_to_hex(Remainder, [B,A]++Work);
    


io_list_to_hex(_, _) ->

    {error, not_an_io_list}.
