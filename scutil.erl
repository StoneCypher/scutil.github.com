
-module(scutil).

-author("John Haugeland - stonecypher@gmail.com").
-webpage("http://scutil.com/").
-license( {mit_license, "http://crunchyd.com/scutil/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-bugtracker("http://crunchyd.com/forum/project.php?projectid=7").
-publicforum("http://crunchyd.com/forum/scutil-discussion/").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("StoneCypher's utility library.  There's a lot of chaff in here.").

-testerl_export( { [], scutil_testsuite } ).

-export( [
    type_of/1,
    get_module_attribute/2,
    byte_to_hex/1, nybble_to_hex/1, io_list_to_hex/1,
    regexp_read_matches/2, regexp_read_matches/3,
    multi_do/3, multi_do/4
] ).





type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
%type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;

type_of(_X)                     -> unknown.





get_module_attribute(Module,Attribute) ->

    % Found at http://www.astahost.com/info.php/mastering-erlang-part-3-erlang-concurrent_t6632.html
    % Reformatted for clarity, removed unnessecary framing list
    % Added error handling behavior

    case beam_lib:chunks(Module, [attributes]) of

        { ok, { _, [ {attributes,Attributes} ] } } ->
            case lists:keysearch(Attribute, 1, Attributes) of
                { value, {Attribute,[Value]} } -> Value;
                false                          -> { error, no_such_attribute }
            end;

        { error, beam_lib, { file_error, _, enoent} } ->
            { error, no_such_module }

    end.




byte_to_hex(TheByte) when is_integer(TheByte), TheByte >= 0, TheByte =< 255 -> { nybble_to_hex(TheByte bsr 4), nybble_to_hex(TheByte band 15) };
byte_to_hex(NotAByte)                                                       -> { error, { not_a_byte, NotAByte }}.





nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 0,  Nyb < 10 -> $0 + Nyb;
nybble_to_hex(Nyb) when is_integer(Nyb), Nyb >= 10, Nyb < 16 -> $a + Nyb - 10;
nybble_to_hex(_)                                                           -> { error, not_a_nybble }.





io_list_to_hex(Input) when is_list(Input)                                            -> io_list_to_hex(Input, []).

io_list_to_hex([],               Work)                                               -> lists:reverse(Work);
io_list_to_hex([Item|Remainder], Work) when is_integer(Item), Item >= 0, Item =< 255 -> {A,B} = byte_to_hex(Item), io_list_to_hex(Remainder, [B,A]++Work);
io_list_to_hex(_,                _)                                                  -> {error, not_an_io_list}.





multi_do(C, Module, Func)             -> multi_do(C, Module, Func, [],   []).
multi_do(C, Module, Func, Args)       -> multi_do(C, Module, Func, Args, []).

multi_do(0,_Module,_Func,_Args, Work) -> Work;
multi_do(I, Module, Func, Args, Work) -> multi_do(I-1, Module, Func, Args, Work ++ [apply(Module, Func, Args)]).





% was regexp_read_matches in pre-svn

regex_read_matches(String, Reg)                          -> regexp_read_matches(String, Reg, {0,0}).
regex_read_matches(String, Reg, {TrimFront, TrimLength}) ->

    case regexp:matches(String, Reg) of
        { match, [] }      -> no_match;
        { match, Matches } -> [ string:substr(String, Start+TrimFront, End-TrimLength) || {Start,End} <- Matches ];
        { error, E }       -> { error, E }
    end.
