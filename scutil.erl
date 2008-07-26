
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
    byte_to_hex/1, nybble_to_hex/1, io_list_to_hex/1, % need tests
    regex_read_matches/2, regex_read_matches/3, % need tests
    multi_do/3, multi_do/4, % need tests
    grid_scatter/2, % need tests
    elements/2, elements/3, elements/4, % needs tests
    random_generator/3, srand/0, rand/1, random_from/1, random_from/2, random_from/3, random_from_weighted/1 % need tests
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

regex_read_matches(String, Reg)                          -> regex_read_matches(String, Reg, {0,0}).
regex_read_matches(String, Reg, {TrimFront, TrimLength}) ->

    case regexp:matches(String, Reg) of
        { match, [] }      -> no_match;
        { match, Matches } -> [ string:substr(String, Start+TrimFront, End-TrimLength) || {Start,End} <- Matches ];
        { error, E }       -> { error, E }
    end.





grid_scatter(Count, {SizeX, SizeY}) -> scutil:random_from(Count, [ {X,Y} || X <- scutil:int_range(SizeX), Y <- scutil:int_range(SizeY) ]);
grid_scatter(Count, Size)           -> grid_scatter(Count, {Size, Size}).





srand() ->

    Now = erlang:now(),
    RandomGeneratorPid = spawn(?MODULE, random_generator, [element(1, Now), element(2, Now), element(3, Now)]),

    SCR = whereis(scutil_rand_source),

    if SCR == undefined -> 0; true -> unregister(scutil_rand_source) end,
    register(scutil_rand_source, RandomGeneratorPid),
    { ok, { seeded, Now } }.





random_generator(SeedA, SeedB, SeedC) ->

    random:seed(SeedA, SeedB, SeedC),
    random_generator().





random_generator() ->

    receive

        terminate ->
            { ok, terminated };

        [Return, Range] ->
            Val = random:uniform(Range),
            Return ! Val,
            random_generator();

        _  ->
            random_generator()

    end.





rand(Range) ->

    case whereis(scutil_rand_source) of

        undefined ->
            srand(),
            rand(Range);

        _ ->

            scutil_rand_source ! [ self(), Range ],
            receive RandVal -> RandVal - 1 end

    end.





random_from([])                                                             -> [];
random_from(List)                     when                    is_list(List) -> random_from(List, noremainder).

random_from(_,     [])                                                      -> [];
random_from(0,     List)              when                    is_list(List) -> [];
random_from(1,     List)              when                    is_list(List) -> Pick = random_from(List), [Pick];
random_from(Count, List)              when is_integer(Count), is_list(List) -> Pick = random_from(List), [Pick] ++ random_from(Count-1, List -- [Pick]);

random_from([],   remainder)                                                -> { undefined, [] };
random_from(List, remainder)          when                    is_list(List) -> Item = lists:nth(scutil:rand(length(List)) + 1, List), {Item, List--[Item]};
random_from(List, noremainder)        when                    is_list(List) -> lists:nth(scutil:rand(length(List)) + 1, List).

random_from(_,     [],   noremainder)                                       -> [];
random_from(0,     List, noremainder) when                    is_list(List) -> [];
random_from(1,     List, noremainder) when                    is_list(List) -> random_from(List);
random_from(Count, List, noremainder) when is_integer(Count), is_list(List) -> Pick = random_from(List), [Pick] ++ random_from(Count-1, List -- [Pick]);

random_from(_,     [],   remainder)                                         -> { undefined, [] };
random_from(0,     List, remainder)   when                    is_list(List) -> {[], List};
random_from(1,     List, remainder)   when                    is_list(List) -> { Item, RList } = random_from(List, remainder), { [Item], RList };

random_from(Count, List, remainder)   when is_integer(Count), is_list(List) ->

    { Pick,  Rem  } = random_from(List, remainder),
    { NPick, NRem } = random_from(Count-1, Rem, remainder),
    { [Pick] ++ NPick, NRem }.





% InputList is [ {Item,Weight}, {Item,Weight}, ... ]
random_from_weighted(InputList) when is_list(InputList) ->
    RandomLimit = rand(lists:sum([ Weight || {_,Weight} <- InputList ])),  % the random cap is equal to the sum of all the weights
    random_from_weighted_worker(InputList, RandomLimit).                   % call the worker with the original list and the cap

% if the list is empty, the cap for randomness was calculated wrongly, and as such the random point is too high
random_from_weighted_worker([], _) -> { error, limit_miscalculation };

% but if the list has reasonable contents and the limit is a pos-or-0 integer
random_from_weighted_worker(InputList, Limit) when is_list(InputList) andalso is_integer(Limit) andalso Limit >= 0 ->
    [ {Item,Weight} | Remainder ] = InputList,   % break off the input list's head as {I,W} and keep the rest as Remainder
    case Weight =< Limit of                                             % if the weight is less than or equal to the limit,
        true  -> random_from_weighted_worker(Remainder, Limit-Weight);  % recurse the next item with a decremented weight
        false -> Item                                                   % if not, this item is the one we want
    end.





% todo implement catching tuple { key, reqtype } from list, to auto-convert before return

% interface

elements(Config, Requested)                when is_list(Config) andalso is_list(Requested)                            -> elements_worker([], Config, Requested, 1).
elements(Config, Requested, KeyIdx)        when is_list(Config) andalso is_list(Requested) andalso is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx);

elements(Config, Requested, strip)         when is_list(Config) andalso is_list(Requested)                            -> elements_worker([], Config, Requested, 1,      strip).
elements(Config, Requested, KeyIdx, strip) when is_list(Config) andalso is_list(Requested) andalso is_integer(KeyIdx) -> elements_worker([], Config, Requested, KeyIdx, strip).

% implementation

elements_worker(Retlist, _,      [],        _)      -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of
        false            -> elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx);
        { value, Tuple } -> elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx);
        AnythingElse     -> { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }
    end.

elements_worker(Retlist, _,      [],        _,      strip) -> Retlist;
elements_worker(Retlist, Config, Requested, KeyIdx, strip) ->

    [ ThisRequest | RemainingRequests ] = Requested,

    case lists:keysearch(ThisRequest, KeyIdx, Config) of
        false                -> elements_worker(Retlist ++ [undefined], Config, RemainingRequests, KeyIdx, strip);
        { value, {_,Tuple} } -> elements_worker(Retlist ++ [Tuple],     Config, RemainingRequests, KeyIdx, strip);
        AnythingElse         -> { error, response_not_understood, { for, lists, keysearch, { ThisRequest, Config } }, { got, AnythingElse } }
    end.
