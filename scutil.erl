
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

-description("StoneCypher's utility library.").

-testerl_export( { [], scutil_testsuite } ).





-export( [

    type_of/1,
    get_module_attribute/2,
    byte_to_hex/1, nybble_to_hex/1, io_list_to_hex/1, % needs tests
    regex_read_matches/2, regex_read_matches/3, % needs tests
    multi_do/3, multi_do/4, % needs tests
    elements/2, elements/3, elements/4, % needs tests
    sanitize_tokens/2,
    sanitize_filename/1, % needs tests
    random_generator/3, srand/0, rand/1, random_from/1, random_from/2, random_from/3, random_from_weighted/1, % needs tests
    grid_scatter/2, % needs tests
    list_product/1, % needs tests
    even_or_odd/1, % needs tests
    histograph/1, % needs tests
    median/1, % needs tests
    mode/1, % needs tests
    arithmetic_mean/1, geometric_mean/1, harmonic_mean/1, weighted_arithmetic_mean/1,  % needs tests
    absolute_difference/2, % needs tests
    std_deviation/1, % needs tests
    root_mean_square/1, % needs tests
    moment/2, moments/1, moments/2, % needs tests
    central_moment/2, central_moments/1, central_moments/2, % needs tests
%    weighted_geometric_mean/1,

    dot_product/2, % needs tests
    vector_magnitude/1, % needs tests
    qsp_average/2, % needs tests
    normalize_vector/1, % needs tests
    amean_vector_normal/1, gmean_vector_normal/1, hmean_vector_normal/1, % needs tests

    ranks_of/1, % needs tests
    tied_ranks_of/1, % needs tests
    ordered_ranks_of/1, % needs tests
    pearson_correlation/1,  pearson_correlation/2, % needs tests
    spearman_correlation/1, spearman_correlation/2, % needs tests
    kendall_correlation/1,  kendall_correlation/2, % needs tests
    skewness/1, % needs tests
    kurtosis/1, % needs tests
    to_lines/1, % needs tests

    receive_one/0, % needs tests

    in_both_lists/1, in_both_lists/2, % needs tests
    all_unique_pairings/1, % needs tests
    walk_unique_pairings/2, % needs tests
    list_to_num/1, % needs tests
    counter/1, inc_counter/1, reset_counter/1, counter_process/0, % needs tests
    start_register_if_not_running/4, % needs tests
    wait_until_terminate/0, wait_until_terminate/1, % needs tests
    module_has_function/2, % needs tests
    regexp_read_matches/2, regexp_read_matches/3, % needs tests

    call_after/2, call_after/3, call_after/4, call_after_worker/4, % needs tests
    shuffle/1, % needs tests

    hex_to_int/1 % needs tests

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





hex_to_int(Hex) when is_integer(Hex), Hex >= $0, Hex =< $9 -> Hex - $0;
hex_to_int(Hex) when is_integer(Hex), Hex >= $a, Hex =< $f -> Hex - $a + 10;
hex_to_int(Hex) when is_integer(Hex), Hex >= $A, Hex =< $F -> Hex - $A + 10;

hex_to_int(Hex) when is_list(Hex) -> hex_to_int(Hex, 0).

hex_to_int([],          Acc) -> Acc;
hex_to_int([Digit|Rem], Acc) -> hex_to_int(Rem, (Acc bsl 4) + hex_to_int(Digit)).





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

    case whereis(scutil_rand_source) of
        undefined -> ok;
        _Defined  -> unregister(scutil_rand_source)
    end,

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





random_from(   List) -> [X] = random_from(1, List, no_remainder), X.
random_from(N, List) -> random_from(N, List, no_remainder).

random_from(N, List, no_remainder) -> {R,_} = random_from(N,List,remainder), R;
random_from(N, List, remainder)    -> lists:split(N,shuffle(List)).





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
% todo There may be a crashing bug here for repeated attributes, which are apparently legal, see http://fullof.bs/reading-module-attributes-in-erlang#comment-466
% todo It may help to re-implement this using proplists instead of doing it manually, profile

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





sanitize_tokens(List, Allowed) when is_list(List), is_function(Allowed) -> lists:filter(Allowed, List);
sanitize_tokens(List, Allowed) when is_list(List), is_list(Allowed)     -> lists:filter(fun(X) -> lists:member(X,Allowed) end, List).





sanitize_filename(Filename) -> sanitize_tokens(Filename, lists:seq($a,$z)++lists:seq($A,$Z)++lists:seq($0,$9)++"-_=()[]").





% batch_reduce(Workload, Function) ->
%
%     [ spawn(Function, LoadItem) || LoadItem <- Workload ],
%     reduce_receive(length(Workload, [])).
%
% reduce_receive(0,           Work) -> Work;
% reduce_receive(AnswerCount, Work) -> receive X -> reduce_receive(AnswerCount-1, [X]++Work).





% distributed_batch_reduce(Workload, Function, Nodes) -> distributed_batch_reduce_handout(Workload, Function, Nodes, [], 0).

% distributed_batch_reduce_handout([],            _Function, _Nodes,              Work, 0)        -> Work;                                                                                                                                   % nothing left in the work queue, count of output outstanding is 0, work's done
% distributed_batch_reduce_handout([],             Function, _Nodes,              Work, CountOut) -> {_Source, Result } = scutil:receive_one(), distributed_batch_reduce_handout([],       Function,  [],       [Result]++Work, CountOut-1); % there's no work left in the queue, but stuff outstanding from child nodes
% distributed_batch_reduce_handout(Workload,       Function,  [],                 Work, CountOut) -> { Source, Result } = scutil:receive_one(), distributed_batch_reduce_handout(Workload, Function,  [Source], [Result]++Work, CountOut-1); % no nodes available, wait for a receive, queue the result and add the node back to the available list
% distributed_batch_reduce_handout([Item|WorkRem], {Mod,Fun}, [ThisNode|NodeRem], Work, CountOut) -> spawn(ThisNode, Mod, Fun, Item),           distributed_batch_reduce_handout(WorkRem,  {Mod,Fun}, NodeRem,  Work,           CountOut+1). % work and nodes available; dispatch some work, increment the work out counter and recurse





% dissimilar_charset(english, lowercase) -> "abcdefghjklmnopqrstuwxyz";
% dissimilar_charset(english, mixedcase) -> "abcdefghjklmnopqrstuwxyzABDEFGHRT";
% dissimilar_charset(english, alphanum)  -> "abcdefghjklmnopqrstuwxyzABDEFGHRT34679".

% similarize_charset   a10OZ2B8 -> aloozzBB





receive_one() ->

    receive (X) -> X
    after 1     -> nothing_there
    end.





arithmetic_mean(List) when is_list(List) -> lists:sum(List) / length(List).
geometric_mean(List)  when is_list(List) -> math:exp(scutil:arithmetic_mean([math:log(X)||X<-List])).
harmonic_mean(List)   when is_list(List) -> length(List) / lists:sum([ 1/X || X<-List ]).



% geometric_mean(List)  when is_list(List) -> math:pow(scutil:list_product(List), 1/length(List)). % replaced because this fails on huge lists due to c std lib precision issues





weighted_arithmetic_mean(List)   when is_list(List) -> weighted_arithmetic_mean(List, 0, 0).

weighted_arithmetic_mean([],           Num, Denom)  -> Num/Denom;
weighted_arithmetic_mean([{W,V}|Tail], Num, Denom)  -> weighted_arithmetic_mean(Tail, Num+(W*V), Denom+W).





% todo this should be guards, not an if

even_or_odd(Num) when is_integer(Num) ->

    if
        Num band 1 == 0 -> even;
        true            -> odd
    end.





median(List) when is_list(List) ->

    SList = lists:sort(List),
    Length = length(SList),
    case even_or_odd(Length) of
        even -> [A,B] = lists:sublist(SList, round(Length/2), 2), (A+B)/2;
        odd  -> lists:nth( round((Length+1)/2), SList )
    end.





mode([])                      -> [];
mode(List) when is_list(List) -> mode_front(lists:reverse(lists:keysort(2, scutil:histograph(List)))).

mode_front([{Item,Freq}|Tail])                      -> mode_front(Tail, Freq, [Item]).

mode_front([ {Item, Freq} | Tail], Freq,   Results) -> mode_front(Tail, Freq, [Item]++Results);
mode_front([{_Item,_Freq} |_Tail],_Better, Results) -> Results;
mode_front([],                    _Freq,   Results) -> Results.





absolute_difference(A,B) -> abs(A-B).





list_product(List) when is_list(List) -> list_product(List, 1).

list_product([],          Counter) -> Counter;
list_product([Head|Tail], Counter) -> list_product(Tail, Counter*Head).





histograph(List) when is_list(List) ->

    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).

histo_count([],             Current, Count, Work) -> lists:reverse([{Current,Count}]++Work);
histo_count([Current|Tail], Current, Count, Work) -> histo_count(Tail, Current, Count+1, Work);
histo_count([New|Tail],     Current, Count, Work) -> histo_count(Tail, New,     1,       [{Current,Count}]++Work).





std_deviation(Values) when is_list(Values) ->

    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ (Val-Mean)*(Val-Mean) || Val <- Values ])).





root_mean_square(List) when is_list(List) -> math:sqrt(arithmetic_mean([ Val*Val || Val <- List ])).





ranks_of(List) when is_list(List) -> lists:zip(lists:seq(1,length(List)),lists:reverse(lists:sort(List))).





% todo comeback make a tied_ranks_of/2 which takes a sorting predicate
% needs significant refactoring; work is being repeated

tied_ranks_of(List) -> tied_rank_worker(ranks_of(List), [], no_prev_value).

tied_add_prev(Work, {FoundAt, NewValue}) -> lists:duplicate(length(FoundAt),{lists:sum(FoundAt) / length(FoundAt), NewValue}) ++ Work.


tied_rank_worker([],               Work, PrevValue) -> lists:reverse(tied_add_prev(Work, PrevValue));
tied_rank_worker([Item|Remainder], Work, PrevValue) ->
    case PrevValue of
        no_prev_value ->
            {BaseRank,BaseVal} = Item,
            tied_rank_worker(Remainder, Work, {[BaseRank],BaseVal});
        {FoundAt,OldVal} ->
            case Item of
                {Id,OldVal} ->
                    tied_rank_worker(Remainder, Work,                           {[Id]++FoundAt,OldVal});
                {Id,NewVal} ->
                    tied_rank_worker(Remainder, tied_add_prev(Work, PrevValue), {[Id],NewVal})
            end
    end.





% todo comeback make an ordered_ranks_of/2 which takes a sorting predicate

ordered_ranks_of(List) when is_list(List) ->
    ordered_ranks_of(List, tied_ranks_of(List), []).

ordered_ranks_of([], [], Work) -> lists:reverse(Work);

ordered_ranks_of([Front|Rem], Ranks, Work) ->
    {value,Item} = lists:keysearch(Front,2,Ranks),
    {IRank,Front} = Item,
    ordered_ranks_of(Rem, Ranks--[Item], [{IRank,Front}]++Work).





% annote(Group, Key, Value) ->
% sc_config, make annote -> config, forget -> delete, recall -> get_config





to_lines(Text) -> string:tokens(Text, "\r\n"). % yay convenience functions





% test data at http://changingminds.org/explanations/research/analysis/pearson.htm

pearson_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    pearson_correlation(A,B).

pearson_correlation(List1, _)     when length(List1) < 2 -> {r,0.0};

pearson_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
pearson_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    SumXY = lists:sum([A*B || {A,B} <- lists:zip(List1,List2) ]),   % the sum of the products of each matched pair

    SumX  = lists:sum(List1),
    SumY  = lists:sum(List2),

    SumXX = lists:sum([L*L || L<-List1]),                           % the sums of the squared items
    SumYY = lists:sum([L*L || L<-List2]),

    N     = length(List1),

    case math:sqrt(   ( (N*SumXX)-(SumX*SumX) )   *   ( (N*SumYY)-(SumY*SumY) )   ) of
        0     -> {r,0.0};  % some nasty values otherwise cause divide by zero
        0.0   -> {r,0.0};  % eg [ [1,1,1,1,1], [1,1,2,1,2] ]
        Denom ->
          Numer = (N*SumXY) - (SumX * SumY),
          {r, (Numer/Denom)}
    end.





% test data at  http://geographyfieldwork.com/SpearmansRank.htm

spearman_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    spearman_correlation(A,B).

spearman_correlation(List1, _)     when length(List1) < 2 -> {rsquared,0.0};

spearman_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
spearman_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {TR1,_} = lists:unzip(ordered_ranks_of(List1)),
    {TR2,_} = lists:unzip(ordered_ranks_of(List2)),

    Numerator   = 6 * lists:sum([ (D1-D2)*(D1-D2) || {D1,D2} <- lists:zip(TR1,TR2) ]),
    Denominator = math:pow(length(List1),3)-length(List1),

    {rsquared,1-(Numerator/Denominator)}.





% http://changingminds.org/explanations/research/analysis/kendall.htm

kendall_correlation(TupleList) when is_list(TupleList) ->
    {A,B} = lists:unzip(TupleList),
    kendall_correlation(A,B).

kendall_correlation(List1, _)     when length(List1) < 2 -> {tau,0.0};

kendall_correlation(List1, List2) when is_list(List1), is_list(List2), length(List1) /= length(List2) -> {error, lists_must_be_same_length};
kendall_correlation(List1, List2) when is_list(List1), is_list(List2) ->

    {RA,_} = lists:unzip(ordered_ranks_of(List1)),
    {RB,_} = lists:unzip(ordered_ranks_of(List2)),

    Ordering = lists:keysort(1,lists:zip(RA,RB)),
    {_,OrdB} = lists:unzip(Ordering),

    N = length(List1),
    P = lists:sum(kendall_right_of(OrdB, [])),

    {tau, -(( (4*P) / (N * (N - 1))) - 1) }.



kendall_right_of([],    Work) -> lists:reverse(Work);
kendall_right_of([F|R], Work) -> kendall_right_of(R, [kendall_right_of_item(F,R)]++Work).

kendall_right_of_item(B, Rem) -> length([R || R <- Rem, R < B]).





% thanks to Chile and Kraln for straightening me out on moments and central moments

moment(List, N) when is_list(List), is_number(N) ->
    scutil:arithmetic_mean( [ math:pow(Item, N) || Item <- List ] ).

moments(List)                                -> moments(List, [2,3,4]).
moments(List, Moments) when is_list(Moments) -> [ moment(List, M) || M <- Moments ].





central_moment(List, N) when is_list(List), is_number(N) ->
    ListAMean = scutil:arithmetic_mean(List),
    scutil:arithmetic_mean( [ math:pow(Item-ListAMean, N) || Item <- List ] ).

central_moments(List)                                -> central_moments(List, [2,3,4]).
central_moments(List, Moments) when is_list(Moments) -> [ central_moment(List, M) || M <- Moments ].





skewness(List) -> central_moment(List, 3).
kurtosis(List) -> central_moment(List, 4).





% quadratic scalar product average
% see http://www.inf.fu-berlin.de/inst/ag-ki/rojas_home/documents/1996/NeuralNetworks/K5.pdf pdf-page 15
% Thanks to the following for help with qsp_average and dependencies: Asterick, Chile, John Sensebe, PfhorSlayer, Raleigh

qsp_average(W, InputVecs) ->

    GetSqVnDp = fun(Xi) ->
        VnDp = vector_magnitude(dot_product(W, Xi)),
        VnDp * VnDp
        end,

    arithmetic_mean([ GetSqVnDp(Xi) || Xi <- InputVecs ]).





dot_product(VX, VY) when length(VX) == length(VY) ->
    lists:sum( [ X*Y || {X,Y} <- lists:zip(VX,VY) ] ).





vector_magnitude(VX) ->
    math:sqrt(lists:sum([ X*X || X <- VX ])).





normalize_vector(VX) ->
    VM = vector_magnitude(VX),
    [ X / VM || X <- VX ].





amean_vector_normal(VX) -> arithmetic_mean(normalize_vector(VX)).
gmean_vector_normal(VX) ->  geometric_mean(normalize_vector(VX)).
hmean_vector_normal(VX) ->   harmonic_mean(normalize_vector(VX)).





% Create reverse sorted list X of 3-ary tuples {K,Ai,Bi} from sorted lists A, B of 2ary {K,Ai}/{K,Bi} tuples where key K appears in both A and B

in_both_lists(TupleList) when is_list(TupleList) -> 
    {A,B} = lists:unzip(TupleList),
    in_both_lists(A,B).

in_both_lists(A,B) when is_list(A), is_list(B) ->
    both_lists_next_item(A,B,[]).

both_lists_next_item([],             _,              Work) -> Work;
both_lists_next_item(_,              [],             Work) -> Work;
both_lists_next_item([ {K,Ai} | Ar], [ {K,Bi} | Br], Work) -> both_lists_next_item(Ar, Br, [{K,Ai,Bi}]++Work);

both_lists_next_item(IA,             IB,             Work) ->
    [{Ka,_}|Ar] = IA,
    [{Kb,_}|Br] = IB,
    if
        Ka < Kb -> both_lists_next_item(Ar, IB, Work);
        true    -> both_lists_next_item(IA, Br, Work)
    end.





% collects results; do not use for huge lists

all_unique_pairings(A) when is_list(A) -> all_unique_pairings(A,[]).

all_unique_pairings([],      Work) -> Work;
all_unique_pairings([Ai|Ar], Work) -> all_unique_pairings(Ar, [{Ai,Ari}||Ari<-Ar] ++ Work).





% used for side effects, doesn't gather results; appropriate for enormous lists

walk_unique_pairings([],    _) -> ok;
walk_unique_pairings([A|R], F) when is_function(F) ->
    walk_unique_pairings(A, R, F),
    walk_unique_pairings(R, F).

walk_unique_pairings(_A, [],     _F) -> ok;
walk_unique_pairings( A, [Rh|Rr], F) ->
    F(A,Rh),
    walk_unique_pairings(A, Rr, F).





list_to_num(X) ->
    case catch list_to_float(X) of
        {'EXIT',_} -> list_to_integer(X);
        Y -> Y
    end.






start_register_if_not_running(Name, Module, Function, Args) when is_atom(Name), is_atom(Module), is_atom(Function), is_list(Args) ->

    case whereis(Name) of
        undefined -> register(Name, spawn(Module, Function, Args));
        _         -> ok
    end.






counter(Name) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), get_counter, Name},
    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





inc_counter(Name) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), inc_counter, Name},
    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





reset_counter(Name) ->

    start_register_if_not_running(scutil_counter_process, scutil, counter_process, []),
    scutil_counter_process ! {self(), reset_counter, Name},
    receive
        {counter_at, Name, Val} -> Val
    after
        1000 -> {error, timeout}
    end.





counter_process() ->
    receive
        shutdown -> ok;
        {Caller, get_counter, Name} ->
            case get(Name) of
                undefined -> Caller ! {counter_at, Name, 0}, put(Name,0), counter_process();
                Defined   -> Caller ! {counter_at, Name, Defined},        counter_process()
            end;
        {Caller, inc_counter, Name} ->
            case get(Name) of
                undefined ->                Caller ! {counter_at, Name, 0},   put(Name,0),   counter_process();
                Defined   -> New=Defined+1, Caller ! {counter_at, Name, New}, put(Name,New), counter_process()
            end;
        {Caller, reset_counter, Name} ->
            Caller ! {counter_at, Name, 0},   
            put(Name,0),   
            counter_process()
    end.





% surprisingly useful in debugging

wait_until_terminate() -> wait_until_terminate(quiet).

wait_until_terminate(quiet) ->
    receive
        terminate -> ok;
        _         -> wait_until_terminate(quiet)
    end;

wait_until_terminate(loud) ->
    receive
        terminate -> ok;
        X         -> io:format("Received ~p~n", [X]), wait_until_terminate(loud)
    end.





module_has_function(Module, Function) ->

    scutil:deprecate("module_has_function() is deprecated in favor of erlang:function_exported/3"),
    lists:keymember(Function, 1, apply(Module, module_info, [exports])).





regexp_read_matches(String, Reg)                          -> regexp_read_matches(String, Reg, {0,0}).
regexp_read_matches(String, Reg, {TrimFront, TrimLength}) ->

    case regexp:matches(String, Reg) of
        { match, [] }      -> no_match;
        { match, Matches } -> [ string:substr(String, Start+TrimFront, End-TrimLength) || {Start,End} <- Matches ];
        { error, E }       -> { error, E }
    end.



% Derived from code originally found at http://wiki.trapexit.org/index.php/RandomShuffle
% they had a bizarre log length repeating behavior; I stripped it because it doesn't increase randomness in any significant way
% the new list position is not in any way dependant on the previous list position, so each time it's shuffled() it's started over
% to repeat the behavior is a study in cluelessness

shuffle(List) ->
   WeightedAndShuffled        = lists:map( fun(Item) -> { random:uniform(), Item } end, List ),
   { _, SortedAndDeweighted } = lists:unzip(lists:keysort(1, WeightedAndShuffled)),
   SortedAndDeweighted.





% Handler must be no_handler_pid or { handler, PID [, idhandle] }

call_after_worker(MS, Func, Args, Handler) ->

    receive after MS ->

        case Func of
            { Module, FuncName } -> Result = apply(Module, FuncName, Args);
            FuncName             -> Result = apply(FuncName, Args)
        end,

        case Handler of
            { handler, PID, Handle } -> PID ! { call_after_result, Result, Handle };
            { handler, PID }         -> PID ! { call_after_result, Result };
            no_handler_pid           -> ok
        end

    end.





call_after(Length, Func)                -> call_after(Length, Func, [],   no_handler_pid).
call_after(Length, Func, Args)          -> call_after(Length, Func, Args, no_handler_pid).
call_after(Length, Func, Args, Handler) ->

    Worker = spawn(?MODULE, call_after_worker, [Length, Func, Args, Handler]),
    { ok, spawned_worker, Worker }.





%  Thanks Table:
%
%    Alain O'Dea                   http://concise-software.blogspot.com/
%    Ayrnieu
%    Bryon Vandiver / Asterick     http://sublab.net/
%    Chile
%    DizzyD
%    Dylan Barrie / PfhorSlayer
%    Etnt
%    GrizzlyAdams                  http://grizzly.thewaffleiron.net/
%    Jeff Katz / Kraln             http://kraln.com/
%    John Sensebe
%    Raleigh
%    Toby Opferman                 http://www.opferman.com/
