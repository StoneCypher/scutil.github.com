
-module(sc.string).

-export( [] ).





%% @since Version 220

starts_with(Remain, [])     -> { true, Remain };
starts_with([],     _)      -> false;
starts_with(Main,   Prefix) ->

    [ MHead | MRemain ] = Main,
    [ PHead | PRemain ] = Prefix,

    if
        PHead /= MHead -> false;
        true           -> starts_with(MRemain, PRemain)
    end.





%% @since Version 219

is_numeric_string(Str)          -> is_numeric_string(Str, decimal).
is_numeric_string(Str, decimal) -> lists:all({scutil,is_numeric_char}, Str).





%% @since Version 220

explode(Separator, Term) ->

    explode(Separator, Term, [], [], -1,  0).



%% @since Version 220

explode(Separator, Term, Max) ->

    explode(Separator, Term, [], [], Max, 0).



%% @private
%% @since Version 220

explode(_Separator, [], Pass, Out, _Max, _Cur) ->

    Out ++ [Pass];



%% @private
%% @since Version 220

explode(Separator, Remainder, Pass, Out, -1, _Cur) -> % ignore cap

    case starts_with(Remainder, Separator) of

        false ->

            [ThisChar | Following] = Remainder,
            explode(Separator, Following, Pass ++ [ThisChar], Out, -1, 0);

        { true, LeftOver } ->

            explode(Separator, LeftOver, [], Out ++ [Pass], -1, 0)

    end;



%% @private
%% @since Version 220

explode(Separator, Remainder, Pass, Out, Max, Cur) -> % check cap

    if
        Cur+1 >= Max ->
            Out ++ [Remainder];

        true ->

            case starts_with(Remainder, Separator) of

                false ->

                    [ThisChar | Following] = Remainder,
                    explode(Separator, Following, Pass ++ [ThisChar], Out, Max, Cur);

                { true, LeftOver } ->

                    explode(Separator, LeftOver, [], Out ++ [Pass], Max, Cur+1)

            end

    end.





% parses a string on all three newline types, discarding any empty lines; applies F as a functor to each line,
% and returns the tuple of the remainder and then a list of all results from the functor(s) issued
% thanks ayrnieu

%% @since Version 214

map_scanline(F,L) ->

    {R,M} = lists:foldl(
        fun
            ( C, R = {[],_} ) when C == $\r orelse C == $\n -> R;
            ( C, {S,M}      ) when C == $\r orelse C == $\n -> { [], [ F(lists:reverse(S)) | M ] };
            ( C, {S,M}      )                               -> { [C|S], M }
        end,
        {[],[]}, L
    ),

    {lists:reverse(R), lists:reverse(M)}.





% third argument passes argument list as secondary argument to the functor, useful for passing ancillary state
% modified from map_scaline/2 by ayrnieu

%% @since Version 214

map_scanline(F,L,A) ->

    {R,M} = lists:foldl(
        fun
            ( C, R = {[],_} ) when C == $\r orelse C == $\n -> R;
            ( C, {S,M}      ) when C == $\r orelse C == $\n -> { [], [ F(lists:reverse(S), A) | M ] };
            ( C, {S,M}      )                               -> { [C|S], M }
        end,
        {[],[]}, L
    ),

    {lists:reverse(R), lists:reverse(M)}.





% by fredrik svensson and adam lindberg, from http://www.merriampark.com/lderlang.htm

%% @since Version 202

levenshtein(Same, Same) when is_list(Same) ->

    0;



levenshtein(String, []) when is_list(String) ->

    length(String);



levenshtein( [], String) when is_list(String) -> 

    length(String);



levenshtein(Source, Target) when is_list(Source), is_list(Target) -> 

    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).





% Recurses over every character in the source string and calculates a list of distances
%% @private

levenshtein_rec( [SrcHead|SrcTail], Target, DistList, Step) ->

    levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);



levenshtein_rec( [], _, DistList, _) ->

    lists:last(DistList).





% Generates a distance list with distance values for every character in the target string
%% @private

levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->

    Min = lists:min( [LastDist + 1, hd(DLT) + 1, DLH + lev_dif(TargetHead, SourceChar)] ),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);



levenshtein_distlist([], _, _, NewDistList, _) ->

    NewDistList.



% Calculates the difference between two characters or other values
%% @private

lev_dif( C,   C ) -> 0;
lev_dif(_C1, _C2) -> 1.





%% mapsplode( % todo      [ Func(X) || X <- explode(Delim, Source) ]
%% exp_map_imp( % todo    implode(Delim, [Func(X) || X <- explode(Delim, Source) ] )    % maybe called imp_mapsplode?
%% quotesplode(           ("a,b,'c,d',e","'") -> ["a","b","'c,d'","e"]
