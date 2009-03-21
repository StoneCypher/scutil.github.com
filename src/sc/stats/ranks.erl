




%% @type ranking() = { Ranking::number(), Value::any() }.  Values are usually {@type number()}s, but do not have to be with custom ranking predicates.
%% @type rankinglist() = list().  Members of a {@type rankinglist()} must be {@type ranking()}s.

%% @todo comeback make a ranks_of/2 which takes a sorting predicate
%% @spec ranks_of(Values::numericlist()) -> rankinglist()

%% @doc {@section Statistics} Returns a ranked ordering of the list without tie rankings.  ```1> scutil:ranks_of([10,90,20,80,30,70,40,60,50]).
%% [{1,90}, {2,80}, {3,70}, {4,60}, {5,50}, {6,40}, {7,30}, {8,20}, {9,10}]
%%
%% 2> scutil:ranks_of([10,10,10,10]).
%% [{1,10},{2,10},{3,10},{4,10}]'''

%% @since Version 42

ranks_of(List) when is_list(List) ->

    lists:zip(lists:seq(1,length(List)),lists:reverse(lists:sort(List))).





%% @todo comeback make a tied_ranks_of/2 which takes a sorting predicate
% needs significant refactoring; work is being repeated

%% @spec tied_ranks_of(Values::numericlist()) -> rankinglist()

%% @doc {@section Statistics} Returns a ranked ordering of the list with tie rankings.  As such, for uniformity, all rankings are floats.  Ties are represented as the centers of ranges. ```1> scutil:tied_ranks_of([10,90,20,80,30,70,40,60,50]).
%% [{1.0,90}, {2.0,80}, {3.0,70}, {4.0,60}, {5.0,50}, {6.0,40}, {7.0,30}, {8.0,20}, {9.0,10}]
%%
%% 2> scutil:tied_ranks_of([100,200,200,300]).
%% [{1.0,300},{2.5,200},{2.5,200},{4.0,100}]'''

%% @since Version 42

tied_ranks_of(List) -> 

    tied_rank_worker(ranks_of(List), [], no_prev_value).
    
    



%% @private

tied_add_prev(Work, {FoundAt, NewValue}) ->

    lists:duplicate( length(FoundAt), {lists:sum(FoundAt)/length(FoundAt), NewValue} ) ++ Work.





%% @private

tied_rank_worker([], Work, PrevValue) -> 

    lists:reverse(tied_add_prev(Work, PrevValue));
    
    

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





%% @todo comeback make a tied_ranks_of/2 which takes a sorting predicate

%% @spec ordered_ranks_of(Values::numericlist()) -> rankinglist()

%% @doc {@section Statistics} Returns a tied ranked ordering of the list, ordered according to the input ordering rather than the sorted ordering.  As with {@link tied_ranks_of/1}, all rankings are floats, and ties are represented as the centers of ranges. ```1> scutil:ordered_ranks_of([10,90,20,80,30,70,40,60,50]).
%% [{9.0,10}, {1.0,90}, {8.0,20}, {2.0,80}, {7.0,30}, {3.0,70}, {6.0,40}, {4.0,60}, {5.0,50}]
%%
%% 2> scutil:ordered_ranks_of([100,200,200,300]).
%% [{4.0,100},{2.5,200},{2.5,200},{1.0,300}]'''

%% @since Version 42

ordered_ranks_of(List) when is_list(List) ->

    ordered_ranks_of(List, tied_ranks_of(List), []).



ordered_ranks_of([], [], Work) ->

    lists:reverse(Work);



ordered_ranks_of([Front|Rem], Ranks, Work) ->

    {value,Item} = lists:keysearch(Front,2,Ranks),
    {IRank,Front} = Item,
    ordered_ranks_of(Rem, Ranks--[Item], [{IRank,Front}]++Work).
