
from sc_lists

%% @todo TODO

% key_split(KeyId, TupleList)           when is_list(TupleList) -> key_split(KeyId, TupleList,                       unsorted).
% key_split(KeyId, TupleList, unsorted) when is_list(TupleList) -> key_split(KeyId, lists:keysort(KeyId, TupleList), sorted);
% key_split(KeyId, TupleList, sorted)   when is_list(TupleList) ->

% key_minimum(
% key_maximum(

% todo invert this so that it returns {currentcount, fun, result} so that it can be continued
% generate(0, _) -> [];
% generate(N, Fun) when is_integer(N) andalso N > 0 andalso is_function(Fun) -> [Fun()] ++ generate(N-1,Fun).

http://en.wikipedia.org/wiki/Naive_Bayes_classifier
http://en.wikipedia.org/wiki/Boosted_trees#Gradient_tree_boosting
http://en.wikipedia.org/wiki/Decision_tree
http://en.wikipedia.org/wiki/Classification_and_regression_tree
http://en.wikipedia.org/wiki/Random_forests
http://www.ibm.com/developerworks/web/library/wa-bayes1/

http://en.wikipedia.org/wiki/Approval_voting
http://en.wikipedia.org/wiki/Range_voting
http://en.wikipedia.org/wiki/Plurality_voting_system
http://en.wikipedia.org/wiki/Burr_dilemma
http://en.wikipedia.org/wiki/Copeland%27s_method
http://en.wikipedia.org/wiki/Kemeny-Young_method
http://en.wikipedia.org/wiki/Preferential_ballot
http://en.wikipedia.org/wiki/Single_transferable_vote
http://en.wikipedia.org/wiki/Instant-runoff_voting
http://en.wikipedia.org/wiki/Minimax_Condorcet
http://en.wikipedia.org/wiki/Nanson%27s_method
http://en.wikipedia.org/wiki/Ranked_pairs
http://en.wikipedia.org/wiki/Schulze_method
http://en.wikipedia.org/wiki/Borda_count
http://en.wikipedia.org/wiki/Plurality_voting
http://en.wikipedia.org/wiki/Bucklin_voting
http://en.wikipedia.org/wiki/Preferential_voting
http://en.wikipedia.org/wiki/Category:Voting_system_criteria
http://en.wikipedia.org/wiki/Coombs%27_method
http://en.wikipedia.org/wiki/Contingent_vote
http://en.wikipedia.org/wiki/Independence_of_irrelevant_alternatives
http://en.wikipedia.org/wiki/Majority_criterion
http://en.wikipedia.org/wiki/Arrow%27s_impossibility_theorem
http://en.wikipedia.org/wiki/Later-no-harm_criterion
http://en.wikipedia.org/wiki/Condorcet_winner
http://en.wikipedia.org/wiki/Condorcet_loser

http://reference.wolfram.com/mathematica/tutorial/DescriptiveStatistics.html
http://reference.wolfram.com/mathematica/guide/ProbabilityAndStatistics.html





%% @spec instant_runoff_vote(ListOfVoteLists::list_of_lists()) -> any()

%% @doc <span style="color:red;font-style:italic">INCOMPLETE</span> <span style="color:orange;font-style:italic">Untested</span> Performs an instant runoff vote.  http://en.wikipedia.org/wiki/Instant-runoff_voting ```1>'''

%% @since Version 485

%% not complete todo comeback
%% also do other voting types

instant_runoff_vote(ListOfVoteLists) ->

    instant_runoff_vote(ListOfVoteLists, []).





instant_runoff_vote(ListOfVoteLists, Exclude) ->

    FilteredVotes = [ VoteList || VoteList <- ListOfVoteLists, VoteList =/= [] ],
    { Exclude, FilteredVotes }.









% in the context of string explode and implode

%% mapsplode( % todo      [ Func(X) || X <- explode(Delim, Source) ]
%% exp_map_imp( % todo    implode(Delim, [Func(X) || X <- explode(Delim, Source) ] )    % maybe called imp_mapsplode?
%% quotesplode(           ("a,b,'c,d',e","'") -> ["a","b","'c,d'","e"]
