




%% @spec bayes_likelihood_of(Event, Given, Data) -> float

%% @doc {@section Probability} <span style="color:red">TODO</span> Calculates the probability of a hypothetical event in the context of a dataset and a baseline given item, using Bayesian inference.  Bayesian inference sorts through the dataset looking for baselines, counting them; when a given is found, the hypothetical event is also looked for, counting them only when the baseline given is located first.  Then, the dividend of the hypothetical and given counts is returned as a likelihood estimation on the range `[0.0 .. 1.0]'.  ```1> scutil:bayes_likelihood_of(cancer, positive, [[cancer,positive],[healthy,negative],[cancer,positive],[healthy,positive]]).
%% 0.6666666666666666
%%
%% 2> TestData = lists:duplicate(40,[healthy,nonsmoker]) ++ lists:duplicate(10,[healthy,smoker]) ++ lists:duplicate(7,[cancer,nonsmoker]) ++ lists:duplicate(3,[cancer,smoker]).
%% [[healthy,nonsmoker], [healthy,nonsmoker], [healthy|...], [...]|...]
%%
%% 3> scutil:bayes_likelihood_of(cancer, smoker, TestData).
%% 0.23076923076923078
%%
%% 4> scutil:bayes_likelihood_of(cancer, nonsmoker, TestData).
%% 0.14893617021276595'''
%%
%% This code and example data was derived from <a href="http://www.ibm.com/developerworks/web/library/wa-bayes1/">this tutorial</a>.  <span style="color:red">Todo: it seems like this could be rewritten to generate a set of likelihoods in an iteration, rather than a single likelihood then re-iterate</span>

%% @since Version 110

bayes_likelihood_of(Event, Given, Data) -> 

    bayes_likelihood_worker(Event, Given, 0, 0, Data).
    


bayes_likelihood_worker(_Event,_Given, EventAndGivenCount, GivenCount, []) ->

    EventAndGivenCount / GivenCount;
    
    

bayes_likelihood_worker( Event, Given, EventAndGivenCount, GivenCount, [Data|Rem]) ->

    case lists:member(Given, Data) of

        true  ->
    
            case lists:member(Event, Data) of
    
                true -> 
                    bayes_likelihood_worker(Event, Given, EventAndGivenCount+1, GivenCount+1, Rem);

                false -> 
                    bayes_likelihood_worker(Event, Given, EventAndGivenCount,   GivenCount+1, Rem)

            end;

        false ->

            bayes_likelihood_worker(Event, Given, EventAndGivenCount, GivenCount, Rem)

    end.
