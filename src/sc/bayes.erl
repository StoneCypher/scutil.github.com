
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





-module(sc.bayes).

-author("John Haugeland <stonecypher@gmail.com>").
-webpage("http://scutil.com/").
-license( {mit_license, "http://scutil.com/license.html"} ).

-publicsvn("svn://crunchyd.com/scutil/").
-currentsource("http://crunchyd.com/release/scutil.zip").

-svn_id("$Id$").
-svn_head("$HeadURL$").
-svn_revision("$Revision$").

-description("Basic bayesian inference and bayesian analysis tools").

-testerl_export( { [], sc_bayes_testsuite } ).  % todo needs test suite

-library_requirements([
]).





-export( [
] ).





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
