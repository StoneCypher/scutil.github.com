
% text facilities.  all are language independant, based on i18n.erl .





% http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test
% http://www.readabilityformulas.com/graphics/fleschresults.gif

%% @since Version 131

flesch_kincaid_readability_score(Words, Sentences, Syllables) ->

    206.835 - (1.015 * (Words/Sentences)) - (84.6 * (Syllables/Words)).





% http://en.wikipedia.org/wiki/Flesch-Kincaid_Readability_Test

%% @since Version 131

% todo
% flesch_kincaid_readability(Data) -> flesch_kincaid_readability(Data, fun count_words/1, fun count_sentences/1, fun count_syllables/1).

flesch_kincaid_readability(Data, WordCounter, SentenceCounter, SyllableCounter) ->

    Words     = WordCounter(Data),
    Sentences = SentenceCounter(Data),
    Syllables = SyllableCounter(Data),

    interpret_flesch_kincaid_score(
      flesch_kincaid_readability_score(Words, Sentences, Syllables)
    ).





%% @since Version 131

interpret_flesch_kincaid_score(R) when R > 100 -> { easy_before_11_years,     R };
interpret_flesch_kincaid_score(R) when R >  90 -> { easy_at_11_years,         R };
interpret_flesch_kincaid_score(R) when R >  70 -> { easy_for_11_to_13_years,  R };
interpret_flesch_kincaid_score(R) when R >  60 -> { easy_for_13_to_15_years,  R };
interpret_flesch_kincaid_score(R) when R >  30 -> { appropriate_for_15_years, R };
interpret_flesch_kincaid_score(R) when R >   0 -> { appropriate_for_college,  R };
interpret_flesch_kincaid_score(R)              -> { difficult,                R }.
