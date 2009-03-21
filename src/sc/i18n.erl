




% count_words(Source) -> count_words(Source, keep_hyphens).
%
% count_words(Source, keep_hyphens) -> count_words(Source, keep_hyphens, 0, not_word).
%
% count_words([],    _Hyphens,      Count,_WordState) -> Count;
% count_words(Source, keep_hyphens, Count, not_word)  ->





%% @since Version 219

is_numeric_char(Ch) -> is_numeric_char(Ch, decimal).

is_numeric_char(Ch, decimal) when $0 =< Ch, Ch =< $9; Ch == $-; Ch == $. -> true;
is_numeric_char(_, _)                                                    -> false.
