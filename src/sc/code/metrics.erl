




%% @since Version 139

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands) ->

    halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, brief).



%% @since Version 139

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, brief) ->

    { Effort, _ } = halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, complete),
    Effort;



%% @since Version 139

halstead_complexity(DistinctOperators, DistinctOperands, TotalOperators, TotalOperands, complete) ->

    ProgramLength     = TotalOperators    + TotalOperands,
    ProgramVocabulary = DistinctOperators + DistinctOperands,

    Volume            = ProgramLength         * (math:log(ProgramVocabulary)),
    Difficulty        = (DistinctOperators/2) * (TotalOperands/DistinctOperands),

    Effort            = Volume * Difficulty,

    { Effort, [{volume, Volume}, {difficulty, Difficulty}, {program_length, ProgramLength}, {program_vocabulary, ProgramVocabulary}] }.
