




%% @since Version 129

unit_scale_signal(Waveform) ->

    { Baseline, MaxObserved } = minmax(Waveform),
    SignalMax                 = MaxObserved - Baseline,

    [ (Sample - Baseline) / SignalMax || 
        Sample <- Waveform
    ].





%% @since Version 129

isolate_signal(Waveform) ->

    Baseline = lists:min(Waveform),

    [ Sample - Baseline ||
        Sample <- Waveform
    ].
