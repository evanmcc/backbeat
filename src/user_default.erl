-module(user_default).

-compile(export_all).

fiddle() ->
    {ok, _Pid} = bb_fiddle:start_link(),
    ok.

pitch(F) when is_float(F) ->
    bb_fiddle:set_pitch(F).

bpm(F) when is_float(F) ->
    bb_fiddle:set_bpm(F).

sweep(Knob, Start, End, Step, Time) ->
    spawn(fun() ->
                  Steps = trunc((End - Start) / Step),
                  TimeMs = trunc(Time * 1000),
                  StepDelay = trunc(TimeMs / Steps),
                  [begin
                       bbsynth:twiddle(Knob, Val * 1.0),
                       timer:sleep(StepDelay)
                   end || Val <- lists:seq(Start, End, Step)]
          end).
