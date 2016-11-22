-module(user_default).

-compile(export_all).

fiddle() ->
    {ok, _Pid} = bb_fiddle:start_link(),
    ok.

pitch(F) when is_float(F) ->
    bb_fiddle:set_pitch(F).
