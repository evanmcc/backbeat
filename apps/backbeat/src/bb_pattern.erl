-module(bb_pattern).

-export([
         duration/2,
         compile/3,
         schedule/3
        ]).

-record(pattern,
        {
          orig,
          pat
        }).

-opaque pattern() :: #pattern{}.

-export_type([pattern/0]).

%% at some point we should accept arpeggios here in addition to a
%% static pitch.
compile(Pattern, Instrument, Pitch) ->
    case length(Pattern) of
        %% N when N == 4,
        %%        N == 8,
        %%        N == 16,
        %%        N == 32 ->
        N when is_integer(N) ->
            L = length(Pattern),
            {ok, #pattern{ orig = Pattern,
                           pat = lists:zip3(Pattern,
                                            lists:duplicate(L, Instrument),
                                            lists:duplicate(L, Pitch))
                         }};
        _ ->
            {error, invalid_pattern}
    end.

%% return is in samples
duration(#pattern{}, BPM) ->
    beat_ms(BPM) * 4.

schedule(#pattern{pat = P}, BPM, Target) ->
    schedule(P, BPM, Target, 0).

schedule([], _, _, _) ->
    ok;
schedule([{Note, Instrument, Pitch}|T], BPM, Target, Time) ->
    {Ms, Samples} = note_len(Note, BPM),
    lager:debug("time=~p, duration=~p", [Time, Samples]),
    case is_rest(Note) of
        true ->
            ok;
        _ ->
            erlang:send_after(Time, Target,
                              {note, Instrument, Pitch, Samples},
                              [])
    end,
    schedule(T, BPM, Target, Time + Ms).



note_len(Note, BPM) when Note == s;
                         Note == os ->
    {trunc(beat_ms(BPM) / 4),
     trunc(beat_sa(BPM) / 4)};
note_len(Note, BPM) when Note == e;
                         Note == oe ->
    {trunc(beat_ms(BPM) / 2),
     trunc(beat_sa(BPM) / 2)};
note_len(Note, BPM) when Note == q;
                         Note == oq ->
    {beat_ms(BPM),
     beat_sa(BPM)};
note_len(Note, BPM) when Note == h;
                         Note == oh ->
    {beat_ms(BPM) * 2,
     beat_sa(BPM) * 2};
note_len(Note, BPM) when Note == w;
                         Note == ow ->
    {beat_ms(BPM) * 4,
     beat_sa(BPM) * 4}.

beat_sa(BPM) ->
    BPS = BPM / 60,
    trunc(44100 / BPS).

beat_ms(BPM) ->
    BPS = BPM / 60,
    trunc(1000 / BPS).

is_rest(o) ->
    true;
is_rest(os) ->
    true;
is_rest(oe) ->
    true;
is_rest(oq) ->
    true;
is_rest(oh) ->
    true;
is_rest(ow) ->
    true;
is_rest(_) ->
    false.

