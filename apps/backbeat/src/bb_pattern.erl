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
    L = length(P) - 1,
    NoteLen = beat_ms(BPM),
    _ = [begin
             Time = Pos * NoteLen,
             Duration = note_len(Note, BPM),
             erlang:send_after(Time, Target,
                               {note, Instrument, Pitch, Duration},
                               [])
         end
        || {{Note, Instrument, Pitch}, Pos} <- lists:zip(P, lists:seq(0, L))].

note_len(Note, BPM) when Note == s;
                         Note == os ->
    trunc(beat_sa(BPM) / 4);
note_len(Note, BPM) when Note == e;
                         Note == oe ->
    trunc(beat_sa(BPM) / 2);
note_len(Note, BPM) when Note == q;
                         Note == oq ->
    beat_sa(BPM);
note_len(Note, BPM) when Note == h;
                         Note == oh ->
    beat_sa(BPM) * 2;
note_len(Note, BPM) when Note == w;
                         Note == ow ->
    beat_sa(BPM) * 4.

beat_sa(BPM) ->
    BPS = BPM / 60,
    trunc(44100 / BPS).

beat_ms(BPM) ->
    BPS = BPM / 60,
    trunc(1000 / BPS).
