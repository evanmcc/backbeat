-module(bb_wav).

-export([
         load_sounds/0, load_sounds/1
        ]).

-define(default_path, "sound_data/").

load_sounds() ->
    load_sounds(?default_path).

load_sounds(Path) ->
    _ = filelib:fold_files(
          Path, ".*wav", true,
          fun(F, _) ->
                  {ok, Bin} = file:read_file(F),
                  {Channels, Data, Duration} = load_wav(Bin),
                  Base = filename:rootname(filename:basename(F)),
                  ok = bbsynth:load_wav(Base, Channels, Duration, Data),
                  ignored
          end,
          ignored).

load_wav(BinFile) ->
    TotalSize = size(BinFile),
    <<"RIFF", RIFFSize:32/little-unsigned-integer, Rest/binary>> =
        BinFile,
    TotalSize = RIFFSize + 8,
    <<"WAVE", "fmt ", Sub1Size:32/little-unsigned-integer, Rest1/binary>> =
        Rest,
    Sub1Size = 16, % we assume PCM for now
    <<Format:16/little-unsigned-integer,
      Channels:16/little-unsigned-integer,
      SampleRate:32/little-unsigned-integer,
      _ByteRate:32/little-unsigned-integer,
      _BlockAlign:16/little-unsigned-integer,
      SampleBits:16/little-unsigned-integer,
      Rest2/binary>> = Rest1,
    %% some assertions
    Format = 1,
    SampleRate = 44100,
    SampleBits = 16,
    true = Channels == 1 orelse Channels == 2,
    %% now extract the actual data
    <<"data", DataSize:32/little-unsigned-integer,
      Data:DataSize/bytes>> = Rest2,
    %% this would be more complicated if we allowed different sample
    %% rates or bit counts.
    Duration = DataSize div 2,
    {Channels, Data, Duration}.
