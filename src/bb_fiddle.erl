-module(bb_fiddle).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         start/0, stop/0,
         set_pattern/1,
         set_bpm/1,
         set_pitch/1,
         set_instrument/1
         %% something something knobs
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RNAME, bb_fiddle_state).
-define(S, #?RNAME).

-record(?RNAME, {
           pattern = [e, oe, e, oe, q, oq] :: [atom()],
           compiled_pat :: bb_pattern:pattern(),
           instrument = <<>> :: binary(),
           bpm = 140 :: integer(),
           pitch = 120.0 :: float(),
           start_time = t() :: integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:call(?MODULE, start).

stop() ->
    gen_server:call(?MODULE, stop).

set_pattern(Pattern) ->
    gen_server:call(?MODULE, {set_pattern, Pattern}).

set_bpm(BPM) ->
    gen_server:call(?MODULE, {set_bpm, BPM}).

set_pitch(Pitch) ->
    gen_server:call(?MODULE, {set_pitch, Pitch}).

set_instrument(Instrument) ->
    gen_server:call(?MODULE, {set_instrument, Instrument}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    self() ! init,
    {ok, ?S{}}.

handle_call({set_pattern, Pat}, _From, S) ->
    {ok, CP} = bb_pattern:compile(Pat, S?S.instrument, S?S.pitch),
    {reply, ok, S?S{compiled_pat = CP, pattern = Pat}};
handle_call({set_bpm, BPM}, _From, S) ->
    {reply, ok, S?S{bpm = BPM}};
handle_call({set_pitch, Pitch}, _From, S) ->
    {ok, CP1} = bb_pattern:compile(S?S.pattern, S?S.instrument, Pitch),
    {reply, ok, S?S{pitch = Pitch, compiled_pat = CP1}};
handle_call({set_instrument, Instrument}, _From, S) ->
    {ok, CP1} = bb_pattern:compile(S?S.pattern, Instrument, S?S.pitch),
    {reply, ok, S?S{compiled_pat = CP1, instrument = Instrument}};
handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info(init, ?S{pattern = Pat, bpm = BPM,
                     pitch = P, instrument = I} = S) ->
    {ok, CP} = bb_pattern:compile(Pat, I, P),
    Duration = bb_pattern:duration(CP, BPM),
    lager:debug("pattern duration: ~p", [Duration]),
    erlang:send_after(Duration, self(),
                      {pattern_start, t(), Duration},
                      []),
    bb_pattern:schedule(CP, BPM, self()),
    {noreply, S?S{compiled_pat = CP}};
handle_info({pattern_start, StartTime, PatDuration},
            ?S{compiled_pat = P, bpm = BPM} = S) ->
    Now = t(),
    %% adjust this for tiny amounts of drift
    Duration = bb_pattern:duration(P, BPM),
    Adjust = (Now - StartTime) - PatDuration,
    lager:debug("pattern duration: ~p", [Duration - Adjust]),
    erlang:send_after(Duration - Adjust, self(),
                      {pattern_start, Now, Duration},
                      []),
    bb_pattern:schedule(P, BPM, self()),
    {noreply, S};
%% this is not flexible enough, really
handle_info({note, Instrument, Pitch, Duration}, S) ->
    bbsynth:play_note(Instrument, Pitch, Duration),
    {noreply, S};
handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

t() ->
    erlang:monotonic_time(milli_seconds).
