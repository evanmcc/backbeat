-module(bbsynth).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         terminate/0,
         play_note/3,
         load_wav/4,
         twiddle/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          nif_resource :: term(),
          origin :: erlang:timestamp()
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

terminate() ->
    gen_server:call(?MODULE, terminate).

play_note(Instrument, Pitch, Duration) ->
    gen_server:call(?MODULE, {play_note, Instrument, Pitch, Duration}).

load_wav(Name, Channels, Duration, Data) ->
    gen_server:call(?MODULE, {load_wav, Name, Channels, Duration, Data}).

twiddle(Name, Value) ->
    gen_server:call(?MODULE, {twiddle, Name, Value}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    PrivDir = code:priv_dir(backbeat),
    ok = erlang:load_nif(filename:join(PrivDir, "libbbsynth"), 0),
    Resource = init_resources(),
    {ok, #state{nif_resource = Resource,
                origin = erlang:monotonic_time(micro_seconds)}}.

handle_call(terminate, _From, #state{nif_resource = Res}) ->
    terminate(Res),
    {stop, normal, ok, #state{}};
handle_call({play_note, Instrument, Pitch, Duration}, _From,
            #state{nif_resource = Res, origin = Origin} = S) ->
    Start = (erlang:monotonic_time(micro_seconds) - Origin) *
        (44100 / 1000000),
    Ret = play_note(Res,  Instrument, trunc(Start), Pitch, Duration),
    {reply, Ret, S};
handle_call({load_wav, Name, Channels, Duration, Data}, _From,
            #state{nif_resource = Res} = S) ->
    Ret = load_wav(Res, list_to_binary(Name), Channels, Duration, Data),
    {reply, Ret, S};
handle_call({twiddle, Name, Value}, _From,
            #state{nif_resource = Res} = S) ->
    Ret = twiddle(Res, iolist_to_binary(Name), Value),
    {reply, {ok, Ret}, S};

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{nif_resource = Res}) ->
    terminate(Res),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_resources() ->
    erlang:nif_error("nif not loaded").

terminate(_Resource) ->
    erlang:nif_error("nif not loaded").

play_note(_Res, _Instrument, _Start, _Pitch, _Duration) ->
    erlang:nif_error("nif not loaded").

load_wav(_Res, _Name, _Channels, _Duration, _Data) ->
    erlang:nif_error("nif not loaded").

twiddle(_Res, _Name, _Value) ->
    erlang:nif_error("nif not loaded").
