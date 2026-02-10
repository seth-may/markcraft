%% Distributed key-value store with gen_server
-module(kv_store).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         get/1, put/2, delete/1,
         keys/0, size/0, dump/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

-record(state, {
    store = #{} :: map(),
    writes = 0  :: non_neg_integer(),
    reads = 0   :: non_neg_integer()
}).

-type key()   :: term().
-type value() :: term().

%%% API Functions
-spec start_link() -> {ok, pid()}.
start_link() -> start_link([]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec get(key()) -> {ok, value()} | {error, not_found}.
get(Key) -> gen_server:call(?MODULE, {get, Key}).

-spec put(key(), value()) -> ok.
put(Key, Value) -> gen_server:call(?MODULE, {put, Key, Value}).

-spec delete(key()) -> ok | {error, not_found}.
delete(Key) -> gen_server:call(?MODULE, {delete, Key}).

keys() -> gen_server:call(?MODULE, keys).
size() -> gen_server:call(?MODULE, size).
dump() -> gen_server:call(?MODULE, dump).

%%% gen_server Callbacks
init(_Opts) ->
    process_flag(trap_exit, true),
    io:format("[kv_store] Started on ~p~n", [node()]),
    {ok, #state{}}.

handle_call({get, Key}, _From, #state{store = S, reads = R} = State) ->
    case maps:find(Key, S) of
        {ok, Value} -> {reply, {ok, Value}, State#state{reads = R + 1}};
        error       -> {reply, {error, not_found}, State#state{reads = R + 1}}
    end;

handle_call({put, Key, Value}, _From, #state{store = S, writes = W} = State) ->
    NewStore = maps:put(Key, Value, S),
    {reply, ok, State#state{store = NewStore, writes = W + 1}};

handle_call({delete, Key}, _From, #state{store = S, writes = W} = State) ->
    case maps:is_key(Key, S) of
        true ->
            NewStore = maps:remove(Key, S),
            {reply, ok, State#state{store = NewStore, writes = W + 1}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call(keys, _From, #state{store = S} = State) ->
    {reply, maps:keys(S), State};

handle_call(size, _From, #state{store = S} = State) ->
    {reply, maps:size(S), State};

handle_call(dump, _From, #state{store = S, reads = R, writes = W} = State) ->
    Stats = #{entries => maps:size(S), reads => R, writes => W},
    {reply, {ok, Stats, S}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, #state{store = S}) ->
    io:format("[kv_store] Shutting down: ~p (~p entries)~n",
              [Reason, maps:size(S)]),
    ok.
