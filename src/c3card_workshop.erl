%%%-------------------------------------------------------------------
%% @doc CodeBEAM workshop helpers.
%% @end
%%%-------------------------------------------------------------------
-module(c3card_workshop).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([request_candy/0,
         set_turn/1,
         candy_turn/0,
         start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(SERVER, ?MODULE).

-type config() :: [].
%% Default configuration for `c3card_workshop'

-export_type([config/0]).

%% API

%% @doc Request a candy from the dispenser
-spec request_candy() -> ok | {error, Reason :: term()}.
request_candy() ->
    gen_server:call(?SERVER, request_candy).

%% @doc Set the current candy turn
-spec set_turn(Turn :: non_neg_integer()) -> ok | {error, Reason :: term()}.
set_turn(Turn) when is_integer(Turn) ->
    gen_server:call(?SERVER, {set_turn, Turn}).

%% @doc Get the current candy turn
-spec candy_turn() -> non_neg_integer() | none | requested.
candy_turn() ->
    gen_server:call(?SERVER, candy_turn).

%% @doc Start and link CodeBEAM workshop helper server
-spec start_link(Config :: config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

%% @private
init(_Config) ->
    ?LOG_NOTICE("starting CodeBEAM workshop helpers"),
    {ok, #{turn => none}}.

%% @private
handle_call(request_candy, _From, State) ->
    Resp = c3card_gateway:send_data(#{command => request_candy}),
    ?LOG_INFO("requesting candy: ~p", [Resp]),
    {reply, Resp, State#{turn => requested}};
handle_call({set_turn, Turn}, _From, State) ->
    {reply, ok, State#{turn => Turn}};
handle_call(candy_turn, _From, State) ->
    {reply, maps:get(turn, State), State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Message, State) ->
    {noreply, State}.
