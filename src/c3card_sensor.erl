%%%-------------------------------------------------------------------
%% @doc workshop_sensor public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_sensor).

-include_lib("kernel/include/logger.hrl").

-include("config.hrl").

-behaviour(gen_server).

-export([read_sensor/0,
	 start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-define(SERVER, ?MODULE).

%% API

read_sensor() ->
    gen_server:call(?SERVER, read_sensor).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(_Config) ->
    {ok, I2CBus} = i2c_bus:start(#{sda => ?DEFAULT_SDA_PIN,
				   scl => ?DEFAULT_SCL_PIN}),
    {ok, AHT} = aht20:start_link(I2CBus, []),
    ?LOG_DEBUG("starting sensor"),
    {ok, #{i2c_bus => I2CBus, aht => AHT}}.

handle_call(read_sensor, _From, #{aht := AHT} = State) ->
    {reply, aht20:take_reading(AHT), State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

%% internal functions
