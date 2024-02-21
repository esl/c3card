%%%-------------------------------------------------------------------
%% @doc `c3card' top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_SDA_PIN, 2).
-define(DEFAULT_SCL_PIN, 3).

-define(INTENSITY, 10).
-define(PERIOD, 1_000).

%% API

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor callbacks

%% @private
init([]) ->
    Config = c3card_config:read_config(),
    I2CBusConfig = #{sda => ?DEFAULT_SDA_PIN,
		     scl => ?DEFAULT_SCL_PIN,
		     peripheral => <<"i2c0">>},

    {ok, I2CBus} = i2c_bus:start_link(I2CBusConfig),

    ChildSpecs =
	[
	 worker(c3card_buttons, Config, []),
	 worker(c3card_neopixel, Config, []),
	 worker(c3card_screen, Config, [{i2c_bus, I2CBus}]),
	 worker(c3card_data, Config, []),
	 worker(c3card_comm, Config, []),
	 worker(c3card_sensor, Config, [{i2c_bus, I2CBus}])
	],

    SupFlags = {one_for_one, ?INTENSITY, ?PERIOD},
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

worker(Mod, Config, Opts) ->
    Args = proplists:get_value(Mod, Config),
    {Mod, {Mod, start_link, [Args ++ Opts]},
     permanent, brutal_kill, worker, [Mod]}.
