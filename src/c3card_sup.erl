%%%-------------------------------------------------------------------
%% @doc `c3card' top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_SDA_PIN, 2).
-define(DEFAULT_SCL_PIN, 3).

-define(INTENSITY, 10).
-define(PERIOD, 1_000).

%% API

-spec start_link(Config :: c3card_config:config()) -> supervisor:startlink_ret().
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%% supervisor callbacks

%% @private
init({Config, I2CBus}) ->
    ChildSpecs =
        [
         worker(c3card_sensors, Config, [{i2c_bus, I2CBus}]),
         worker(c3card_buttons, Config, []),
         worker(c3card_neopixel, Config, []),
         worker(c3card_screen, Config, [{i2c_bus, I2CBus}]),
         worker(c3card_status, Config, [])
        ],

    SupFlags = {one_for_one, ?INTENSITY, ?PERIOD},
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

worker(Mod, Config, Opts) ->
    Args = maps:to_list(maps:get(Mod, Config)),
    {Mod, {Mod, start_link, [Args ++ Opts]},
     permanent, brutal_kill, worker, [Mod]}.
