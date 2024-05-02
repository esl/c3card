%%%-------------------------------------------------------------------
%% @doc I²C based sensors public API.
%%
%% The `c3card' provides one I²C interfaces mapped to the following
%% pins:
%%
%% <ul>
%%   <li>`IO2': I2C SDA</li>
%%   <li>`IO3': I2C SCL</li>
%% </ul>
%%
%% It uses the `i2c_bus' provided by <a
%% href="https://github.com/atomvm/atomvm_lib">atomvm_lib</a> for
%% sharing access to the I²C interface provided by the ESP32C3
%% microcontroller.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_sensors).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
    read_sensors/0,
    internal_sensor/0,
    start_link/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

-type reading() ::
    #{
        type => atom(),
        data => number()
    }
    | #{error => Error :: term(), sensor => SensorName :: atom()}.
%% Reading format

-type readings() :: #{SensorName :: atom() => [reading()]}.
%% Recorded readings

-type sensors_option() ::
    {i2c_bus, I2CBus :: i2c_bus:i2c_bus()}
    | {sensors, [{Mod :: atom(), StartFun :: atom(), Args :: list()}]}.
%% Sensor server option

-type config() :: [sensors_option()].
%% Default configuration for `c3card_sensor'

-export_type([config/0, readings/0]).

%% API

%% @doc Return all available readings from available sensors
-spec read_sensors() -> {ok, readings()} | {error, Reason :: term()}.
read_sensors() ->
    gen_server:call(?SERVER, read_sensors).

%% @doc Return internal (AHT20) sensor reading
-spec internal_sensor() -> {ok, reading()} | {error, Reason :: term()}.
internal_sensor() ->
    gen_server:call(?SERVER, internal_sensor).

%% @doc Start and link the sensor facility.
%%
%% On startup, the server will try to start the drivers specified on
%% the configuration.
%%
%% The `c3card' includes an internal AHT20 sensor.
%% @end
-spec start_link(Config :: config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

%% @private
init(Config) ->
    I2CBus = proplists:get_value(i2c_bus, Config),
    Sensors = lists:map(
        fun({Mod, Fun, Args}) ->
            case Mod:Fun(I2CBus, Args) of
                {ok, Pid} -> {Mod, Pid};
                Error -> throw({stop, {c3card_sensors, Mod, Error}})
            end
        end,
        proplists:get_value(sensors, Config)
    ),
    ?LOG_NOTICE("starting sensors: ~p", [Sensors]),
    {ok, #{i2c_bus => I2CBus, sensors => Sensors}}.

%% @private
handle_call(internal_sensor, _From, #{sensors := Sensors} = State) ->
    Pid = proplists:get_value(aht20, Sensors),
    {ok, Reading} = aht20:take_reading(Pid),
    Reply = #{aht20 => deaggregate_reading(aht20, Reading)},
    {reply, {ok, Reply}, State};
handle_call(read_sensors, _From, #{sensors := Sensors} = State) ->
    Readings0 = lists:map(
        fun({Mod, Pid}) ->
            case Mod:take_reading(Pid) of
                {ok, Reading} -> {Mod, Reading};
                {error, Reason} -> {Mod, Reason}
            end
        end,
        Sensors
    ),
    Readings =
        maps:map(fun deaggregate_reading/2, maps:from_list(Readings0)),
    {reply, {ok, Readings}, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Message, State) ->
    {noreply, State}.

%% internal functions

deaggregate_reading(bme280, {Temp, Pressure, Hum}) ->
    [
        #{type => humidity, data => Hum},
        #{type => pressure, data => Pressure},
        #{type => temperature, data => Temp}
    ];
deaggregate_reading(aht20, {Hum, Temp, RH}) ->
    [
        #{type => humidity, data => Hum},
        #{type => relative_humidity, data => RH},
        #{type => temperature, data => Temp}
    ];
deaggregate_reading(scd40, {CO2, Temp, Hum}) ->
    [
        #{type => carbon_dioxide, data => CO2},
        #{type => temperature, data => Temp},
        #{type => humidity, data => Hum}
    ];
deaggregate_reading(Sensor, Error) ->
    [#{sensor => Sensor, error => Error}].
