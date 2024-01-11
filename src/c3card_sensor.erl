%%%-------------------------------------------------------------------
%% @doc workshop_sensor public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_sensor).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([read_sensors/0,
	 start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-define(SERVER, ?MODULE).

%% API

read_sensors() ->
    gen_server:call(?SERVER, read_sensors).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(Config) ->
    SDA = proplists:get_value(sda, Config),
    SCL = proplists:get_value(scl, Config),
    {ok, I2CBus} = i2c_bus:start(#{sda => SDA, scl => SCL}),
    Sensors = lists:map(fun({Mod, Fun, Opts}) ->
				{ok, Pid} = Mod:Fun(I2CBus, Opts),
				{Mod, Pid}
			end, proplists:get_value(sensors, Config)),
    ?LOG_NOTICE("starting sensors: ~p", [Sensors]),
    {ok, #{i2c_bus => I2CBus, sensors => Sensors}}.

handle_call(read_sensors, _From, #{sensors := Sensors} = State) ->
    Readings0 = lists:map(fun({Mod, Pid}) ->
				 case Mod:take_reading(Pid) of
				     {ok, Reading} -> {Mod, Reading};
				     {error, Reason} -> {Mod, Reason}
				 end
			 end, Sensors),
    Readings = maps:map(fun(Sensor, Reading) ->
					    deaggregate_reading(Sensor, Reading)
				    end, maps:from_list(Readings0)),
    {reply, {ok, Readings}, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

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
deaggregate_reading(_Sensor, Error) ->
    [#{error => Error}].
