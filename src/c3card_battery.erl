-module(c3card_battery).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
    current_state/0,
    start_link/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(DEFAULT_VBATTERY_PIN, 0).

%% 72K Ohms
-define(R1, 72_000).
%% 102K Ohms
-define(R2, 102_000).

-define(SAMPLES, 64).

-define(SERVER, ?MODULE).

%% API

current_state() ->
    gen_server:call(?SERVER, current_state).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(_Config) ->
    ADCOpts = [
        {pin, ?DEFAULT_VBATTERY_PIN},
        {bitwidth, bit_max},
        %% 0 mV ~ 2500 mV with ADC_11db
        {attenuation, db_11}
    ],
    ADC = adc:open(ADCOpts),
    ?LOG_NOTICE("starting battery status helper"),
    {ok, #{adc => ADC}}.

handle_call(current_state, _From, State) ->
    #{adc := ADC} = State,
    ReadingOpts = [{samples, ?SAMPLES}, {raw, true}, {voltage, true}],
    {ok, _BattRaw, BattVoltage} = adc:take_reading(ADC, ReadingOpts),
    Reply = #{
        voltage => BattVoltage,
        level => calculate_battery_level(BattVoltage)
    },
    ?LOG_NOTICE("battery status: ~p", [Reply]),
    {reply, Reply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

%% Internal functions

calculate_battery_level(Voltage) ->
    (Voltage / 1_000.0) * ((?R1 + ?R2) / ?R2).
