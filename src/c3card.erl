%%%-------------------------------------------------------------------
%% @doc `c3card' AtomVM entrypoint.
%% @end
%%%-------------------------------------------------------------------

-module(c3card).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_SDA_PIN, 2).
-define(DEFAULT_SCL_PIN, 3).

-export([start/0]).

%% AtomVM main function

%% @doc Main AtomVM entrypoint.
%%
%% This function will prepare configuration, logging and wireless
%% networking facilities before starting the application, and after
%% that will loop forever taking the device readings and dispatching
%% them to the gateway.
start() ->
    I2CBusConfig = #{
        sda => ?DEFAULT_SDA_PIN,
        scl => ?DEFAULT_SCL_PIN,
        clock_speed_hz => 1_000_000,
        peripheral => <<"i2c0">>,
        use_nif => true
    },

    {ok, I2CBus} = i2c_bus:start_link(I2CBusConfig),
    {ok, _} = logger_manager:start_link(#{}),

    case c3card_config:read_config() of
        undefined ->
            ScreenOpts = [
                {i2c_bus, I2CBus},
                {screen, {2, c3card_screen_provision}}
            ],
            {ok, _Pid} = c3card_screen:start_link(ScreenOpts),
            c3card_provision:start_provisioning(),
            ?LOG_NOTICE("starting initial provisioning..."),
            timer:sleep(infinity);
        Config ->
            WiFiConfig = maps:get(c3card_wifi, Config),
            {ok, {IP, _, _}} = c3card_wifi:start(WiFiConfig),
            {ok, _} = c3card_app:start(normal, [{config, {Config, I2CBus}}]),
            c3card_status:set_ip(IP),

            DeviceInfo = c3card_system:device_specs(),
            DeviceModel = <<"c3card">>,
            DeviceName = c3card_config:get_name(),
            CardAttrs =
                #{
                    <<"platform">> => atomvm:platform(),
                    <<"frequency">> => esp:freq_hz(),
                    <<"mac">> => c3card_config:get_mac(),
                    <<"firmware">> => <<"c3card">>,
                    <<"version">> => <<"v1.0.0">>,
                    <<"model">> => DeviceModel
                },
            ProvisionInfo = #{
                name => DeviceName,
                info => DeviceInfo,
                attributes => CardAttrs
            },
            c3card_mqtt:publish(<<"provision">>, ProvisionInfo),
            ?LOG_NOTICE("entering loop..."),
            loop(#{sleep_ms => 100})
    end.

%% Internal functions

%% @hidden
loop(State) ->
    #{sleep_ms := SleepMs} = State,
    {ok, Buttons} = c3card_buttons:button_status(),
    handle_buttons(Buttons),
    _ = timer:sleep(SleepMs),
    loop(State).

%% @hidden
handle_buttons(#{4 := low, 2 := low}) ->
    c3card_config:reset_config(),
    esp:restart();
handle_buttons(#{1 := low}) ->
    c3card_neopixel:toggle_led(3, 350),
    c3card_neopixel:clear_all();
handle_buttons(#{2 := low}) ->
    c3card_neopixel:toggle_led(2, 350),
    c3card_neopixel:clear_all();
handle_buttons(#{3 := low}) ->
    c3card_neopixel:toggle_led(1, 350),
    c3card_neopixel:clear_all();
handle_buttons(#{4 := low}) ->
    c3card_neopixel:toggle_led(0, 350),
    c3card_neopixel:clear_all();
handle_buttons(_Buttons) ->
    ok.
