%%%-------------------------------------------------------------------
%% @doc `c3card' AtomVM entrypoint.
%% @end
%%%-------------------------------------------------------------------

-module(c3card).

-include_lib("kernel/include/logger.hrl").

-export([start/0]).

%% AtomVM main function

%% @doc Main AtomVM entrypoint.
%%
%% This function will prepare configuration, logging and wireless
%% networking facilities before starting the application, and after
%% that will loop forever taking the device readings and dispatching
%% them to the gateway.
start() ->
    Config = c3card_config:reset_config(),
    WiFiConfig = proplists:get_value(c3card_wifi, Config),

    {ok, _} = logger_manager:start_link(#{}),
    {ok, _} = c3card_wifi:start(WiFiConfig),
    {ok, _} = c3card_app:start(normal, []),

    ?LOG_NOTICE("entering loop..."),
    loop(#{sleep_ms => 50, screen => c3card_screen:default_screen()}).

%% Internal functions

%% @hidden
loop(State) ->
    #{sleep_ms := SleepMs, screen := CurrentScreen} = State,
    _ = timer:sleep(SleepMs),
    {ok, Readings} = c3card_sensor:read_sensors(),
    {ok, Buttons} = c3card_buttons:button_status(),
    CardInfo = #{readings => Readings,
                 platform => esp32c3,
                 screen => CurrentScreen,
                 system_info => c3card_system:info(),
                 control => c3card_comm:get_port(),
                 buttons => Buttons},
    NextScreen = btn_to_screen(Buttons, CurrentScreen),
    spawn(fun() -> handle_buttons(Buttons, NextScreen) end),
    case CurrentScreen == NextScreen of
        false -> c3card_screen:clear();
        true -> ok
    end,
    ok = c3card_screen:render_screen(NextScreen, CardInfo),
    spawn(fun() -> maybe_send_info(CardInfo) end),
    loop(State#{screen => NextScreen}).

%% @hidden
btn_to_screen(#{1 := low}, CurrentScreen) ->
    c3card_screen:switch_screen(CurrentScreen);
btn_to_screen(_Buttons, CurrentScreen) ->
    CurrentScreen.

%% @hidden
handle_buttons(#{1 := low}, _Screen) ->
    c3card_neopixel:toggle_led(0, 350),
    c3card_neopixel:clear_all();
handle_buttons(_Buttons, _Screen) ->
    ok.

%% @hidden
maybe_send_info(CardInfo) ->
    case c3card_data:send_data(CardInfo) of
        ok ->
	    c3card_neopixel:toggle_led(2, 200);
        {error, offline} ->
            c3card_neopixel:toggle_led(2, 50);
        Error ->
	    c3card_neopixel:toggle_led(2, 350),
            ?LOG_ERROR("error sending data: ~p", [Error])
    end,
    c3card_neopixel:clear_all().
