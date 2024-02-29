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
    loop(#{sleep_ms => 50}).

%% Internal functions

%% @hidden
loop(State) ->
    #{sleep_ms := SleepMs} = State,
    {ok, Buttons} = c3card_buttons:button_status(),
    handle_buttons(Buttons),
    _ = timer:sleep(SleepMs),
    loop(State).

%% @hidden
handle_buttons(#{1 := low}) ->
    c3card_neopixel:toggle_led(0, 350),
    c3card_neopixel:clear_all(),
    c3card_screen:next_screen();
handle_buttons(#{4 := low}) ->
    c3card_gateway:reconnect();
handle_buttons(#{2 := low}) ->
    c3card_workshop:request_candy();
handle_buttons(_Buttons) ->
    ok.
