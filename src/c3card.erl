%%%-------------------------------------------------------------------
%% @doc C3 Card AtomVM entrypoint
%% @end
%%%-------------------------------------------------------------------

-module(c3card).

-include_lib("kernel/include/logger.hrl").

-export([start/0]).

%% AtomVM main function

%% @doc Main AtomVM entrypoint
start() ->
    Config = c3card_config:reset_config(),
    WiFiConfig = proplists:get_value(c3card_wifi, Config),

    {ok, _} = logger_manager:start_link(#{}),
    {ok, _} = c3card_wifi:start(WiFiConfig),
    {ok, _} = c3card_app:start(normal, []),

    ?LOG_NOTICE("entering loop..."),
    ok = c3card_screen:draw_text("card started"),
    loop(#{}).

%% Internal functions

%% @private
loop(State) ->
    _ = timer:sleep(500),
    {ok, Readings} = c3card_sensor:read_sensors(),
    {ok, Buttons} = c3card_buttons:button_status(),
    Payload = #{readings => Readings,
		platform => esp32c3,
		system_info => c3card_system:info(),
		control => c3card_comm:get_port(),
		buttons => Buttons},

    handle_buttons(Buttons),

    case c3card_data:send_data(Payload) of
	ok ->
	    ok;
	Error ->
	    ?LOG_ERROR("error sending data: ~p", [Error])
    end,
    loop(State).

%% @private
handle_buttons(#{1 := low}) ->
    c3card_screen:draw_text("button 1 was pressed");
handle_buttons(_Buttons) ->
    noop.
