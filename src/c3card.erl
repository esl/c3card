%%%-------------------------------------------------------------------
%% @doc C3 Card AtomVM entrypoint
%% @end
%%%-------------------------------------------------------------------

-module(c3card).

-include_lib("kernel/include/logger.hrl").

-export([start/0]).

start() ->
    Config = c3card_config:reset_config(),
    {ok, _Pid} = logger_manager:start_link(#{}),
    c3card_wifi:start(Config),
    c3card_app:start(normal, []),
    ?LOG_NOTICE("entering loop..."),
    loop().

loop() ->
    timer:sleep(1_000),
    {ok, Readings} = c3card_sensor:read_sensors(),
    {ok, Buttons} = c3card_buttons:button_status(),
    Payload = #{readings => Readings,
		control => c3card_comm:get_port(),
		buttons => Buttons},
    case c3card_data:send_data(Payload) of
	ok ->
	    ok;
	Error ->
	    ?LOG_ERROR("error sending data: ~p", [Error])
    end,
    loop().

%% Internal functions
