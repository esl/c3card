%%%-------------------------------------------------------------------
%% @doc C3 Card AtomVM entrypoint
%% @end
%%%-------------------------------------------------------------------

-module(c3card).

-export([start/0]).

start() ->
    {ok, _Pid} = logger_manager:start_link(#{}),
    c3card_wifi:start([]),
    timer:sleep(5_000), %% TODO: Fix this mess
    c3card_app:start(normal, []),
    loop().

loop() ->
    timer:sleep(1000),
    {ok, {Hum, Temp, RH}} = c3card_sensor:read_sensor(),
    Buttons = c3card_buttons:button_status(),
    Payload = #{readings => [#{type => humidity, data => Hum},
			     #{type => relative_humidity, data => RH},
			     #{type => temperature, data => Temp}],
		buttons => Buttons},
    ok = c3card_socket:send_data(Payload),
    io:format("payload: ~p~n", [Payload]),
    loop().
