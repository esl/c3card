%%%-------------------------------------------------------------------
%% @doc C3 Card AtomVM entrypoint
%% @end
%%%-------------------------------------------------------------------

-module(c3card).

-export([start/0]).

start() ->
    {ok, _Pid} = logger_manager:start_link(#{}),
    c3card_wifi:start([]),
    c3card_app:start(normal, []),
    loop().

loop() ->
    timer:sleep(500),
    Reading = c3card_sensor:read_sensor(),
    Buttons = c3card_buttons:button_status(),
    io:format("reading: ~p~n", [Reading]),
    io:format("buttons: ~p~n", [Buttons]),
    loop().
