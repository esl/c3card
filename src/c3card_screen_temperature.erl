%%%-------------------------------------------------------------------
%% @doc `c3card_screen_temperature' screen info.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_temperature).

-behaviour(c3card_screen).

-export([draw/1]).

%% Callbacks

draw(CardInfo) ->
    #{readings := #{aht20 := Readings}} = CardInfo,
    [#{data := Hum, type := humidity},
     #{data := RelHum, type := relative_humidity},
     #{data := Temp, type := temperature}
    ] = Readings,
    SensorInfo = io_lib:format("AHT10:~n~pC, ~p%, ~pRH",
                               [trunc(Temp), trunc(Hum), trunc(RelHum)]),
    {ok, SensorInfo}.
