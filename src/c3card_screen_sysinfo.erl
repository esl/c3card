%%%-------------------------------------------------------------------
%% @doc `c3card_screen_sysinfo' screen info.
%%
%% This is the main screen for the `c3card'. It displays the current
%% AHT20 sensor readings, current process count and the system date.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_sysinfo).

-behaviour(c3card_screen).

-export([draw/0]).

%% Callbacks

%% @private
draw() ->
    {ok, #{aht20 := [
                     #{data := Hum, type := humidity},
                     #{data := RelHum, type := relative_humidity},
                     #{data := Temp, type := temperature}
                    ]}} = c3card_sensor:read_sensors(),
    #{process_count := ProcessCount} = c3card_system:info(),
    {{Year, Month, Day}, _} = erlang:universaltime(),
    CurrentTurn = c3card_codebeam:candy_turn(),
    SysInfo =
        io_lib:format(
          "AHT20:~n  ~pC, ~p%, ~pRH~nprocesses: ~p~nturn: ~p~ndate: ~p/~p/~p",
          [trunc(Temp), trunc(Hum), trunc(RelHum),
           ProcessCount, CurrentTurn, Year, Month, Day]
         ),
    {ok, SysInfo}.
