%%%-------------------------------------------------------------------
%% @doc `c3card_screen_sysinfo' screen info.
%%
%% This is the main screen for the `c3card'. It displays the current
%% AHT20 sensor readings, the device current IP, current process count
%% and the system date.
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
                    ]}} = c3card_sensor:internal_sensor(),
    #{process_count := ProcessCount} = c3card_system:info(),
    {A, B, C, D} = c3card_status:get_ip(),
    {{Year, Month, Day}, _} = erlang:universaltime(),

    SysInfo =
        io_lib:format(sysinfo_header(),
          [trunc(Temp), trunc(Hum), trunc(RelHum),
           A, B, C, D, ProcessCount, Year, Month, Day]
         ),
    {ok, SysInfo}.

%% Internal functions

%% @hidden
sysinfo_header() ->
    "AHT20:~n ~pC, ~p%, ~pRH~n~n ~p.~p.~p.~p~nprocesses: ~p~ndate: ~p/~p/~p".
