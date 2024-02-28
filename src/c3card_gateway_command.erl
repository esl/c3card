%%%-------------------------------------------------------------------
%% @doc Remote gateway commands and callback implementation.
%%
%% It provides demo commands that the gateway will send to the device
%% for ensuring functionality and providing extra features.
%%
%% The following commands are supported:
%% <ul>
%%  <li>`{neopixel, {on, Led}}'</li>
%%  <li>`{candy, Turn}'</li>
%%  <li>`ping'</li>
%% </ul>
%% @end
%%%-------------------------------------------------------------------

-module(c3card_gateway_command).

-behaviour(c3card_command).

-export([handle_command/1]).

%% Callbacks

%% @private
handle_command({neopixel, {on, Led}}) ->
    spawn(fun() ->
                  [c3card_neopixel:toggle_led(Led, Hue)
                   || Hue <- lists:seq(1, 350)],
                  c3card_neopixel:clear_all()
          end),
    noreply;
handle_command({candy, Turn}) ->
    spawn(fun() ->
                  c3card_codebeam:set_turn(Turn),
                  c3card_screen:clear()
          end),
    noreply;
handle_command(ping) ->
    {reply, pong};
handle_command(Command) ->
    io:format("unknown command from gateway: ~p~n", [Command]),
    noreply.
