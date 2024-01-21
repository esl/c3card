%%%-------------------------------------------------------------------
%% @doc Remote gateway commands and callback implementation
%% @end
%%%-------------------------------------------------------------------

-module(c3card_gateway_command).

-behaviour(c3card_command).

-export([handle_command/1]).

%% Callbacks

%% @private
handle_command({neopixel, {on, Led}}) ->
    [c3card_neopixel:toggle_led(Led, Hue) || Hue <- lists:seq(1, 350)],
    c3card_neopixel:clear_all(),
    noreply;
handle_command(ping) ->
    {reply, pong};
handle_command(Command) ->
    io:format("unknown command from gateway: ~p~n", [Command]),
    noreply.
