%%%-------------------------------------------------------------------
%% @doc `c3card_screen_date' screen info.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_date).

-behaviour(c3card_screen).

-export([draw/1]).

%% Callbacks

draw(_CardInfo) ->
    {ok, "Hello date"}.
