%%%-------------------------------------------------------------------
%% @doc `c3card_screen_codebeam' screen info.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_codebeam).

-behaviour(c3card_screen).

-export([draw/1]).

%% Callbacks

draw(_CardInfo) ->
    {ok, "Hello codebeam"}.
