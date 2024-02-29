%%%-------------------------------------------------------------------
%% @doc `c3card_screen_workshop' screen info.
%%
%% This screen is meant to show CodeBEAM workshop helpers. Currently,
%% it displays your turn in case you requested a candy!
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_workshop).

-behaviour(c3card_screen).

-export([draw/0]).

%% Callbacks

%% @private
draw() ->
    CurrentTurn = c3card_workshop:candy_turn(),
    CandyInfo = io_lib:format("Get your candy!~n~nturn: ~p",
                              [CurrentTurn]),
    {ok, CandyInfo}.
