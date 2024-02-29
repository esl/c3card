%%%-------------------------------------------------------------------
%% @doc `c3card_screen_codebeam' screen info.
%%
%% This screen is meant to show CodeBEAM helpers. Currently, it
%% displays your turn in case you requested a candy!
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_codebeam).

-behaviour(c3card_screen).

-export([draw/0]).

%% Callbacks

%% @private
draw() ->
    CurrentTurn = c3card_codebeam:candy_turn(),
    CandyInfo = io_lib:format("Get your candy!~n~nturn: ~p",
                              [CurrentTurn]),
    {ok, CandyInfo}.
