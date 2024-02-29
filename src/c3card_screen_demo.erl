%%%-------------------------------------------------------------------
%% @doc `c3card_screen_demo' screen info.
%%
%% A screen for demo purposes.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_demo).

-behaviour(c3card_screen).

-export([draw/0]).

%% Callbacks

%% @private
draw() ->
    {ok, io_lib:format("~n~n~nHello CodeBEAM!~n", [])}.
