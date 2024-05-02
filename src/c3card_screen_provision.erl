%%%-------------------------------------------------------------------
%% @doc `c3card_screen_provision' screen info.
%%
%% A screen for demo purposes.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_provision).

-behaviour(c3card_screen).

-export([draw/0]).

%% Callbacks

%% @private
draw() ->
    DeviceName = erlang:binary_to_list(c3card_config:default_name()),
    Password = "password",
    ProvisionInfo =
        io_lib:format(
            "~nSSID: ~n~s~n~nPSK: ~s~nWeb: 192.168.4.1~n",
            [DeviceName, Password]
        ),
    {ok, ProvisionInfo}.
