%%%-------------------------------------------------------------------
%% @doc `c3card' configuration facilities and public API.
%%
%% The `c3card' configuration is stored in the card's NVS storage
%% encoded as a binary. The configuration holds all the diferent
%% options and parameters that the different application components
%% needs by default.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_config).

-include("config.hrl").

-export([read_config/0,
	 store_config/1,
	 reset_config/0]).

-type config_option() ::
	{c3card_buttons, c3card_buttons:config()}
      | {c3card_neopixel, c3card_neopixel:config()}
      | {c3card_screen, c3card_screen:config()}
      | {c3card_comm, c3card_comm:config()}
      | {c3card_data, c3card_data:config()}
      | {c3card_sensor, c3card_sensor:config()}
      | {c3card_wifi, c3card_wifi:config()}.
%% `c3card' configuration option

-type config() :: [config_option()].
%% Default configuration proplist for the `c3card' application

%% API

%% @doc Read the `c3card' configuration settings.
%%
%% If the configuration is not found in the NVS, it will return the
%% default configuration based on the compilation settings.
%% @end
-spec read_config() -> config().
read_config() ->
    case esp:nvs_get_binary(c3card, config) of
	undefined ->
	    default_config();
	Config ->
	    erlang:binary_to_term(Config)
	end.

%% @doc Store the configuration in the NVS
-spec store_config(config()) -> ok | {error, Reason :: term()}.
store_config(Config) ->
    Encoded = erlang:term_to_binary(Config),
    esp:nvs_set_binary(c3card, config, Encoded).

%% @doc Restore the configuration to the default settings
-spec reset_config() -> config().
reset_config() ->
    ok = esp:nvs_reformat(),
    Config = default_config(),
    store_config(Config),
    Config.

%% internal functions

default_config() ->
    [
     {c3card_buttons, []},
     {c3card_neopixel, []},
     {c3card_screen, []},
     {c3card_comm, [{handler, ?DEFAULT_GW_HANDLER},
		    {backend, ?DEFAULT_INET_BACKEND},
		    {gateway, ?DEFAULT_GW_HOST},
		    {port, ?DEFAULT_GW_COMM_PORT}]},
     {c3card_data, [{gateway, ?DEFAULT_GW_HOST},
		    {port, ?DEFAULT_GW_DATA_PORT}]},
     {c3card_sensor, [{sensors, ?DEFAULT_SENSORS}]},
     {c3card_wifi, [{ssid, ?DEFAULT_STA_SSID},
		    {psk, ?DEFAULT_STA_PSK},
		    {ntp, ?DEFAULT_NTP_HOST}]}
    ].
