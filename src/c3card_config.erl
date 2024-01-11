%%%-------------------------------------------------------------------
%% @doc c3card_config public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_config).

-include("config.hrl").

-export([read_config/0,
	 store_config/1,
	 reset_config/0]).

%% API

read_config() ->
    case esp:nvs_get_binary(c3card, config) of
	undefined ->
	    default_config();
	Config ->
	    erlang:binary_to_term(Config)
	end.

store_config(Config) ->
    Encoded = erlang:term_to_binary(Config),
    esp:nvs_set_binary(c3card, config, Encoded).

reset_config() ->
    ok = esp:nvs_reformat(),
    Config = default_config(),
    store_config(Config),
    Config.

%% internal functions

default_config() ->
    [
     {c3card_buttons, []},
     {c3card_neopixel, [{pin, ?DEFAULT_NEOPIXEL_PIN},
			{total_pixels, ?DEFAULT_NEOPIXEL_TOTAL_PIXELS}]},
     {c3card_screen, []},
     {c3card_comm, [{handler, ?DEFAULT_GW_HANDLER},
		    {backend, ?DEFAULT_INET_BACKEND},
		    {gateway, ?DEFAULT_GW_HOST},
		    {port, ?DEFAULT_GW_COMM_PORT}]},
     {c3card_data, [{gateway, ?DEFAULT_GW_HOST},
		    {port, ?DEFAULT_GW_DATA_PORT}]},
     {c3card_sensor, [{sensors, ?DEFAULT_SENSORS},
		      {sda, ?DEFAULT_SDA_PIN},
		      {scl, ?DEFAULT_SCL_PIN}]},
     {c3card_wifi, [{ssid, ?DEFAULT_STA_SSID},
		    {psk, ?DEFAULT_STA_PSK}]}
    ].
