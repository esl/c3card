%%%-------------------------------------------------------------------
%% @doc c3card_config public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_config).

-include_lib("kernel/include/logger.hrl").

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
     {c3card, [{sda, ?DEFAULT_SDA_PIN},
	       {scl, ?DEFAULT_SCL_PIN}]},
     {c3card_buttons, []},
     {c3card_neopixel, [{neopixel_pin, ?DEFAULT_NEOPIXEL_PIN},
			{neopixel_total_pixels, ?DEFAULT_NEOPIXEL_TOTAL_PIXELS}]},
     {c3card_screen, []},
     {c3card_sensor, []},
     {c3card_wifi, [{sta, ?DEFAULT_STA_SSID},
		    {psk, ?DEFAULT_STA_PSK}]}
    ].
