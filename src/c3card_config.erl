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

-export([
    default_name/0,
    set_name/1,
    get_name/0,
    get_mac/0, get_mac/1,
    device_id/0
]).

-export([
    read_config/0,
    default_config/0,
    store_config/1,
    merge_config/3,
    reset_config/0
]).

-export([
    store_provision_info/1,
    read_provision_info/0
]).

-type config_key() ::
	c3card_buttons
      | c3card_neopixel
      | c3card_screen
      | c3card_sensors
      | c3card_battery
      | c3card_wifi.
%% `c3card' configuration key

-type config_map() ::
        c3card_buttons:config()
      | c3card_neopixel:config()
      | c3card_screen:config()
      | c3card_sensors:config()
      | c3card_wifi:config().
%% `c3card' configuration option

-type config() :: #{ConfigKey :: config_key() => ConfigValue :: config_map()}.
%% Configuration for `c3card'

%% API

%% @doc Read the `c3card' configuration settings
-spec read_config() -> config() | undefined.
read_config() ->
    case esp:nvs_get_binary(c3card, config) of
        undefined ->
	    undefined;
        Config ->
            erlang:binary_to_term(Config)
        end.

%% @doc Store the configuration in the NVS
-spec store_config(config()) -> ok | {error, Reason :: term()}.
store_config(Config) ->
    Encoded = erlang:term_to_binary(Config),
    esp:nvs_set_binary(c3card, config, Encoded).

%% @doc Reset the configuration
-spec reset_config() -> ok.
reset_config() ->
    esp:nvs_reformat().

-spec merge_config(Config :: config(), Key :: atom(), Options :: map()) -> NewConfig :: config().
merge_config(Config, Key, Options) ->
    CurrentOptions = maps:get(Key, Config),
    maps:put(Key, maps:merge(CurrentOptions, Options), Config).

-spec set_name(DeviceName :: bitstring()) -> ok | {error, Reason :: term()}.
set_name(DeviceName) when is_binary(DeviceName) ->
    esp:nvs_set_binary(c3card, name, DeviceName).

-spec get_name() -> DeviceName :: bitstring() | {error, Reason :: term()}.
get_name() ->
    case esp:nvs_get_binary(c3card, name) of
        undefined -> none;
        DeviceName -> DeviceName
    end.

-spec get_mac() -> MacAddr :: string().
get_mac() ->
    get_mac([{dotted, true}]).

get_mac(Opts) ->
    UseDots = proplists:get_value(dotted, Opts, true),
    {ok, Mac} = esp:get_default_mac(),
    decode_mac(Mac, UseDots).

store_provision_info(ProvisionInfo) ->
    Encoded = erlang:term_to_binary(ProvisionInfo),
    esp:nvs_set_binary(c3card, provision, Encoded).

read_provision_info() ->
    case esp:nvs_get_binary(c3card, provision) of
        undefined ->
            undefined;
        ProvisionInfo ->
            erlang:binary_to_term(ProvisionInfo)
    end.

device_id() ->
    case read_provision_info() of
	undefined ->
	    undefined;
	#{device_id := DeviceId} ->
	    DeviceId
    end.

%% internal functions

default_config() ->
    #{
      c3card_buttons => #{},
      c3card_mqtt => #{handler => c3card_mqtt_handler},
      c3card_neopixel => #{},
      c3card_screen => #{screen => {1, c3card_screen_sysinfo}},
      c3card_status => #{},
      c3card_battery => #{},
      c3card_sensors => #{sensors => ?DEFAULT_SENSORS},
      c3card_wifi => #{ntp => ?DEFAULT_NTP_HOST}
    }.

default_name() ->
    {ok, Mac} = esp:get_default_mac(),
    DeviceModel = <<"c3card">>,
    DeviceMac = decode_mac(Mac, false),
    <<DeviceModel/binary, <<"_">>/binary, DeviceMac/binary>>.

%% @hidden
decode_mac(<<A:8, B:8, C:8, D:8, E:8, F:8>>, WithDots) ->
    erlang:iolist_to_binary(
      io_lib:format(with_dots(WithDots), [A,B,C,D,E,F])
     ).

%% @hidden
with_dots(true) ->
    "~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b";
with_dots(false) ->
    "~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b".
