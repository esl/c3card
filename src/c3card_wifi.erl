%%%-------------------------------------------------------------------
%% @doc WiFi public API and configuration
%% @end
%%%-------------------------------------------------------------------

-module(c3card_wifi).

-include_lib("kernel/include/logger.hrl").

-export([start/1]).

-type wifi_option() ::
        {ssid, SSID :: binary()}
      | {psk, PSK :: binary()}
      | {ntp, Host :: string()}.
-type wifi_config() :: [wifi_option()].

-export_type([wifi_config/0]).

%% API

start(Config) ->
    WiFiConfig = proplists:get_value(c3card_wifi, Config),
    SSID = proplists:get_value(ssid, WiFiConfig),
    Psk = proplists:get_value(psk, WiFiConfig),
    NetConfig =
        [{sntp,
	  [{host, "pool.ntp.org"},
	   {synchronized, fun ntp_syncronized/1}]},
         {sta,
          [{connected, fun connected/0},
	   {got_ip, fun got_ip/1},
           {disconnected, fun disconnected/0},
	   {ssid, SSID},
	   {psk, Psk}]}],
    case network:start(NetConfig) of
        {ok, _Pid} = Res ->
	    timer:sleep(5_000),
	    Res;
        Error ->
            ?LOG_ERROR("an error happened: ~p", [Error]),
            Error
    end.

%% internal functions

got_ip(IpInfo) ->
    ?LOG_NOTICE("IP: ~p", [IpInfo]).

connected() ->
    ?LOG_NOTICE("fully connected", []).

disconnected() ->
    ?LOG_WARNING("disconnected", []).

ntp_syncronized(_Timeval) ->
    ?LOG_NOTICE("NTP syncronized", []).
