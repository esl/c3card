%%%-------------------------------------------------------------------
%% @doc workshop_wifi public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_wifi).

-include_lib("kernel/include/logger.hrl").

-include("config.hrl").

-export([start/1]).

%% API

start(_Config) ->
    NetConfig =
        [{sntp,
	  [{host, "pool.ntp.org"},
	   {synchronized, fun ntp_syncronized/1}]},
         {sta,
          [{connected, fun connected/0},
	   {got_ip, fun got_ip/1},
           {disconnected, fun disconnected/0},
	   {ssid, ?DEFAULT_STA_SSID},
	   {psk, ?DEFAULT_STA_PSK}]}],
    case network:start(NetConfig) of
        {ok, _Pid} = Res ->
            Res;
        Error ->
            ?LOG_ERROR("an error happened: ~p", [Error]),
            Error
    end.

%% internal functions

got_ip(IpInfo) ->
    ?LOG_DEBUG("got IP info: ~p", [IpInfo]).

connected() ->
    ?LOG_DEBUG("fully connected", []).

disconnected() ->
    ?LOG_WARNING("disconnected", []).

ntp_syncronized(_Timeval) ->
    ?LOG_DEBUG("NTP syncronized", []).
