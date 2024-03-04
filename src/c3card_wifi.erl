%%%-------------------------------------------------------------------
%% @doc WiFi public API and configuration.
%%
%% The current implementation only allows for the `c3card' to connect
%% to an specific access point provided in the initial params.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_wifi).

-include_lib("kernel/include/logger.hrl").

-export([start/1]).

-type wifi_option() ::
        {ssid, SSID :: binary()}
      | {psk, PSK :: binary()}
      | {ntp, Host :: string()}. %% WiFi configuration options

-type config() :: [wifi_option()].
%% Default configuration for `c3card_wifi'

-export_type([config/0]).

%% API

%% @doc Start WiFi facilities using the provided `Config'.
%%
%% The WiFi interface will connect to an access point provided in
%% `Config'. It does not support AP mode.
%% @end
-spec start(Config :: config()) -> {ok, WiFi :: pid()} | {error, Reason :: term()}.
start(Config) ->
    Parent = self(),
    SSID = proplists:get_value(ssid, Config),
    Psk = proplists:get_value(psk, Config),
    NTPHost = proplists:get_value(ntp, Config),
    NetConfig =
        [{sntp, [{host, NTPHost},
                 {synchronized, fun ntp_syncronized/1}]},
         {sta, [{connected, fun connected/0},
                {got_ip, fun(IpInfo) ->
				 got_ip(IpInfo),
				 Parent ! {ip, IpInfo}
			 end},
                {disconnected, fun disconnected/0},
                {ssid, SSID},
                {psk, Psk}]}],
    case network:start(NetConfig) of
        {ok, _Pid} ->
	    receive
		{ip, IpInfo} -> {ok, IpInfo}
	    end;
        Error ->
            ?LOG_ERROR("an error happened: ~p", [Error]),
            Error
    end.

%% internal functions

got_ip({IP, _Netmask, _Gateway}) ->
    ?LOG_NOTICE("STA IP: ~p", [IP]).

connected() ->
    ?LOG_NOTICE("WiFi connected", []).

disconnected() ->
    ?LOG_WARNING("WiFi disconnected", []).

ntp_syncronized({TVSec, TVUsec}) ->
    ?LOG_NOTICE("NTP syncronized: TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).
