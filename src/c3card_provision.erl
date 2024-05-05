-module(c3card_provision).

-include_lib("kernel/include/logger.hrl").

-export([
    start_provisioning/0
]).

-export([
    handle_req/3
]).

-define(DEFAULT_AP_PSK, <<"password">>).
-define(DEFAULT_WEB_SERVER_PORT, 80).

%% API

start_provisioning() ->
    spawn(fun start_network/0).

%% Internal functions

save_config(SubmittedConfig) ->
    #{
        ssid := SSID,
        pass := Pass,
        gateway := Gateway,
        mqtt_enable := EnableMQTT,
        mqtt_user := User,
        mqtt_password := Password,
        device_name := DeviceName
    } = SubmittedConfig,

    MQTTOpts = #{
        enabled => EnableMQTT,
        host => Gateway,
        user => User,
        password => Password
    },
    WiFiOpts = #{ssid => SSID, psk => Pass},

    Config0 = c3card_config:default_config(),
    Config1 = c3card_config:merge_config(Config0, c3card_mqtt, MQTTOpts),
    Config2 = c3card_config:merge_config(Config1, c3card_wifi, WiFiOpts),

    c3card_config:store_config(Config2),
    c3card_config:set_name(DeviceName),

    ?LOG_NOTICE("Configuration has been set: ~p", [Config2]),
    ok.

get_net_config() ->
    [
        {ssid, c3card_config:default_name()},
        {psk, ?DEFAULT_AP_PSK}
    ].

start_network() ->
    ?LOG_NOTICE("Starting network"),
    case network:wait_for_ap(get_net_config()) of
        ok ->
            ?LOG_NOTICE("WLAN AP ready. Waiting connections."),
            start_web_server();
        Error ->
            ?LOG_ERROR("An error occurred starting network: ~p", [Error])
    end.

%% Web Server

start_web_server() ->
    Port = ?DEFAULT_WEB_SERVER_PORT,
    Router = [
        {"*", ?MODULE, []}
    ],
    http_server:start_server(Port, Router),
    ?LOG_NOTICE("Web server listening on port ~p", [Port]).

handle_req("GET", [], Conn) ->
    Body = config_html(),
    http_server:reply(200, Body, Conn);
handle_req("POST", [], Conn) ->
    ParamsBody = proplists:get_value(body_chunk, Conn),
    Params = http_server:parse_query_string(ParamsBody),

    SSID = proplists:get_value("ssid", Params),
    Pass = proplists:get_value("pass", Params),
    EnableMQTT = proplists:get_value("gateway_enabled", Params, "off"),
    Gateway = proplists:get_value("gateway", Params),
    User = proplists:get_value("gateway_user", Params),
    Password = proplists:get_value("gateway_pass", Params),
    DeviceName = proplists:get_value("device_name", Params),

    ok = save_config(
        #{
            ssid => SSID,
            pass => Pass,
            gateway => erlang:list_to_binary(Gateway),
            mqtt_enable => enable_mqtt(EnableMQTT),
            mqtt_user => erlang:list_to_binary(User),
            mqtt_password => erlang:list_to_binary(Password),
            device_name => erlang:list_to_binary(DeviceName)
        }
    ),

    spawn(fun() ->
        timer:sleep(5_000),
        esp:restart()
    end),

    Body = configured_html(),
    http_server:reply(200, Body, Conn);
handle_req(Method, Path, Conn) ->
    ?LOG_WARNING("invalid route: ~p ~p", [Method, Path]),
    Body = not_found_html(),
    http_server:reply(404, Body, Conn).

%% HTML templates

enable_mqtt("on") -> true;
enable_mqtt("off") -> false.

get_template(Template) ->
    case atomvm:read_priv(c3card, Template) of
        Bin when is_binary(Bin) -> {ok, Bin};
        undefined -> {error, no_template}
    end.

not_found_html() ->
    {ok, Template} = get_template("404.html"),
    Template.

configured_html() ->
    {ok, Template} = get_template("configured.html"),
    Template.

config_html() ->
    {ok, Template} = get_template("provision.html"),
    Template.
