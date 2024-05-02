-module(c3card_mqtt).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([
  start_link/1,
  publish/2
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

-define(NAME, ?MODULE).

-define(DEFAULT_PREFIX, <<"atomvm">>).
-define(DEFAULT_TOPICS, [<<"config">>, <<"state">>]).

%% API

publish(Action, Data) ->
    Payload = erlang:term_to_binary(Data),
    gen_server:call(?NAME, {publish, Action, Payload}).

start_link(Config) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Config, []).

%% gen_server callbacks

init(Config) ->
    Host = proplists:get_value(host, Config),
    Handler = proplists:get_value(handler, Config),
    User = proplists:get_value(user, Config),
    Password = proplists:get_value(password, Config),
    ConnectedHandler = fun(MQTT) -> handle_connected(MQTT, Handler) end,
    MqttConfig = #{url => <<"mqtt://", Host/binary>>,
		   connected_handler => ConnectedHandler,
		   username => User,
		   password => Password,
		   client_id => c3card_config:get_name()},
    {ok, MqttClient} = mqtt_client:start(MqttConfig),
    ?LOG_NOTICE("starting MQTT client"),
    State = #{client => MqttClient,
	      handler => Handler},
    {ok, State}.

handle_call({publish, Action, Payload}, _From, State) ->
    #{client := MqttClient} = State,
    Topic = build_topic(?DEFAULT_PREFIX, Action),
    HandlePublished = fun(_, _PublishedTopic, _MsgId) -> noop end,
    Opts = #{qos => at_least_once,
	     published_handler => HandlePublished},
    MsgId = mqtt_client:publish(MqttClient, Topic, Payload, Opts),
    {reply, {ok, MsgId}, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

%% Internal functions

handle_connected(MQTT, Handler) ->
    Prefix = ?DEFAULT_PREFIX,
    Config = mqtt_client:get_config(MQTT),
    ?LOG_NOTICE("Connected to ~p~n", [maps:get(url, Config)]),
    DataHandler = fun(MQTT1, Topic, Data) ->
			  handle_data(MQTT1, Prefix, Topic, Data, Handler)
		  end,
    [ok = mqtt_client:subscribe(MQTT, build_topic(Prefix, Topic),
				#{subscribed_handler => fun handle_subscribed/2,
				  data_handler => DataHandler})
     || Topic <- ?DEFAULT_TOPICS].

handle_subscribed(_MQTT, Topic) ->
    ?LOG_NOTICE("Subscribed to ~p.~n", [Topic]).

handle_data(MQTT, Prefix, Topic0, Data, Handler) ->
    case decode_data(Data) of
	{ok, Command} ->
	    case Handler:handle_data(Topic0, Command) of
		ok ->
		    ok;
		{reply, {command, _, Action, _Params} = ReplyCommand} ->
		    Payload = erlang:term_to_binary(ReplyCommand),
		    Topic = build_topic(Prefix, <<"command/", Action/binary>>),
		    mqtt_client:publish(MQTT, Topic, Payload)
	    end;
	{error, Reason} ->
	    ?LOG_ERROR("invalid command: ~p", [Reason]),
	    ok
    end.

build_topic(Prefix, Action) ->
    Name = c3card_config:get_name(),
    Component = <<"sensor/">>,
    TopicSuffix = <<"/", Action/binary>>,
    <<Prefix/binary,
      "/",
      Component/binary,
      Name/binary,
      TopicSuffix/binary>>.

decode_data(Data) ->
    try
	{ok, erlang:binary_to_term(Data)}
    catch
	_:_:_ ->
	    {error, invalid_command}
    end.
