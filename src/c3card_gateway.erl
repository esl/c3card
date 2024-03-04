%%%-------------------------------------------------------------------
%% @doc Gateway TCP communications public API.
%%
%% Provides an active TCP socket connected to the gateway for sending
%% any Erlang term over the wireless interface.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_gateway).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([reconnect/0,
         send_data/1,
         start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(SERVER, ?MODULE).

-type data_option() ::
        {gateway, inet:ip4_address()}
      | {port, non_neg_integer()}.
%% Gateway configuration option

-type config() :: [data_option()].
%% Default configuration for `c3card_data'

-export_type([config/0]).

%% API

%% @doc Reconnect to the gateway
-spec reconnect() -> ok | offline.
reconnect() ->
    gen_server:cast(?SERVER, reconnect).

%% @doc Send any Erlang term to the gateway
-spec send_data(Data :: term()) -> ok | {error, Reason :: term()}.
send_data(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

%% @doc Start and link the gateway TCP connection socket
-spec start_link(Config :: config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

%% @private
init(Config) ->
    Host = proplists:get_value(gateway, Config),
    Port = proplists:get_value(port, Config),
    ?LOG_NOTICE("starting gateway socket"),
    {ok, #{socket => try_connect(Host, Port),
           host => Host,
           port => Port}}.

%% @private
handle_call({send_data, _Data}, _From, #{socket := offline} = State) ->
    {reply, {error, offline}, State};
handle_call({send_data, Data}, _From, #{socket := Socket} = State) ->
    Payload = erlang:term_to_binary(Data),
    {reply, gen_tcp:send(Socket, Payload), State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(reconnect, State) ->
    #{host := Host, port := Port} = State,
    Socket = try_connect(Host, Port),
    {noreply, State#{socket => Socket}};
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info({tcp_closed, _Socket}, _State) ->
    {stop, tcp_closed};
handle_info({tcp_error, _Socket, Reason}, _State) ->
    {stop, {tcp_error, Reason}};
handle_info({tcp, _Socket, _Packet}, State) ->
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.

try_connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
            Socket;
        Error ->
            ?LOG_WARNING("unable to connect to gateway: ~p", [Error]),
            offline
    end.
