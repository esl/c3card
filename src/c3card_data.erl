%%%-------------------------------------------------------------------
%% @doc Gateway TCP data socket public API.
%%
%% Provides an active TCP socket connected to the gateway for sending
%% any Erlang term over the wireless interface.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_data).

-behaviour(gen_server).

-export([send_data/1,
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
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
	    {ok, Socket};
        Error ->
	    {stop, Error}
    end.

%% @private
handle_call({send_data, Data}, _From, Socket) ->
    Payload = erlang:term_to_binary(Data),
    {reply, gen_tcp:send(Socket, Payload), Socket};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info({tcp_closed, Socket}, Socket) ->
    {stop, tcp_closed};
handle_info({tcp_error, Socket, Reason}, Socket) ->
    {stop, {tcp_error, Reason}};
handle_info({tcp, Socket, _Packet}, Socket) ->
    {noreply, Socket};
handle_info(_Message, State) ->
    {noreply, State}.
