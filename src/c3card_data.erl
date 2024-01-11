%%%-------------------------------------------------------------------
%% @doc c3card_data public API
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

%% API

send_data(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(Config) ->
    Host = proplists:get_value(gateway, Config),
    Port = proplists:get_value(port, Config),
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
	    {ok, Socket};
        Error ->
	    {stop, Error}
    end.

handle_call({send_data, Data}, _From, Socket) ->
    Payload = erlang:term_to_binary(Data),
    Status = gen_tcp:send(Socket, Payload),
    {reply, Status, Socket};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, Socket) ->
    {stop, tcp_closed};
handle_info({tcp_error, Socket, Reason}, Socket) ->
    {stop, {tcp_error, Reason}};
handle_info({tcp, Socket, _Packet}, Socket) ->
    {noreply, Socket};
handle_info(_Message, State) ->
    {noreply, State}.

%% internal functions
