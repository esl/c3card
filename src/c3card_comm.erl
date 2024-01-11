%%%-------------------------------------------------------------------
%% @doc c3card_comm public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_comm).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([get_port/0,
	 start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-define(SERVER, ?MODULE).

%% API

get_port() ->
    gen_server:call(?SERVER, get_port).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(Config) ->
    Port = proplists:get_value(port, Config),
    Handler = proplists:get_value(handler, Config),
    Backend = proplists:get_value(backend, Config),
    case gen_tcp:listen(Port, [{active, true}, {inet_backend, Backend}]) of
        {ok, Socket} ->
	    spawn(fun() -> accept(Socket, Handler) end),
	    {ok, #{socket => Socket,
		   handler => Handler,
		   port => Port}};
        Error ->
	    {stop, Error}
    end.

handle_call(get_port, _From, #{port := Port} = State) ->
    {reply, Port, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

%% internal functions

accept(ListeningSocket, Handler) ->
    case gen_tcp:accept(ListeningSocket) of
	{ok, Socket} ->
	    spawn(fun() -> accept(ListeningSocket, Handler) end),
	    NewConn = spawn(fun() -> cmd_loop(Handler) end),
	    ?LOG_NOTICE("accepting new connection: ~p", [Socket]),
	    ok = gen_tcp:controlling_process(Socket, NewConn);
	{error, Reason} -> Reason
    end.

cmd_loop(Handler) ->
    receive
	{tcp_closed, _Socket} ->
	    ?LOG_NOTICE("remote session closed"),
	    ok;
	{tcp_error, _Socket, Reason} ->
	    ?LOG_ERROR("error for remote session: ~p", [Reason]),
	    ok;
	{tcp, Socket, Packet} ->
	    try
		Data = erlang:binary_to_term(erlang:iolist_to_binary(Packet)),
		case Handler:handle_command(Data) of
		    {reply, Reply} ->
			Payload = erlang:term_to_binary(Reply),
			gen_tcp:send(Socket, Payload);
		    noreply ->
			ok;
		    {error, _Reason} = Err ->
			Payload = erlang:term_to_binary(Err),
			gen_tcp:send(Socket, Payload)
		    end,
		cmd_loop(Handler)
	    catch _:_:_ ->
		    ?LOG_ERROR("exception while handling remote connection"),
		    ok
	    end;
	_ ->
	    ok
	end.
