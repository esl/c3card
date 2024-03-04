%%%-------------------------------------------------------------------
%% @doc `c3card_status' reporter server.
%%
%% This server periodically reports the current card status to the
%% gateway via TCP.
%%
%% The card status is a map with the following keys:
%% <pre>
%%  #{readings => Readings,
%%    system_info => c3card_system:info(),
%%    control => c3card_comm:get_port()}</pre>
%% @end
%%%-------------------------------------------------------------------

-module(c3card_status).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([set_ip/1,
         get_ip/0,
         start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(SERVER, ?MODULE).

%% API

set_ip(IP) ->
    gen_server:cast(?SERVER, {set_ip, IP}).

get_ip() ->
    gen_server:call(?SERVER, get_ip).

%% @doc Start and link the card status reporter
-spec start_link(Config :: term()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

%% @private
init(_Config) ->
    Timer = timer_manager:send_after(1_000, self(), report),
    ?LOG_NOTICE("starting status reporter..."),
    {ok, #{timer => Timer, ip => undefined}}.

%% @private
handle_call(get_ip, _From, State) ->
    {reply, maps:get(ip, State), State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({set_ip, IP}, State) ->
    {noreply, State#{ip => IP}};
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(report, State) ->
    maybe_send_info(card_status()),
    Timer = timer_manager:send_after(1_000, self(), report),
    {noreply, State#{timer => Timer}};
handle_info(_Message, State) ->
    {noreply, State}.

%% Internal functions

%% @hidden
card_status() ->
    {ok, Readings} = c3card_sensor:read_sensors(),
    #{readings => Readings,
      system_info => c3card_system:info(),
      control => c3card_comm:get_port()}.

%% @hidden
maybe_send_info(CardInfo) ->
    case c3card_gateway:send_data(CardInfo) of
        ok ->
            c3card_neopixel:toggle_led(2, 200);
        {error, offline} ->
            c3card_neopixel:toggle_led(2, 50);
        Error ->
            c3card_neopixel:toggle_led(2, 350),
            ?LOG_ERROR("error sending data: ~p", [Error])
    end,
    c3card_neopixel:clear_all().
