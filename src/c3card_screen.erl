%%%-------------------------------------------------------------------
%% @doc c3card_screen public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen).

-include_lib("kernel/include/logger.hrl").

-include("config.hrl").

-behaviour(gen_server).

-export([switch_screen/1,
	 start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-define(SERVER, ?MODULE).

-callback draw() -> {ok, binary()} | {error, Reason :: term()}.

%% API

switch_screen(Screen) when is_atom(Screen) ->
    gen_server:call(?SERVER, {switch_screen, Screen}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(_Config) ->
    SSDConfig = #{sda_pin => ?DEFAULT_SDA_PIN,
		  scl_pin => ?DEFAULT_SCL_PIN},
    {ok, SSD1306} = ssd1306:start(SSDConfig),
    ssd1306:clear(SSD1306),
    ssd1306:set_contrast(SSD1306, 0),
    {ok, #{display => SSD1306}}.

handle_call({switch_screen, Screen}, _From, #{display := SSD1306} = State) ->
    ssd1306:clear(SSD1306),
    Reply =
	try
	    case Screen:draw() of
		{ok, Binary} -> ssd1306:set_text(SSD1306, Binary);
		{error, _Reason} -> ok
	    end
	catch
	    _C:E:_S -> {error, E}
	end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

%% internal functions
