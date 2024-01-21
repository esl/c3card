%%%-------------------------------------------------------------------
%% @doc c3card_screen public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([draw_text/1, draw_text/2,
	 clear/0,
	 switch_screen/1,
	 start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-define(SERVER, ?MODULE).

-define(SSD1306_ADDRESS, 16#3C).

-callback draw() -> {ok, binary()} | {error, Reason :: term()}.

%% API

draw_text(Text) ->
    draw_text(Text, []).

draw_text(Text, Opts) when is_list(Opts) ->
    gen_server:call(?SERVER, {draw_text, Text, Opts}).

clear() ->
    gen_server:call(?SERVER, clear).

switch_screen(Screen) when is_atom(Screen) ->
    gen_server:call(?SERVER, {switch_screen, Screen}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

%% @private
init(Config) ->
    I2CBus = proplists:get_value(i2c_bus, Config),
    _GPIO = proplists:get_value(gpio, Config),
    SSDConfig = #{i2c_bus => I2CBus,
		  address => ?SSD1306_ADDRESS,
		  use_nif => false,
		  reset_pin => 10},
    ?LOG_NOTICE("starting display"),
    {ok, SSD1306} = ssd1306:start(SSDConfig),
    ssd1306:clear(SSD1306),
    ssd1306:set_contrast(SSD1306, 255),
    {ok, #{display => SSD1306, i2c_bus => I2CBus}}.

%% @private
handle_call({draw_text, Text0, Opts}, _From, #{display := SSD1306} = State) ->
    Header = io_lib:format("c3card on ~p~n---------------~n", [atomvm:platform()]),
    Text = io_lib:format(Text0, Opts),
    ssd1306:clear(SSD1306),
    {reply, ssd1306:set_text(SSD1306, erlang:iolist_to_binary([Header, Text])), State};
handle_call(clear, _From, #{display := SSD1306} = State) ->
    ssd1306:clear(SSD1306),
    Header = io_lib:format("c3card on ~p~n---------------~n", [atomvm:platform()]),
    {reply, ssd1306:set_text(SSD1306, Header), State};
handle_call({switch_screen, Screen}, _From, #{display := SSD1306} = State) ->
    ssd1306:clear(SSD1306),
    Reply =
	try
	    case Screen:draw() of
		{ok, Text} ->
		    ssd1306:clear(SSD1306),
		    ssd1306:set_text(SSD1306, Text);
		{error, _Reason} -> ok
	    end
	catch
	    _C:E:_S -> {error, E}
	end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Msg, State) ->
    {noreply, State}.
