%%%-------------------------------------------------------------------
%% @doc Internal OLED display public API and control.
%%
%% The `c3card' provides an internal SSD1306 OLED display, mapped as:
%%
%% <ul>
%%   <li>`IO10': OLED Reset</li>
%% </ul>
%%
%% It uses the `i2c_bus' provided by <a
%% href="https://github.com/atomvm/atomvm_lib">atomvm_lib</a> for
%% sharing access to the I²C interface provided by the ESP32C3
%% microcontroller.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([draw_text/1, draw_text/2,
         clear/0,
	 default_screen/0,
	 available_screens/0,
	 switch_screen/1,
         render_screen/2,
         start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(SERVER, ?MODULE).

-define(SSD1306_ADDRESS, 16#3C).
-define(SSD1306_RESET_PIN, 10).

-define(SCREEN_HEADER, "c3card on ~p~n---------------~n").

-define(AVAILABLE_SCREENS, [{1, c3card_screen_sysinfo},
			    {2, c3card_screen_date},
			    {3, c3card_screen_temperature},
			    {4, c3card_screen_codebeam}]).

-callback draw(CardInfo :: map()) -> {ok, Text :: binary()} | {error, Reason :: term()}.

-type screen_option() ::
        {i2c_bus, i2c_bus:i2c_bus()}
      | {gpio, pid()}.
%% Screen configuration option

-type config() :: [screen_option()].
%% Default configuration for `c3card_screen'

-type screen_idx() :: 1..4.
-type screen() :: {ScreenIdx :: screen_idx(), ScreenMod :: atom()}.

-export_type([config/0]).

%% API

-spec default_screen() -> screen().
default_screen() ->
    {1, c3card_screen_sysinfo}.

-spec available_screens() -> [screen()].
available_screens() ->
    ?AVAILABLE_SCREENS.

-spec switch_screen(Screen :: screen())  -> screen().
switch_screen({ScreenIdx, _ScreenMod}) when ScreenIdx >= 1, ScreenIdx =< 4 ->
    case lists:keyfind(ScreenIdx + 1, 1, ?AVAILABLE_SCREENS) of
	false -> {1, c3card_screen_sysinfo};
	ScreenInfo  -> ScreenInfo
    end.

%% @doc Render a Screen providing the current card info.
-spec render_screen(Screen :: screen(), CardInfo :: map()) -> ok | {error, Reason :: term()}.
render_screen({_ScreenIdx, ScreenMod}, CardInfo) ->
    try
        case ScreenMod:draw(CardInfo) of
            {ok, Text} ->
                draw_text(Text);
            {error, _Reason} ->
                ok
        end
    catch _C:E:_S ->
            {error, E}
    end.


%% @doc Draw a string on the OLED screen
-spec draw_text(Text :: bitstring()) -> ok | {error, Reason :: term()}.
draw_text(Text) ->
    draw_text(Text, []).

%% @doc Draw a string on the OLED screen with formatting options
-spec draw_text(Text :: bitstring(), FormatOpts :: list()) -> ok | {error, Reason :: term()}.
draw_text(Text, FormatOpts) when is_list(FormatOpts) ->
    gen_server:call(?SERVER, {draw_text, Text, FormatOpts}).

%% @doc Clear the OLED screen
-spec clear() -> ok.
clear() ->
    gen_server:call(?SERVER, clear).

%% @doc Start and link the OLED screen server
-spec start_link(Config :: config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

%% @private
init(Config) ->
    I2CBus = proplists:get_value(i2c_bus, Config),
    SSDConfig = #{i2c_bus => I2CBus,
                  address => ?SSD1306_ADDRESS,
                  use_nif => false,
                  reset_pin => ?SSD1306_RESET_PIN},
    {ok, SSD1306} = ssd1306:start(SSDConfig),
    ssd1306:clear(SSD1306),
    ssd1306:set_contrast(SSD1306, 255),
    ?LOG_NOTICE("display started"),
    ssd1306:set_text(SSD1306, draw_header()),
    {ok, #{display => SSD1306}}.

%% @private
handle_call({draw_text, Text0, FormatOpts}, _From, #{display := SSD1306} = State) ->
    Header = draw_header(),
    Text = io_lib:format(Text0, FormatOpts),
    %%ssd1306:clear(SSD1306),
    {reply, ssd1306:set_text(SSD1306, erlang:iolist_to_binary([Header, Text])), State};
handle_call(clear, _From, #{display := SSD1306} = State) ->
    Header = draw_header(),
    ssd1306:clear(SSD1306),
    {reply, ssd1306:set_text(SSD1306, Header), State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Msg, State) ->
    {noreply, State}.

%% Internal functions

%% @hidden
draw_header() ->
    io_lib:format(?SCREEN_HEADER, [atomvm:platform()]).
