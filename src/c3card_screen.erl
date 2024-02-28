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
         next_screen/0,
         set_screen/1,
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
                            {2, c3card_screen_codebeam},
                            {3, c3card_screen_demo}]).

-callback draw() -> {ok, Text :: binary()} | {error, Reason :: term()}.

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

-spec next_screen()  -> screen().
next_screen() ->
    gen_server:call(?SERVER, next_screen).

set_screen(Screen) ->
    gen_server:cast(?SERVER, {set_screen, Screen}).

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
    _Timer = timer_manager:send_after(100, self(), redraw),
    {ok, #{display => SSD1306, screen => default_screen()}}.

%% @private
handle_call({draw_text, Text0, FormatOpts}, _From, #{display := SSD1306} = State) ->
    Header = draw_header(),
    Text = io_lib:format(Text0, FormatOpts),
    %%ssd1306:clear(SSD1306),
    {reply, ssd1306:set_text(SSD1306, erlang:iolist_to_binary([Header, Text])), State};
handle_call(next_screen, _From, #{screen := CurrentScreen, display := SSD1306} = State) ->
    ssd1306:clear(SSD1306),
    NextScreen = switch_screen(CurrentScreen),
    render_screen(SSD1306, NextScreen),
    {reply, NextScreen, State#{screen => NextScreen}};
handle_call(clear, _From, #{display := SSD1306} = State) ->
    Header = draw_header(),
    ssd1306:clear(SSD1306),
    {reply, ssd1306:set_text(SSD1306, Header), State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({set_screen, Screen}, #{display := SSD1306} = State) ->
    ssd1306:clear(SSD1306),
    render_screen(SSD1306, Screen),
    {reply, ok, State#{screen => Screen}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(redraw, #{screen := Screen, display := SSD1306} = State) ->
    render_screen(SSD1306, Screen),
    _Timer = timer_manager:send_after(100, self(), redraw),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% Internal functions

%% @hidden
draw_header() ->
    io_lib:format(?SCREEN_HEADER, [atomvm:platform()]).

%% @hidden
render_screen(Display, {_ScreenIdx, ScreenMod}) ->
    case ScreenMod:draw() of
        {ok, Text0} ->
            Header = draw_header(),
            Text = io_lib:format(Text0, []),
            ssd1306:set_text(Display, erlang:iolist_to_binary([Header, Text]));
        {error, Reason} = Err ->
            ?LOG_ERROR("failed to draw: ~p", [Reason]),
            Err
    end.

%% @hidden
switch_screen({ScreenIdx, _ScreenMod}) ->
    case lists:keyfind(ScreenIdx + 1, 1, ?AVAILABLE_SCREENS) of
        false -> default_screen();
        ScreenInfo  -> ScreenInfo
    end.
