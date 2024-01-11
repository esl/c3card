%%%-------------------------------------------------------------------
%% @doc c3card_neopixel public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_neopixel).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([clear_all/0,
	 toggle_led/1, toggle_led/2,
	 start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-define(SERVER, ?MODULE).

-define(NEOPIXEL_SATURATION, 100).
-define(NEOPIXEL_VALUE, 15).

%% API

clear_all() ->
    gen_server:call(?SERVER, clear_all).

toggle_led(Led) ->
    toggle_led(Led, 0).

toggle_led(Led, Hue) ->
    gen_server:call(?SERVER, {toggle_led, Led, Hue}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(Config) ->
    ?LOG_NOTICE("starting neopixels"),
    NeoPixelPin = proplists:get_value(pin, Config),
    NeoPixelTotal = proplists:get_value(total_pixels, Config),
    {ok, NeoPixel} = neopixel:start(NeoPixelPin, NeoPixelTotal),
    ok = neopixel:clear(NeoPixel),
    {ok, #{neopixel => NeoPixel}}.

handle_call(clear_all, _From, #{neopixel := NeoPixel} = State) ->
    {reply, neopixel:clear(NeoPixel), State};
handle_call({toggle_led, Led, Hue}, _From, #{neopixel := NeoPixel} = State) ->
    ok = neopixel:set_pixel_hsv(
	   NeoPixel, Led, Hue,
	   ?NEOPIXEL_SATURATION, ?NEOPIXEL_VALUE
	  ),
    {reply, neopixel:refresh(NeoPixel), State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

%% internal functions
