%%%-------------------------------------------------------------------
%% @doc c3card_buttons public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_buttons).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([button_status/0,
	 start_link/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2]).

-define(SERVER, ?MODULE).

-define(BTN1, {1, 5}).
-define(BTN2, {2, 6}).
-define(BTN3, {3, 7}).
-define(BTN4, {4, 8}).
-define(BUTTONS, [?BTN1, ?BTN2, ?BTN3, ?BTN4]).

-type button_state() :: high | low.

-type button_status() ::
	#{1 => button_state(),
	  2 => button_state(),
	  3 => button_state(),
	  4 => button_state()}.

-type buttons_config() :: [{gpio, pid()}].

-export_type([button_status/0]).

%% API

-spec button_status() -> {ok, button_status()}.
button_status() ->
    gen_server:call(?SERVER, button_status).

-spec start_link(Config :: buttons_config()) -> gen_server:start_ret().
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(_Config) ->
    ?LOG_NOTICE("starting buttons: ~p", [?BUTTONS]),
    GPIO = gpio:start(),
    lists:map(fun({_Label, Pin}) ->
		      ok = gpio:set_direction(GPIO, Pin, input),
		      ok = gpio:set_pin_pull(Pin, up)
	      end,
	      ?BUTTONS),
    {ok, GPIO}.

%% @private
handle_call(button_status, _From, GPIO) ->
    Status = maps:from_list(
	       lists:map(fun({Label, Pin}) ->
				 {Label, gpio:digital_read(Pin)}
			 end,
			 ?BUTTONS)),
    {reply, {ok, Status}, GPIO};
handle_call(_Message, _From, State) ->
    {reply, error, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Message, State) ->
    {noreply, State}.
