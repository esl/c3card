%%%-------------------------------------------------------------------
%% @doc c3card_buttons public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_buttons).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-type button_status() :: map().

-export_type([button_status/0]).

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

%% API

button_status() ->
    gen_server:call(?SERVER, button_status).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% gen_server callbacks

init(_Config) ->
    ?LOG_DEBUG("starting buttons: ~p", [?BUTTONS]),
    GPIO = gpio:start(),
    lists:map(fun({_Label, Pin}) ->
		      ok = gpio:set_direction(GPIO, Pin, input),
		      ok = gpio:set_pin_pull(Pin, up)
	      end,
	      ?BUTTONS),
    {ok, GPIO}.

handle_call(button_status, _From, GPIO) ->
    Status = maps:from_list(
	       lists:map(fun({Label, Pin}) ->
				 {Label, gpio:digital_read(Pin)}
			 end,
			 ?BUTTONS)),
    {reply, Status, GPIO};
handle_call(_Message, _From, State) ->
    {reply, error, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

%% internal functions
