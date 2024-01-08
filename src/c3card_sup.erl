%%%-------------------------------------------------------------------
%% @doc workshop top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(INTENSITY, 10).
-define(PERIOD, 1_000).

%% API

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor callbacks

init([]) ->
    SupFlags = {one_for_one, ?INTENSITY, ?PERIOD},
    Config = c3card_config:read_config(),
    ChildSpecs =
	[
	 worker(c3card_sensor, Config),
	 worker(c3card_buttons, Config),
	 worker(c3card_socket, Config)
	 %%worker(c3card_screen, Config)
	],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

worker(Mod, Config) ->
    Args = proplists:get_value(Mod, Config),
    {Mod, {Mod, start_link, [Args]},
     permanent, brutal_kill, worker, [Mod]}.
