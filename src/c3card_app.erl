%%%-------------------------------------------------------------------
%% @doc C3 Card Application public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_app).

-behaviour(application).

-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).

%% application callbacks

start(_StartType, _StartArgs) ->
    ?LOG_NOTICE("starting application"),
    c3card_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
