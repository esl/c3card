%%%-------------------------------------------------------------------
%% @doc C3 Card Application public API and callbacks
%% @end
%%%-------------------------------------------------------------------

-module(c3card_app).

-behaviour(application).

-export([start/2, stop/1]).

%% application callbacks

%% @doc Start the <em>c3card</em> application
-spec start(StartType :: atom(), StartArgs :: []) -> application:start_type().
start(_StartType, _StartArgs) ->
    c3card_sup:start_link().

%% @doc Stop the <em>c3card</em> application
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.
