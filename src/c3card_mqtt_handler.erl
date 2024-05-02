-module(c3card_mqtt_handler).

-include_lib("kernel/include/logger.hrl").

-export([handle_data/2]).

%% Callbacks

handle_data(Topic, Command) ->
    ?LOG_NOTICE("unknown command ~p: ~p", [Topic, Command]),
    ok.
