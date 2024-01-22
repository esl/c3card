-define(DEFAULT_INET_BACKEND, socket).

-ifdef(PROD).
-include("config.prod.hrl").
-else.
-include("config.dev.hrl").
-endif.
