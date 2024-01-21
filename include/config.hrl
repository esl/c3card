-define(DEFAULT_INET_BACKEND, socket).

-define(DEFAULT_SDA_PIN, 2).
-define(DEFAULT_SCL_PIN, 3).

-ifdef(PROD).
-include("config.prod.hrl").
-else.
-include("config.dev.hrl").
-endif.
