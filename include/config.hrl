-define(DEFAULT_INET_BACKEND, socket).

-define(DEFAULT_SDA_PIN, 2).
-define(DEFAULT_SCL_PIN, 3).
-define(DEFAULT_NEOPIXEL_PIN, 4).
-define(DEFAULT_NEOPIXEL_TOTAL_PIXELS, 3).

-ifdef(PROD).
-include("config.prod.hrl").
-else.
-include("config.dev.hrl").
-endif.
