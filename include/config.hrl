%% Default configuration options

-define(DEFAULT_STA_SSID, <<"ssid">>).
-define(DEFAULT_STA_PSK, <<"12345678">>).

%% Default gateway config
-define(DEFAULT_GW_HOST, {192, 168, 0, 20}).
-define(DEFAULT_GW_DATA_PORT, 9999).
-define(DEFAULT_GW_COMM_PORT, 9998).
-define(DEFAULT_GW_HANDLER, c3card_gateway_command).

-define(DEFAULT_SDA_PIN, 2).
-define(DEFAULT_SCL_PIN, 3).

-define(DEFAULT_NEOPIXEL_PIN, 4).
-define(DEFAULT_NEOPIXEL_TOTAL_PIXELS, 3).

-define(DEFAULT_INET_BACKEND, socket).

%% TODO: Not defined by behaviours
-define(DEFAULT_SENSORS,
	[
	 {aht20, start_link, []},
	 {bme280, start, [{address, 16#77}]}
	]
	).
