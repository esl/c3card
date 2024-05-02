%%%-------------------------------------------------------------------
%% @doc System information public API.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_system).

-export([
    info/0,
    device_specs/0
]).

-type sensor_type() ::
    temperature
    | humidity
    | relative_humidity
    | carbon_dioxide.

-type info_map() ::
    #{
        atom_count => non_neg_integer(),
        process_count => non_neg_integer(),
        port_count => non_neg_integer(),
        %% System information map
        word_size => non_neg_integer()
    }.

-type specs_map() ::
    #{sensors => #{SensorType :: sensor_type() => #{unit => bitstring()}}}.

-export_type([info_map/0, specs_map/0]).

%% API

%% @Doc return a map with available device specs
-spec device_specs() -> specs_map().
device_specs() ->
    #{
        sensors =>
            #{
                temperature => #{unit => <<"°C">>},
                humidity => #{unit => <<"%">>},
                relative_humidity => #{unit => <<"RH">>},
                carbon_dioxide => #{unit => <<"ppm">>}
            }
    }.

%% @doc Return a map with basic system information
-spec info() -> info_map().
info() ->
    #{
        atom_count => erlang:system_info(atom_count),
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        word_size => erlang:system_info(wordsize)
    }.
