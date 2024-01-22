%%%-------------------------------------------------------------------
%% @doc System information public API.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_system).

-export([info/0]).

-type info_map() ::
	#{atom_count => non_neg_integer(),
	  process_count => non_neg_integer(),
	  port_count => non_neg_integer(),
	  word_size => non_neg_integer(),
	  system_architecture => string()}. %% System information map

-export_type([info_map/0]).

%% API

%% @doc Return a map with basic system information
-spec info() -> info_map().
info() ->
    #{atom_count => erlang:system_info(atom_count),
      process_count => erlang:system_info(process_count),
      port_count => erlang:system_info(port_count),
      word_size => erlang:system_info(wordsize),
      system_architecture => erlang:system_info(system_architecture)}.
