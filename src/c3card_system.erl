%%%-------------------------------------------------------------------
%% @doc c3card_system public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_system).

-export([info/0]).

%% API

info() ->
    #{atom_count => erlang:system_info(atom_count),
      process_count => erlang:system_info(process_count),
      port_count => erlang:system_info(port_count),
      word_size => erlang:system_info(wordsize),
      system_architecture => erlang:system_info(system_architecture)}.
