%%%-------------------------------------------------------------------
%% @doc `c3card_screen_sysinfo' screen info.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_screen_sysinfo).

-behaviour(c3card_screen).

-export([draw/1]).

%% Callbacks

draw(CardInfo) ->
    {{Year, Month, Day}, _} = erlang:universaltime(),
    #{system_info := #{atom_count := AtomCount,
                       process_count := ProcessCount,
                       system_architecture := SystemArch}} = CardInfo,
    SysInfo = io_lib:format("~p~n~natoms: ~p~nprocesses: ~p~n~ndate: ~p/~p/~p~n",
                            [SystemArch, AtomCount, ProcessCount,
			     Year, Month, Day]),
    {ok, SysInfo}.
