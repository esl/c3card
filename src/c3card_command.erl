%%%-------------------------------------------------------------------
%% @doc `c3card_command' behaviour callbacks.
%%
%% Provides the required handler callbacks that the `c3card_comm'
%% service needs to invoke once a successful payload decoding.
%% @end
%%%-------------------------------------------------------------------

-module(c3card_command).

-callback handle_command(Command :: term()) ->
    {reply, Response :: term()}
        | noreply
        | {error, Reason :: term()}.
%% Handle decoded Erlang terms from the gateway
