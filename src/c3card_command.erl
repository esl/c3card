%%%-------------------------------------------------------------------
%% @doc c3card_command public API
%% @end
%%%-------------------------------------------------------------------

-module(c3card_command).

-callback handle_command(Command :: term()) ->
    {reply, Response :: term()}
	| noreply
	| {error, Reason :: term()}.
