%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%		application进程
%%% @end
%%% Created : 2025 14:21
%%%-------------------------------------------------------------------
-module(chat_room_app).
-author("caigou").

%% API
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/websocket", chat_websocket_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 10086}], #{
		env => #{dispatch => Dispatch}
	}),
	chat_room_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).
