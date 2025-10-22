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
-behavior(application).
%% API
-export([start/2]).
-export([stop/1,prep_stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/websocket", chat_websocket_handler, []},
			%% 静态文件
			{"/", cowboy_static, {priv_file, chat_room, "priv/static/pages/login.html"}},
			{"/chat.html", cowboy_static, {priv_file, chat_room, "priv/static/pages/chat.html"}},
			{"/[...]", cowboy_static, {priv_dir, chat_room, "priv/static"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 10086}], #{
		env => #{dispatch => Dispatch}
	}),
	chat_room_sup:start_link().


prep_stop(State) ->
	channel_manager:broadcast_stop(),
	State.

stop(_State) ->
	cowboy:stop_listener(http).

