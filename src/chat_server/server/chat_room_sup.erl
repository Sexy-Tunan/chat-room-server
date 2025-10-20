%%%-------------------------------------------------------------------
%%% @author caiogu
%%% @doc
%%%		聊天室监控树进程
%%% @end
%%% Created : 20. 10月 2025 14:20
%%%-------------------------------------------------------------------
-module(chat_room_sup).
-author("caigou").

%% API
-export([start_link/1,start_link/0, init/1]).

start_link(Args) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, Args).
start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []).

init([]) ->
	{ok,
		{
			%% 重启策略
			{one_for_one, 3, 10},
			%% Children
			[
				{
					tag1,
					{database_queryer, start, [true]},
					permanent,
					10000,
					worker,
					[area_server]
				},
				{
					tag2,
					{channel_manager, start, []},
					permanent,
					10000,
					worker,
					[prime_server]
				}
			]
		}
	}.