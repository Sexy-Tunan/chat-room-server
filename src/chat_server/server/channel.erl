%%%-------------------------------------------------------------------
%%% @author channel
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%% Created : 16. 10月 2025 17:27
%%%-------------------------------------------------------------------
-module(channel).
-author("caiogu").

%% API
-export([start/1]).


start(Name) ->
	Pid = register(Name, spawn(?MODULE, loop, [Name, #{}])),
	register(Name, Pid),
	Pid.

loop(Name, Users) ->
	receive
		{join, UserPid} ->
			io:format("[~p] 用户加入: ~p~n", [Name, UserPid]),
			loop(Name, maps:put(UserPid, true, Users));

		{leave, UserPid} ->
			io:format("[~p] 用户离开: ~p~n", [Name, UserPid]),
			loop(Name, maps:remove(UserPid, Users));

		{broadcast, FromPid, Msg} ->
			io:format("[~p] 广播消息: ~p~n", [Name, Msg]),
			lists:foreach(
				fun(U) ->
					U ! {channel_msg, Name, Msg}
				end,
				maps:keys(Users)
			),
			loop(Name, Users);

		stop ->
			io:format("[~p] 已停止~n", [Name]),
			ok
	end.