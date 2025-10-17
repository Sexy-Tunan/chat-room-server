%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc  只提供启动方法用于启动对应频道的进程，想要销毁只能通过channel_manager销毁，管理者会发送stop消息给对应的频道进程处理。
%%% @end
%%% Created : 16. 10月 2025 19:27
%%%-------------------------------------------------------------------
-module(channel).
-author("caiogu").

-behavior(gen_server).
%% API
-export([start/1]).
-export([init/1, handle_info/2, handle_cast/2]).

-include("../include/database/chat_database.hrl").


start(ChannelName) ->
	gen_server:start(?MODULE, [ChannelName], []).


init([ChannelName]) ->
	put(channelName, ChannelName),
	%% 查询所有关联的用户名
	UserNameList = database_queryer:query_user_message_by_channel_name(ChannelName),
	%% 使用named_table选项，这样就不会全局注册导致冲突
	Ets = ets:new(#user_pid,[set, private, {keypos, #user_pid.user_name}]),
	{ok,Ets}.


%% ===================================================================================
%% 回调方法
%% 查询pid

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(stop, State) -> {stop, normal, State};

handle_info({msg,SenderName,Message},State) ->
	%% 消息广播
	ChannelName = get(channel),
	PidList = ets:match(State, {_,'$1'}),
	[Pid ! {msg_broadcast,ChannelName,SenderName,Message} || Pid <- PidList],
	{noreply, State};

%% 用户向频道注册自己的信息
handle_info({user_register, UserName, From},State) ->
	%% 用户加入频道，或者用户连接了服务都需要来到频道注册自己的信息，不然无法进行广播
	ets:insert(State,#user_pid{user_name = UserName, pid = From}),
	{noreply, State};

%% 用户向频道注销自己的信息
handle_info({user_revoke, UserName},State) ->
	%% 用户websocket断联，或者退出频道都需要向频道注销自己的信息
	io:format("用户[~p]注销频道[~p]",[UserName,get(channelName)]),
	ets:delete(State,UserName),
	{noreply, State};
handle_info(_Info, State) -> {noreply, State}.


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