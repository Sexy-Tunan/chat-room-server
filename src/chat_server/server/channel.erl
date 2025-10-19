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
	%% 聊天消息广播
	ChannelName = get(channelName),
	PidList = ets:match(State, {_,'$1'}),
	[Pid ! {msg_broadcast,ChannelName,SenderName,Message} || Pid <- PidList],
	{noreply, State};

%% 用户向频道注册自己的信息
handle_info({user_register, UserName, From},State) ->
	%% 用户加入频道，或者用户连接了服务都需要来到频道注册自己的信息，不然无法进行广播
	ets:insert(State,#user_pid{user_name = UserName, pid = From}),
	%% 广播其他用户有新用户加入频道
	ChannelName = get(channelName),
	PidList = ets:match(State, {_,'$1'}),
	[Pid ! {user_join_channel,UserName,ChannelName} || Pid <- PidList],
	{noreply, State};

%% 用户向频道注销自己的信息
handle_info({user_revoke, UserName},State) ->
	ChannelName = get(channelName),
	%% 用户websocket断联，或者退出频道都需要向频道注销自己的信息
	ets:delete(State,UserName),
	%% 广播其他用户有新用户加入频道
	PidList = ets:match(State, {_,'$1'}),
	[Pid ! {user_quit_channel,UserName,ChannelName} || Pid <- PidList],
	{noreply, State};

%% 通过世界频道进程向所有用户广播新频道创建信息
handle_info({create_channel, Creator, CreatedChannelName},State) ->
	PidList = ets:match(State, {_,'$1'}),
	[Pid ! {create_channel,Creator,CreatedChannelName} || Pid <- PidList],
	{noreply, State};

%% 通过世界频道进程向所有用户广播频道删除信息
handle_info({delete_channel, Deleter, DeletedChannelName},State) ->
	PidList = ets:match(State, {_,'$1'}),
	[Pid ! {delete_channel,Deleter,DeletedChannelName} || Pid <- PidList],
	{noreply, State};

handle_info(_Info, State) -> {noreply, State}.
