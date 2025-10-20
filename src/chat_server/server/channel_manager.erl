%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 10月 2025 17:55
%%%-------------------------------------------------------------------
-module(channel_manager).
-author("Administrator").
-behavior(gen_server).

-define(WORLD_CHANNEL_NAME, "world").

%% API
-export([start/0,stop/0]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([query_channel_pid/1, query_channel_pid_batch/1, register_channel/2, revoke_channel/2]).


-include("../../../include/database/chat_database.hrl").

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
stop() ->
	gen_server:call(?MODULE, stop).


init([]) ->

	%% 预设此管理进程会挂，那么监控树会拉起，但是并非每次启动都需要去启动频道进程，只有第一次需要
	case ets:whereis(channel_pid_ets) of
		ChannelEts -> {ok, ChannelEts};
		undefined ->
			%% 查询数据库，得到所有的未删除的频道，创建对应的所有频道进程
			ChannelNameList = database_queryer:query_all_channel_name_alive(),
			%% 循环创建频道进程，并将对应关系插入
			ChannelEts = ets:new(channel_pid_ets, [bag, named_table, private, {keypos, 1}]),
			lists:foreach(fun(ChannelName) ->
				ets:insert(ChannelEts, #channel_pid{channel_name = ChannelName, pid = channel:start(ChannelName)}) end
				, ChannelNameList
			),
			{ok, ChannelEts}
	end.


%% ==================================================================================
%% API
query_channel_pid(ChannelName) -> gen_server:call(?MODULE, {query_pid, ChannelName}).
query_channel_pid_batch(ChannelNameList) -> gen_server:call(?MODULE, {query_pid_batch, ChannelNameList}).
register_channel(Creator,ChannelName) -> gen_server:call(?MODULE, {register, Creator, ChannelName}).
revoke_channel(Deleter,ChannelName) -> gen_server:call(?MODULE, {revoke, Deleter, ChannelName}).





%% ===================================================================================
%% 回调方法
%% 查询pid
handle_call({query_pid,ChannelName}, _From, State) ->
	{_, ChannelPid} = ets:lookup(State,ChannelName),
	{reply, {ok, ChannelPid}, State};


handle_call({query_pid_batch,ChannelNameList}, _From, State) ->
	ChannelPidList = [
		Pid || Name <- ChannelNameList, [{_, Pid}] <- [ets:lookup(State, Name)]  %% 只匹配查到的
	],
	{reply, {ok, ChannelPidList}, State};

%% 创建频道回调方法
handle_call({register,Creator, ChannelName}, _From, State) ->
	%% 创建频道进程并将关系写入ets内存
	NewChannelPid = channel:start(ChannelName),
	ets:insert(State, #channel_pid{channel_name = ChannelName, pid = NewChannelPid}),
	%% 数据库表新增记录
	database_queryer:add_channel_record(Creator,ChannelName),
	%% 通过世界频道进程将新频道信息广播给所有用户进程并通知客户端
	{_,_,WorldPid} = ets:lookup(State,?WORLD_CHANNEL_NAME),
	WorldPid ! {create_channel, Creator, ChannelName},
	{reply, {ok, NewChannelPid}, State};

%% 管理删除频道的回调方法
handle_call({revoke,Deleter, ChannelName}, _From, State) ->
	%% 创建频道进程并将关系写入ets内存
	NewChannelPid = channel:start(ChannelName),
	ets:insert(State, #channel_pid{channel_name = ChannelName, pid = NewChannelPid}),
	%% 通过世界频道进程将删除的频道信息广播给所有用户进程并通知客户端
	{_,_,WorldPid} = ets:lookup(State,?WORLD_CHANNEL_NAME),
	WorldPid ! {delete_channel, Deleter, ChannelName},
	{reply, ok, State};


handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.