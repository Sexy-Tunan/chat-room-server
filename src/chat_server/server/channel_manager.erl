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

%% API
-export([start/0,stop/0]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([query_channel_pid/1, register_channel/1, revoke_channel/1]).


-include("../include/database/chat_database.hrl").

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
stop() ->
	gen_server:call(?MODULE, stop).


init([]) ->

	%% 预设此管理进程会挂，那么监控树会拉起，但是并非每次启动都需要去启动频道进程，只有第一次需要
	case ets:whereis(#channel_pid) of
		ChannelEts -> {ok, ChannelEts};
		undefined ->
			%% 查询数据库，得到所有的未删除的频道，创建对应的所有频道进程
			ChannelNameList = database_queryer:query_all_channel_name_alive(),
			%% 循环创建频道进程，并将对应关系插入
			ChannelEts = ets:new(#channel_pid, [bag, named_table, private, {keypos, #channel_pid.channel_name}]),
			lists:foreach(fun(ChannelName) ->
				ets:insert(ChannelEts, #channel_pid = {channel_name = ChannelName, pid = channel:start(ChannelName)}) end
				, ChannelNameList
			),
			{ok, ChannelEts}
	end.


%% ==================================================================================
%% API
query_channel_pid(ChannelName) -> gen_server:call(?MODULE, {query_pid, ChannelName}).
register_channel(ChannelName) -> gen_server:call(?MODULE, {register, ChannelName}).
revoke_channel(ChannelName) -> gen_server:call(?MODULE, {revoke, ChannelName}).







%% ===================================================================================
%% 回调方法
%% 查询pid
handle_call({query_pid,ChannelName}, _From, State) ->
	{_, ChannelPid} = ets:lookup(State,ChannelName),
	{reply, {ok, ChannelPid}, State};

handle_call({register,ChannelName}, _From, State) ->
	%% 创建频道进程

	%% 关系写入ets内存
	ets:insert(State,#channel_pid{}),
	{reply, {ok, temp}, State};

handle_call({revoke,ChannelName}, _From, State) ->
	{_, ChannelPid} = ets:lookup(State,ChannelName),
	{reply, {ok, ChannelPid}, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.