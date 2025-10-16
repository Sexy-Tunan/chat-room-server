%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc 数据查询接口提供
%%%
%%% @end
%%% Created : 16. 10月 2025 09:22
%%%-------------------------------------------------------------------
-module(database_queryer).
-author("Administrator").
-behaviour(gen_server).

-include("../include/database/chat_database.hrl").


%% API
-export([start/1,stop/0]).
-export([init/1,query_user_message_by/1, handle_call/3, handle_info/2, handle_cast/2]).

-spec query_user_message_by(user_name()) -> user_message().
-type user_name() :: string().
-type user_message() :: {ok,#user{}} | {error, no_table} | user_not_found.


-record(state, {
	tables % #{TableName => #{ets => Ets, dets => Dets}},其实就是一个map映射存储表信息
}).

%% 启停方法
start(IsNeedInitData) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [IsNeedInitData], []).

stop() ->
	gen_server:call(?MODULE, stop).

%% API 接口

%% @doc 根据用户名查询用户信息
%% @return
query_user_message_by(UserName) -> gen_server:call(?MODULE,{query_user, table_name, user, user_name, UserName}).

%% ====================================================================
%% 初始化方法
init([IsNeedInitData]) ->
	%% 从dets数据库中查出数据加载到ets
	%% 定义所有表的元信息
	TablesInfo = [
		{user,    "data/user.dets",    user_ets,    #user.name},
		{msg,     "data/msg.dets",     msg_ets,     #msg.user_name},
		{channel, "data/channel.dets", channel_ets, #channel.name}
	],
	Tables = load_all_dets_to_ets(TablesInfo),
	case IsNeedInitData of
		true -> init_data(Tables);
		false -> do_nothing
	end,
	{ok, #state{tables = Tables}}.

load_all_dets_to_ets(TableInfos) ->
	lists:foldl(fun load_one_table/2, #{}, TableInfos).

%% 加载单张表
load_one_table({DetsName, DetsFile, EtsName, KeyPos}, Acc) ->
	{ok, _} = dets:open_file(DetsName,[{repair,true},{file,DetsFile},{type,bag}]),
	Ets = ets:new(EtsName, [bag, named_table, public, {keypos, KeyPos}]),
	load_from_dets(DetsName, Ets),
	maps:put(DetsName, #{ets => Ets, dets => DetsName}, Acc).

%% 将 dets 中的记录导入 ets
load_from_dets(Dets, Ets) ->
	dets:traverse(Dets,
		fun(Record) -> ets:insert(Ets, Record), continue end
	),
	ok.

init_data(Tables) ->
	%% 插入部分数据到ets
	case maps:get(user,Tables,undefined) of
		undefined -> nothing;
		#{ets := Ets1} ->
			ets:insert(Ets1,#user{name="Bruce",password="123456"}),
			ets:insert(Ets1,#user{name="Ben",password="123456"})
	end,

	case maps:get(channel,Tables,undefined) of
		undefined -> nothing;
		#{ets := Ets2} ->
			ets:insert(Ets2,#channel{name="bruce的频道",creator = "Bruce", alive = true}),
			ets:insert(Ets2,#channel{name="Ben的频道",creator = "Ben", alive = true})
	end,

	case maps:get(msg,Tables,undefined) of
		undefined -> nothing;
		#{ets := Ets3} ->
			ets:insert(Ets3,#msg{user_name = "Bruce", channel_name = "Bruce的频道", time = calendar:now_to_local_time(os:timestamp()), message = "你好呀Bruce"}),
			ets:insert(Ets3,#msg{user_name = "Ben", channel_name = "Ben的频道", time = calendar:now_to_local_time(os:timestamp()), message = "你好呀Ben"})
	end.



%% =======================================================================
%% 回调方法
%% 根据用户名查询用户信息
handle_call({query_user, table_name, TableName, user_name, UserName}, _From, State) ->
	#state{tables = Tables} = State,
	case maps:get(TableName, Tables, undefined) of
		undefined -> {reply, {error, no_table}, State};
		#{ets := Ets} ->
			case ets:lookup(Ets, UserName) of
				[Record] -> {reply, {ok, Record}, State};
				[] -> {reply, not_found, State}
			end
	end;

handle_call(stop, _From, State) ->
	#state{tables = Tables} = State,
	io:format("接受到停止请求，开始将ets数据写回dets~n"),
	write_ets_back_to_dets(Tables),
	{stop, normal, stopped, State}.

write_ets_back_to_dets(Tables) ->
	%% 便利Tables映射组将ets数据写回dets
	maps:foreach(
		fun(_TableName, #{ets := Ets, dets := Dets}) ->
			dets:from_ets(Dets,Ets),
			io:format("将~p数据写回dets~n",[_TableName])
		end,
		Tables
	).

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.



