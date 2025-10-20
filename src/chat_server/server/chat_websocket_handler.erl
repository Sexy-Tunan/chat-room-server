%%%-------------------------------------------------------------------
%%% @author caigou
%%% @doc 浏览器不支持tcp原生连接，所以通过增加一个中间人转发信息
%%%
%%% @end
%%% Created : 20. 10月 2025 12:02
%%%-------------------------------------------------------------------
-module(chat_websocket_handler).
-author("caigou").
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include("../../../include/protocol/chat_protocol.hrl").
-include("../../../include/database/chat_database.hrl").


init(Req, _Opt) ->
	io:format("有客户端尝试与服务器进行websocket连接"),
	{cowboy_websocket, Req, #{}}.

websocket_init(State) ->
	io:format("有客户端与服务器建立websocket连接~n"),
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, State}.

%% 处理用户发送来的消息,无需回复，因为这些消息频道都会广播给对应的用户，当然就包括了处理这条消息的用户
websocket_handle({binary, <<_:32/big-unsigned, ProtoId:16/big-unsigned, JsonBin/binary>>}, State) ->
	io:format("Received JsonBin = ~p~n", [JsonBin]),
	DataMap = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
	case ProtoId of
		%% 处理登录请求
		?LOGIN_REQUEST_PROTOCOL_NUMBER ->
			UserName = maps:get(userName, DataMap),
			{PayLoadDataBin,NewState} = case login_check(JsonBin) of
			    true ->
					io:format("用户登录成功~n"),
					TempState = State#{login_state => true, user => UserName},
			 	    %% 查询数据库该用户加入了哪些频道，这些频道有哪些用户
			 	    JoinedChannelInfoWithMembers = database_queryer:query_joined_channel_info_with_members(UserName),
			 	    PayloadJsonBin = jsx:encode(#{state => true, user => UserName, data => JoinedChannelInfoWithMembers}),
					%% 向已加入的频道注册自己的登录信息，以便频道信息广播能接收到
					JoinChannelList = database_queryer:query_joined_channel_info_with_members(UserName),
					ChannelPidList = channel_manager:query_channel_pid_batch(JoinChannelList),
					[Pid ! {user_login_register,UserName,self()} || Pid <- ChannelPidList],

			 	    {PayloadJsonBin,TempState};
			    false ->
			 		io:format("用户登录失败密码错误~n"), put(login_state, false),
			 		PayloadJsonBin = jsx:encode(#{state => false, user => UserName, data => "密码错误"}),
					{PayloadJsonBin,State}
			end,
			PacketLength = 2 + length(PayLoadDataBin),
			Packet = <<
				PacketLength:32/big-unsigned-integer,
				?Login_RESPONSE_PROTOCOL_NUMBER:16/big-unsigned-integer,
				PayLoadDataBin
			>>,
			%% 构造数据包返回
			{reply, {binary,Packet},NewState};
		%% 用户发送频道消息
		?MSG_REQUEST_PROTOCOL_NUMBER ->
			%% 向频道管理者得到所属频道PID
			channel_manager:query_channel_pid(maps:get(channel,DataMap)),
			{ok, State};

		%% 用户创建频道
		?CHANNEL_CREAT_REQUEST_PROTOCOL_NUMBER ->
			%% 向频道管理者发送新增频道消息
			Creator = maps:get(#channel_user_packet.user,DataMap),
			ChannelName = maps:get(#channel_user_packet.channel,DataMap),
			{ok,ChannelPid} = channel_manager:register_channel(Creator,ChannelName),
			{ok, State};

		%% 用户删除频道
		?CHANNEL_DELETE_REQUEST_PROTOCOL_NUMBER ->
			Deleter = maps:get(#channel_user_packet.user,DataMap),
			ChannelName = maps:get(#channel_user_packet.channel,DataMap),
			{ok,_} = channel_manager:revoke_channel(Deleter,ChannelName),
			{ok, State};

		%% 用户加入频道
		?JOIN_CHANNEL_REQUEST_PROTOCOL_NUMBER ->
			Joiner = maps:get(#channel_user_packet.user,DataMap),
			ChannelName = maps:get(#channel_user_packet.channel,DataMap),
			{ok,ChannelPid} = channel_manager:query_channel_pid(ChannelName),
			%% 数据库新增频道与用户关系记录
			database_queryer:add_channel_user_record(ChannelName,Joiner),
			%% 向频道进程注册自己的信息，其会向其他客户端广播说明自己的加入
			ChannelPid ! {user_join_register, Joiner, self()},
			{ok, State};

		%% 用户退出频道
		?QUIT_CHANNEL_REQUEST_PROTOCOL_NUMBER ->
			Quitter = maps:get(#channel_user_packet.user,DataMap),
			ChannelName = maps:get(#channel_user_packet.channel,DataMap),
			{ok,ChannelPid} = channel_manager:query_channel_pid(ChannelName),
			%% 数据库新增频道与用户关系记录
			database_queryer:remove_channel_user_record(ChannelName,Quitter),
			%% 向频道进程注册自己的信息，其会向其他客户端广播说明自己的加入
			ChannelPid ! {user_revoke, Quitter},
			{ok, State}
	end;

websocket_handle(_Data, State) ->
	{ok, State}.

%% 接受频道广播消息 告知客户端用户加入频道信息
websocket_info({create_channel, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply,[{text, Msg}], State};

%% 接受频道广播消息 告知客户端用户退出频道信息
websocket_info({create_channel, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply,[{text, Msg}], State};

%% 接受频道广播消息 告知客户端新建频道信息
websocket_info({create_channel, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply,[{text, Msg}], State};

%% 接受频道广播消息 告知客户端删除频道信息
websocket_info({create_channel, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply,[{text, Msg}], State};

websocket_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, PartialReq, State) ->
	io:format("断联~n"),
	%% 查询自己已加入的频道有哪些
	JoinChannelList = database_queryer:query_joined_channel_info(maps:get(user,State)),
	case length(JoinChannelList) > 0 of
		true ->
			%% 向频道注销自己的登录信息
			{ok,ChannelPidList} = channel_manager:query_channel_pid_batch(JoinChannelList),
			UserName = maps:get(user, State),
			[Pid ! {user_revoke,UserName} || Pid <- ChannelPidList];
		false -> do_nothing
	end,
	ok.

%% 私有方法
%%====================================================================================
login_check(JsonBin) ->
	%% 默认将键转为原子
	DataMpa = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
	UserName = maps:get(userName, DataMpa),
	Password = maps:get(password, DataMpa),
	case database_queryer:query_user_message_by_user_name(binary_to_list(UserName)) of
		{ok, User} -> string:equal(binary_to_list(Password), User#user.password);
		not_found ->
			%% 用户不存在，为其注册
			database_queryer:add_user_record(UserName,Password),
			true
	end.