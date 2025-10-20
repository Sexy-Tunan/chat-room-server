%% ==============================================================
%% 文件: chat_session.erl (聊天会话)
%% 每个客户端连接对应一个独立的业务处理进程
%% ==============================================================
-module(chat_session).
-export([start/1]).

-include("../include/protocol/chat_protocol.hrl").
-include("../include/database/chat_database.hrl").

-define(WORLD_CHANNEL_NAME, "world").

start(Socket) ->
	process_flag(trap_exit, true),
	receive
		start ->
			io:format("[chat_session, ~p] 获取了新建socket通道控制权~n", [self()]),
			io:format("[chat_session, ~p] 正在等待登录消息包~n", [self()]),
			try_login(Socket),
			inet:setopts(Socket, [{active, once}]),
			loop(Socket)
	end.

try_login(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		%% ----  获取socket通道数据
		{tcp,Socket,<<ProtoId:16, JsonBin/binary>>} ->
			io:format("接受到数据包~n"),
			case jsx:is_json(JsonBin) of
				true ->
					case ProtoId of
						?LOGIN_REQUEST_PROTOCOL_NUMBER ->
							case login_check(JsonBin) of
								true ->
									io:format("用户登录成功~n"),
									put(login_state,true),
									%% 构造返回内容，测试阶段
									ResponseJsonBin = jsx:encode(#{#login_response_packet.state => true, #login_response_packet.data => "登录成功啦"}),
									gen_tcp:send(socket, ResponseJsonBin),
									true;
								false -> io:format("用户登录失败(不存在或者密码错误)~n"), put(login_state,false), wrong_password;
								not_found -> io:format("用户不存在~n"), not_found
							end;
						_ -> io:format("~p非登录协议号，丢弃包数据~n", [ProtoId]), try_login(Socket)
					end;
				false -> io:format("json数据包并非utf8字节流"), try_login(Socket)
			end;
		%% ---- TCP 连接关闭 ----
		{tcp_closed, Socket} ->
			io:format("[chat_session] 客户端断开连接~n"),
			{error, disconnect}
	end.
login_check(JsonBin) ->
	%% 默认将键转为原子
	DataMpa = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
	UserName = maps:get(userName, DataMpa),
	Password = maps:get(password, DataMpa),
	case database_queryer:query_user_message_by_user_name(binary_to_list(UserName)) of
		{ok, User} -> string:equal(binary_to_list(Password), User#user.password);
		not_found -> not_found
	end.


new_loop(Socket) ->
	receive
		%% ----  获取socket通道数据
		{tcp,Socket,<<ProtoId:16, JsonBin/binary>>} ->
			DataMap = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
			case ProtoId of
				%% 用户发送频道消息
				?MSG_REQUEST_PROTOCOL_NUMBER ->
					%% 向频道管理者得到所属频道PID
					channel_manager:query_channel_pid(maps:get(channel,DataMap));

				%% 用户创建频道
				?CHANNEL_CREAT_REQUEST_PROTOCOL_NUMBER ->
					%% 向频道管理者发送新增频道消息
					Creator = maps:get(#channel_user_packet.user,DataMap),
					ChannelName = maps:get(#channel_user_packet.channel,DataMap),
					{ok,ChannelPid} = channel_manager:register_channel(Creator,ChannelName);
					%% 返回创建成功消息

				%% 用户删除频道
				?CHANNEL_DELETE_REQUEST_PROTOCOL_NUMBER ->
					Deleter = maps:get(#channel_user_packet.user,DataMap),
					ChannelName = maps:get(#channel_user_packet.channel,DataMap),
					{ok,_} = channel_manager:revoke_channel(Deleter,ChannelName);

				%% 用户加入频道
			 	?JOIN_CHANNEL_REQUEST_PROTOCOL_NUMBER ->
					Joiner = maps:get(#channel_user_packet.user,DataMap),
					ChannelName = maps:get(#channel_user_packet.channel,DataMap),
					{ok,ChannelPid} = channel_manager:query_channel_pid(ChannelName),
					%% 数据库新增频道与用户关系记录
					database_queryer:add_channel_user_record(ChannelName,Joiner),
					%% 向频道进程注册自己的信息，其会向其他客户端广播说明自己的加入
					ChannelPid ! {user_register, Joiner, self()};

				%% 用户退出频道
				?QUIT_CHANNEL_REQUEST_PROTOCOL_NUMBER ->
					Quitter = maps:get(#channel_user_packet.user,DataMap),
					ChannelName = maps:get(#channel_user_packet.channel,DataMap),
					{ok,ChannelPid} = channel_manager:query_channel_pid(ChannelName),
					%% 数据库新增频道与用户关系记录
					database_queryer:remove_channel_user_record(ChannelName,Quitter),
					%% 向频道进程注册自己的信息，其会向其他客户端广播说明自己的加入
					ChannelPid ! {user_revoke, Quitter}
			end,
			inet:setopts(Socket, [{active, once}]),
			new_loop(Socket);


		%% ---- TCP 连接关闭 ----
		{tcp_closed, Socket} ->
			io:format("[chat_session] 客户端断开连接~n"),
			{error, disconnect};

		%% 接收到频道广播消息，发送给客户端
		{msg_broadcast,ChannelName,SenderName,Message} ->

			JsonBinary = jsx:encode(
				[
					{#msg_response_packet.sender, SenderName},
					{#msg_response_packet.channel, ChannelName},
					{#msg_response_packet.message, Message}
				]
			),
			gen_tcp:send(Socket,JsonBinary),
			new_loop(Socket);

		%% 接收频道加入新用户消息，发送给客户端
		{user_join_channel,UserName,ChannelName} ->

			JsonBinary = jsx:encode(
				[

				]
			),
			gen_tcp:send(Socket,JsonBinary),
			new_loop(Socket);

		%% 接收到用户退出频道消息，发送给客户端
		{user_quit_channel,UserName,ChannelName} ->
			new_loop(Socket);

		%% 接受到新频道创建消息，发送给客户端
		{create_channel,Creator,CreatedChannelName} ->
			new_loop(Socket);

		%% 接受到频道删除消息，发送给客户端
		{delete_channel,Deleter,DeletedChannelName} ->
			new_loop(Socket)
	end.




handle_packet(BinData, Sock) ->
	%% 协议可根据前几个字节定义协议号，这里简单打印
	io:format("[chat_session] Received data: ~p~n", [BinData]),
	io:format("[chat_session] Received data (to str): ~ts~n", [unicode:characters_to_list(BinData,utf8)]),

	%% 示例：回发响应
	gen_tcp:send(Sock, <<"server ack">>).

%%  =============================================================================================
%% 旧版本的登录校验与循环,因为使用同步接受消息导致无法接收频道广播消息，摒弃，改为{active,once}并配合inet模块
try_login_sync(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, <<ProtoId:16, JsonBin/binary>>} ->
			case jsx:is_json(JsonBin) of
				true ->
					case ProtoId of
						?LOGIN_REQUEST_PROTOCOL_NUMBER ->
							case login_check(JsonBin) of
								true -> io:format("用户登录成功~n"), put(login_state,true), true;
								false -> io:format("用户登录失败(不存在或者密码错误)~n"), put(login_state,false), wrong_password;
								not_found -> io:format("用户不存在~n"), not_found
							end;
						_ -> io:format("~p非登录协议号，丢弃包数据~n", [ProtoId]), try_login(Socket)
					end;
				false -> io:format("json数据包并非utf8字节流"), try_login(Socket)
			end;
		{error, closed} ->
			io:format("[chat_session] 客户端断联~n"),
			{error, disconnect}
	end.


loop(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, <<ProtoId:16, JsonBin/binary>>} ->
			io:format("协议号——~p~n",[ProtoId]),
			handle_packet(JsonBin, Socket),
			loop(Socket);
		{error, closed} ->
			io:format("[chat_session] 客户端断联~n"),
			{error, disconnect};
		{error, Reason} ->
			io:format("[chat_session] 收到错误信息: ~p~n", [Reason]),
			{error, Reason}
	end.
%% ===============================================================================================