%% ==============================================================
%% 文件: chat_session.erl (聊天会话)
%% 每个客户端连接对应一个独立的业务处理进程
%% ==============================================================
-module(chat_session).
-export([start/1]).

-include("../include/protocol/chat_protocol.hrl").
-include("../include/database/chat_database.hrl").

start(Socket) ->
	process_flag(trap_exit, true),
	receive
		start ->
			io:format("[chat_session, ~p] 获取新建socket通道控制权~n", [self()]),
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
	case database_queryer:query_user_message_by(binary_to_list(UserName)) of
		{ok, User} -> string:equal(binary_to_list(Password), User#user.password);
		not_found -> not_found
	end.


new_loop(Socket) ->
	receive
	%% ----  获取socket通道数据
		{tcp,Socket,<<ProtoId:16, JsonBin/binary>>} ->
			case ProtoId of
				%% 用户发送频道消息
				?MSG_REQUEST_PROTOCOL_NUMBER ->
					DataMap = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
					%% 向频道管理者得到所属频道PID
					channel_manager:query_channel_pid(maps:get(channel,DataMap))
			end;

	%% ---- TCP 连接关闭 ----
		{tcp_closed, Socket} ->
			io:format("[chat_session] 客户端断开连接~n"),
			{error, disconnect}
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