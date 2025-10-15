%% ==============================================================
%% 文件: chat_session.erl (聊天会话)
%% 每个客户端连接对应一个独立的业务处理进程
%% ==============================================================
-module(chat_session).
-export([start/1]).

start(Sock) ->
	process_flag(trap_exit, true),
	receive
		start ->
			io:format("[chat_session, ~p] ready to recv~n", [self()]),
			loop(Sock)
	end.

loop(Sock) ->
	case gen_tcp:recv(Sock, 0) of
%%		{ok, <<>>} ->
%%			%% 对端已关闭发送
%%			io:format("[chat_session] 客户端发送 EOF, 关闭socker连接~n"),
%%			loop(Sock);
		{ok, <<ProtoId:16, JsonBin/binary>>} ->
			io:format("协议号——~p~n",[ProtoId]),
			handle_packet(JsonBin, Sock),
			loop(Sock);
		{error, closed} ->
			io:format("[chat_session] 客户端断联~n"),
			ok;
		{error, Reason} ->
			io:format("[chat_session] recv error: ~p~n", [Reason]),
			ok
	end.

handle_packet(BinData, Sock) ->
	%% 协议可根据前几个字节定义协议号，这里简单打印
	io:format("[chat_session] Received data: ~p~n", [BinData]),
	io:format("[chat_session] Received data (to str): ~ts~n", [unicode:characters_to_list(BinData,utf8)]),

	%% 示例：回发响应
	gen_tcp:send(Sock, <<"server ack">>).