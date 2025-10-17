%% ============================================================
%% CLIENT --> SERVER
-define(LOGIN_REQUEST_PROTOCOL_NUMBER, 10001).
-define(MSG_REQUEST_PROTOCOL_NUMBER, 11001).



%%=============================================================
%% SERVER --> CLIENT
-define(Login_RESPONSE_PROTOCOL_NUMBER, 20001).
-define(MSG_RESPONSE_PROTOCOL_NUMBER, 21001).


%% ============================================================
%% 协议号决定行为，数据体就是单纯的数据，它的意义依据行为的不同而发生变化,例如创建删除频道都可通用channel_user_packet的数据包格式
-record(msg_packet,{
	sender,
	channel,
	msg
}).

-record(login_packet,{
	user,
	password
}).

-record(channel_user_packet,{
	channel,
	user
}).

