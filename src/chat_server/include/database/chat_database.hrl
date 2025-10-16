%% 聊天数据库中用户信息，聊天消息、频道信息的表结构如下：
-record(user,{name,password}).
-record(channel, {name, creator, alive}).
-record(msg, {user_name, channel_name, time, message}).


