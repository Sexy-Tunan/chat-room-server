// 协议号定义
const PROTOCOL = {
    // 客户端 -> 服务器
    LOGIN_REQUEST: 10001,
    MSG_REQUEST: 11001,
    CHANNEL_CREATE_REQUEST: 12001,
    CHANNEL_DELETE_REQUEST: 12002,
    JOIN_CHANNEL_REQUEST: 13001,
    QUIT_CHANNEL_REQUEST: 13002,
    
    // 服务器 -> 客户端
    LOGIN_RESPONSE: 20001,
    MSG_RESPONSE: 21001,
    CREATE_CHANNEL_RESPONSE: 22001,
    DELETE_CHANNEL_RESPONSE: 22002,
    JOIN_CHANNEL_RESPONSE: 23001,
    QUIT_CHANNEL_RESPONSE: 23002
};

// 构建数据包
function buildPacket(protocolId, jsonObj) {
    const jsonStr = JSON.stringify(jsonObj);
    const encoder = new TextEncoder();
    const jsonBytes = encoder.encode(jsonStr);
    
    const totalLength = 2 + jsonBytes.length;
    const buffer = new ArrayBuffer(4 + totalLength);
    const view = new DataView(buffer);
    
    let offset = 0;
    view.setUint32(offset, totalLength, false);
    offset += 4;
    view.setUint16(offset, protocolId, false);
    offset += 2;
    
    const bytes = new Uint8Array(buffer);
    bytes.set(jsonBytes, offset);
    
    return buffer;
}

// 登录请求
function buildLoginPacket(userName, password) {
    return buildPacket(PROTOCOL.LOGIN_REQUEST, {
        userName: userName,
        password: password
    });
}

// 发送消息
function buildMessagePacket(sender, channel, message) {
    return buildPacket(PROTOCOL.MSG_REQUEST, {
        sender: sender,
        channel: channel,
        message: message
    });
}

// 创建频道
function buildCreateChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.CHANNEL_CREATE_REQUEST, {
        user: user,
        channel: channel
    });
}

// 删除频道
function buildDeleteChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.CHANNEL_DELETE_REQUEST, {
        user: user,
        channel: channel
    });
}

// 加入频道
function buildJoinChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.JOIN_CHANNEL_REQUEST, {
        user: user,
        channel: channel
    });
}

// 退出频道
function buildQuitChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.QUIT_CHANNEL_REQUEST, {
        user: user,
        channel: channel
    });
}
