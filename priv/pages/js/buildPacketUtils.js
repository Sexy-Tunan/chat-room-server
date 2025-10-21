// 协议号定义
import {PROTOCOL} from './protocol.js';

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
export function buildLoginPacket(userName, password) {
    return buildPacket(PROTOCOL.LOGIN_REQUEST, {
        userName: userName,
        password: password
    });
}

// 发送消息
export function buildMessagePacket(sender, channel, message) {
    return buildPacket(PROTOCOL.MSG_REQUEST, {
        sender: sender,
        channel: channel,
        message: message
    });
}

// 创建频道
export function buildCreateChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.CHANNEL_CREATE_REQUEST, {
        user: user,
        channel: channel
    });
}

// 删除频道
export function buildDeleteChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.CHANNEL_DELETE_REQUEST, {
        user: user,
        channel: channel
    });
}

// 加入频道
export function buildJoinChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.JOIN_CHANNEL_REQUEST, {
        user: user,
        channel: channel
    });
}

// 退出频道
export function buildQuitChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.QUIT_CHANNEL_REQUEST, {
        user: user,
        channel: channel
    });
}
