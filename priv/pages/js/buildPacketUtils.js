// WebSocket 数据包工具类
// 数据格式：4字节包长(大端序) + 2字节协议号(大端序) + JSON数据

// 协议号定义
const PROTOCOL = {
    // 客户端 -> 服务器
    LOGIN_REQUEST: 10001,
    MSG_REQUEST: 11001,
    CHANNEL_CREATE_REQUEST: 12001,
    CHANNEL_DELETE_REQUEST: 12002,
    JOIN_CHANNEL_REQUEST: 13001,
    QUIT_CHANNEL_REQUEST: 13002,


};

/**
 * 构建数据包
 * @param {number} protocolId - 协议号
 * @param {object} jsonObj - JSON对象
 * @returns {ArrayBuffer} - 完整的数据包
 */
function buildPacket(protocolId, jsonObj) {
    // 将JSON对象转为字符串
    const jsonStr = JSON.stringify(jsonObj);

    // 使用TextEncoder将字符串编码为UTF-8字节
    const encoder = new TextEncoder();
    const jsonBytes = encoder.encode(jsonStr);

    // 计算总长度 = 协议号(2字节) + JSON数据长度
    const totalLength = 2 + jsonBytes.length;

    // 创建完整的数据包缓冲区：包长(4字节) + 协议号(2字节) + JSON数据
    const buffer = new ArrayBuffer(4 + totalLength);
    const view = new DataView(buffer);

    let offset = 0;

    // 写入包长（4字节，大端序）
    view.setUint32(offset, totalLength, false); // false表示大端序
    offset += 4;

    // 写入协议号（2字节，大端序）
    view.setUint16(offset, protocolId, false);
    offset += 2;

    // 写入JSON字节数据
    const bytes = new Uint8Array(buffer);
    bytes.set(jsonBytes, offset);

    return buffer;
}

/**
 * 解析数据包
 * @param {ArrayBuffer} buffer - 接收到的数据包
 * @returns {object} - 包含协议号和数据的对象 {protocolId, data}
 */
function parsePacket(buffer) {
    const view = new DataView(buffer);

    let offset = 0;

    // 读取包长（4字节，大端序）
    const packetLength = view.getUint32(offset, false);
    offset += 4;

    // 读取协议号（2字节，大端序）
    const protocolId = view.getUint16(offset, false);
    offset += 2;

    // 读取JSON数据
    const jsonBytes = new Uint8Array(buffer, offset);
    const decoder = new TextDecoder('utf-8');
    const jsonStr = decoder.decode(jsonBytes);

    let data = null;
    try {
        data = JSON.parse(jsonStr);
    } catch (e) {
        console.error('解析JSON失败:', e);
    }

    return {
        packetLength,
        protocolId,
        data
    };
}

/**
 * 构建登录请求包
 * @param {string} userName - 用户名
 * @param {string} password - 密码
 * @returns {ArrayBuffer}
 */
function buildLoginPacket(userName, password) {
    return buildPacket(PROTOCOL.LOGIN_REQUEST, {
        userName: userName,
        password: password
    });
}

/**
 * 构建聊天消息请求包
 * @param {string} sender - 发送者
 * @param {string} channel - 频道名
 * @param {string} message - 消息内容
 * @returns {ArrayBuffer}
 */
function buildMessagePacket(sender, channel, message) {
    return buildPacket(PROTOCOL.MSG_REQUEST, {
        sender: sender,
        channel: channel,
        message: message
    });
}

/**
 * 构建创建频道请求包
 * @param {string} user - 用户名
 * @param {string} channel - 频道名
 * @returns {ArrayBuffer}
 */
function buildCreateChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.CHANNEL_CREATE_REQUEST, {
        user: user,
        channel: channel
    });
}

/**
 * 构建删除频道请求包
 * @param {string} user - 用户名
 * @param {string} channel - 频道名
 * @returns {ArrayBuffer}
 */
function buildDeleteChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.CHANNEL_DELETE_REQUEST, {
        user: user,
        channel: channel
    });
}

/**
 * 构建加入频道请求包
 * @param {string} user - 用户名
 * @param {string} channel - 频道名
 * @returns {ArrayBuffer}
 */
function buildJoinChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.JOIN_CHANNEL_REQUEST, {
        user: user,
        channel: channel
    });
}

/**
 * 构建退出频道请求包
 * @param {string} user - 用户名
 * @param {string} channel - 频道名
 * @returns {ArrayBuffer}
 */
function buildQuitChannelPacket(user, channel) {
    return buildPacket(PROTOCOL.QUIT_CHANNEL_REQUEST, {
        user: user,
        channel: channel
    });
}
