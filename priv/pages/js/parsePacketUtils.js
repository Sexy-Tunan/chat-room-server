// WebSocket 数据包工具类
// 数据格式：4字节包长(大端序) + 2字节协议号(大端序) + JSON数据

// 协议号定义
const PROTOCOL = {
    // 服务器 -> 客户端
    LOGIN_RESPONSE: 20001,
    MSG_RESPONSE: 21001,
    CREATE_CHANNEL_RESPONSE: 22001,
    DELETE_CHANNEL_RESPONSE: 22002,
    JOIN_CHANNEL_RESPONSE: 23001,
    QUIT_CHANNEL_RESPONSE: 23002,
};


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

