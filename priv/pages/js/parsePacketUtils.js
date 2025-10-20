// 解析数据包
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
        packetLength: packetLength,
        protocolId: protocolId,
        data: data
    };
}
