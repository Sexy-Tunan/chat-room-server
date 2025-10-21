// chat.js
import {buildLoginPacket, buildMessagePacket, buildCreateChannelPacket,buildDeleteChannelPacket,buildJoinChannelPacket,buildQuitChannelPacket} from './buildPacketUtils.js';
import {parsePacket} from './parsePacketUtils.js';
import {PROTOCOL} from './protocol.js';
// ======== WebSocket 配置 ========
let ws = null;
const WS_URL = "ws://172.22.2.101:10086/websocket"; // 替换成你的服务端地址


// ======== 页面加载时初始化 WebSocket 和 chat页面控件绑定========
window.addEventListener("DOMContentLoaded", () => {
    // 发送消息按钮
    const sendBtn = document.getElementById("sendBtn");
    const messageInput = document.getElementById("messageInput");

    sendBtn.onclick = sendMessage;
    messageInput.addEventListener("keypress", (e) => {
        if (e.key === "Enter") sendMessage();
    });

    // 创建频道弹窗
    const createChannelBtn = document.getElementById("createChannelBtn");
    const createModal = document.getElementById("createChannelModal");
    const confirmCreateBtn = document.getElementById("confirmCreateBtn");
    const cancelCreateBtn = document.getElementById("cancelCreateBtn");
    const closeModal = createModal.querySelector(".close");

    createChannelBtn.onclick = () => (createModal.style.display = "flex");
    confirmCreateBtn.onclick = () => {
        const channelName = document.getElementById("newChannelName").value.trim();
        if (channelName) createChannel(channelName);
        createModal.style.display = "none";
    };
    cancelCreateBtn.onclick = closeModal.onclick = () => (createModal.style.display = "none");

    // 退出频道按钮
    const leaveBtn = document.getElementById("leaveChannelBtn");
    leaveBtn.onclick = () => {
        const currentChannel = document.getElementById("currentChannel").textContent;
        if (currentChannel && currentChannel !== "请选择一个频道") {
            quitChannel(currentChannel);
        }
    };
    connectWebSocket();
});

// ======== 发送消息 ========
function sendMessage() {
    const messageInput = document.getElementById("messageInput");
    const message = messageInput.value.trim();
    const currentChannel = document.getElementById("currentChannel").textContent;

    if (!message || !currentChannel || currentChannel === "请选择一个频道") return;

    const buffer = buildMessagePacket(localStorage.getItem('userName'), currentChannel, message);
    ws.send(buffer);

    // 本地渲染自己的消息
    const messageList = document.getElementById("messageList");
    const div = document.createElement("div");
    div.className = "message user";
    div.innerHTML = `<span class="username">你</span>: ${message}`;
    messageList.appendChild(div);
    messageList.scrollTop = messageList.scrollHeight;

    messageInput.value = "";
}



function connectWebSocket() {
    ws = new WebSocket(WS_URL);

    ws.binaryType = "arraybuffer"; // 我们要发送二进制包

    ws.onopen = () => {
        console.log("WebSocket 已连接");
        sendLoginRequest(localStorage.getItem('userName'), localStorage.getItem('password'));
        showLoginOverlay(true);
    };

    ws.onmessage = (event) => {
        // 将解包解码后的数据进行处理
        handleServerPacket(deepDecode(parsePacket(event.data)));
    };

    ws.onclose = () => {
        console.log("WebSocket 已断开");
        showLoginOverlay(false);
    };

    ws.onerror = (err) => {
        console.error("WebSocket 错误:", err);
        showLoginOverlay(false);
    };
}

// ======== 显示/隐藏登录遮罩 ========
function showLoginOverlay(show) {
    const overlay = document.getElementById("loginOverlay");
    overlay.style.display = show ? "flex" : "none";
}

// ======== 构建并发送登录请求包 ========
// 自定义协议：包长(4字节) + 协议号(2字节) + 数据(JSON)
function sendLoginRequest(user, password) {
    var buffer = buildLoginPacket(user,password);

    ws.send(buffer);
    console.log("已发送登录请求:", buffer);
}

// ======== 加入频道请求 ========
function joinChannel(channelName) {
    const buffer = buildJoinChannelPacket(localStorage.getItem('userName'),channelName);

    ws.send(buffer);
    console.log("发送加入频道请求:", channelName);
}
// ======== 退出频道请求 ========
function quitChannel(channelName) {
    const buffer = buildQuitChannelPacket(localStorage.getItem('userName'),channelName);

    ws.send(buffer);
    console.log("发送加入频道请求:", channelName);
}
// ======== 创建频道请求 ========
function createChannel(channelName) {
    const buffer = buildCreateChannelPacket(localStorage.getItem('userName'),channelName);

    ws.send(buffer);
    console.log("发送加入频道请求:", channelName);
}

// ======== 删除频道请求 ========
function deleteChannel(channelName) {
    const buffer = buildDeleteChannelPacket(localStorage.getItem('userName'),channelName);

    ws.send(buffer);
    console.log("发送加入频道请求:", channelName);
}


// ===========================================================================================================
// ============ 响应处理
// ======== 处理服务端返回数据包 ========
function handleServerPacket(parsedPacket) {

    console.log("收到数据包 Protocol:", parsedPacket.protocolNumber, "Data:", parsedPacket.data);
//    console.log("是否能解析存储常量的js对象", PROTOCOL.LOGIN_RESPONSE);

    switch (parsedPacket.protocolNumber) {
        case PROTOCOL.LOGIN_RESPONSE:
            handleLoginResponse(parsedPacket.data);
            break;
        case 21001:
            handleMsgResponse(parsedPacket.data);
            break;
        case 22001:
            handleCreateChannelResponse(parsedPacket.data);
            break;
        case 22002:
            handleDeleteChannelResponse(parsedPacket.data);
            break;
        case 23001:
             handleJoinChannelResponse(parsedPacket.data);
             break;
        case 23002:
            handleQuitChannelResponse(parsedPacket.data);
            break;
        default:
            console.warn("未知协议号:", protocolNumber);
    }
}

// ======== 处理登录响应 ========
function handleLoginResponse(payload) {
    // packet = { state: true/false, user: "username", data: {...} }
    if (payload.state) {
        console.log("登录验证成功:", payload.user);
        showLoginOverlay(false);
        // 可以初始化频道列表
        initChannels(payload.data);
    } else {
        console.error("登录失败:", payload.data);
        alert("登录失败：" + payload.data);
    }
}
// ======== 初始化频道列表 ========
function initChannels(channels) {
    const channelList = document.getElementById("channelList");
    const memberList = document.getElementById("memberList");
    channelList.innerHTML = "";
    memberList.innerHTML = "";

    for (const channel of channels) {
        const div = document.createElement("div");
        div.className = "channel-item";
        div.textContent = channel.channel_name;
        div.onclick = () => {
            renderMemberList(channel.channel_name,channel.members);
            document.getElementById("currentChannel").textContent = channel.channel_name;
        };
        channelList.appendChild(div);
    }
}

// ======== 处理消息的广播响应 ========
function handleMsgResponse(payload) {
    const messageList = document.getElementById("messageList");
    const div = document.createElement("div");
    div.className = "message";
    div.innerHTML = `<span class="username">${payload.sender}</span>: ${payload.message}`;
    messageList.appendChild(div);
    messageList.scrollTop = messageList.scrollHeight;
}

// ======== 处理新创建频道的广播响应 ========
function handleCreateChannelResponse(payload) {
    initChannels(payload.channels);
}
// ======== 处理删除频道的广播响应 ========
function handleDeleteChannelResponse(payload) {
    initChannels(payload.channels);

    // 如果删除的是当前频道，退出频道
    const currentChannel = document.getElementById("currentChannel").textContent;
    if (!payload.channels[currentChannel]) handleQuitChannelResponse({channel: currentChannel});
}
// ======== 处理加入频道的广播响应 ========
function handleJoinChannelResponse(payload) {
    // 更新当前频道
    document.getElementById("currentChannel").textContent = payload.channel;

    // 渲染成员列表
    renderMemberList(payload.channel, payload.users);

    // 激活输入框和发送按钮
    document.getElementById("messageInput").disabled = false;
    document.getElementById("sendBtn").disabled = false;

    // 清空消息列表
    document.getElementById("messageList").innerHTML = "";
}
// ======== 处理加入频道的广播响应 ========
function handleQuitChannelResponse(payload) {
    const currentChannel = document.getElementById("currentChannel");
    if (currentChannel.textContent === payload.channel) {
        currentChannel.textContent = "请选择一个频道";
        document.getElementById("messageInput").disabled = true;
        document.getElementById("sendBtn").disabled = true;
        document.getElementById("messageList").innerHTML = "";
        document.getElementById("memberList").innerHTML = "";
    }
}


// ======== 渲染成员列表 ========
function renderMemberList(channel, users) {
    const memberList = document.getElementById("memberList");
    memberList.innerHTML = "";
    users.forEach(user => {
        const div = document.createElement("div");
        div.className = "member-item";
        div.textContent = user;
        memberList.appendChild(div);
    });
}


// 将二进制转utf8字符串，因为erlang将字符串转成二进制
function deepDecode(data) {
    if (Array.isArray(data)) {
        // 如果是字符码数组（都是数字）
        if (data.every(x => typeof x === "number")) {
            return String.fromCharCode(...data);
        }
        // 如果是嵌套数组
        return data.map(deepDecode);
    } else if (typeof data === "object" && data !== null) {
        const result = {};
        for (const key in data) {
            result[key] = deepDecode(data[key]);
        }
        return result;
    } else {
        return data;
    }
}


