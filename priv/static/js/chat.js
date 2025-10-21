// chat.js
import {buildLoginPacket, buildMessagePacket, buildCreateChannelPacket,buildDeleteChannelPacket,buildJoinChannelPacket,buildQuitChannelPacket} from './buildPacketUtils.js';
import {parsePacket} from './parsePacketUtils.js';
import {PROTOCOL} from './protocol.js';
// ======== WebSocket 配置 ========
let ws = null;
const WS_URL = "/websocket"; // 替换成你的服务端地址

// 本地缓存的所有频道、成员、消息
const ChatState = {
    channels: {},    // {channelName: {members: [...], messages: [...]}}
    currentChannel: null,
    currentUser: localStorage.getItem("userName"),
};

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
        case PROTOCOL.JOIN_CHANNEL_RESPONSE:
            handleJoinChannelResponse();
            break;
        case PROTOCOL.MSG_BROADCAST:
            handleMsgBroadcast(parsedPacket.data);
            break;
        case PROTOCOL.CREATE_CHANNEL_BROADCAST:
            handleCreateChannelBroadcast(parsedPacket.data);
            break;
        case PROTOCOL.DELETE_CHANNEL_BROADCAST:
            handleDeleteChannelBroadcast(parsedPacket.data);
            break;
        case PROTOCOL.JOIN_CHANNEL_BROADCAST:
             handleJoinChannelBroadcast(parsedPacket.data);
             break;
        case PROTOCOL.QUIT_CHANNEL_BROADCAST:
            handleQuitChannelBroadcast(parsedPacket.data);
            break;
        default:
            console.warn("未知协议号:", protocolNumber);
    }
}

// ======== 处理登录响应 ========
function handleLoginResponse(payload) {
    console.log("处理登录消息");
    // packet = { state: true/false, user: "username", data: {...} }
    if (payload.state) {
        console.log("登录验证成功:", payload.user);
        showLoginOverlay(false);

        // 保存信息全局对象ChatState
        payload.data.forEach(ch => {
            ChatState.channels[ch.channel_name] = {
                members: ch.members,
                messages: []
            };
        });
        // 可以初始化频道列表
        initChannels();
    } else {
        console.error("登录失败:", payload.data);
        alert("登录失败：" + payload.data);
    }
}
// ======== 初始化频道列表,并将信息保存到全局对象中 ========
function initChannels(channels) {

    const channelList = document.getElementById("channelList");
    channelList.innerHTML = "";

    Object.keys(ChatState.channels).forEach(channelName => {
        const div = document.createElement("div");
        div.className = "channel-item";
        div.textContent = channelName;

        // 为每个渠道增加点击事件，以便切换频道时为聊天窗口和成员列表渲染数据
        div.onclick = () => showChannel(channelName);
        channelList.appendChild(div);
    });
}
//
function showChannel(channelName) {
    ChatState.currentChannel = channelName;
    const channel = ChatState.channels[channelName];

    const currentUser = ChatState.currentUser;
    const isMember = channel.members.includes(currentUser);

    document.getElementById("currentChannel").textContent = channelName;
    document.getElementById("memberCount").textContent = `成员数：${channel.members.length}`;
    renderMemberList(channel.members);

    const input = document.getElementById("messageInput");
    const sendBtn = document.getElementById("sendBtn");

    if (isMember) {
        input.disabled = false;
        sendBtn.disabled = false;
        renderMessages(channel.messages);
    } else {
        input.disabled = true;
        sendBtn.disabled = true;
        renderMessages([], true, channelName); // 显示提示遮罩
    }
}

function handleJoinChannelResponse(){

}

// ======== 处理消息的广播响应 ========
function handleMsgBroadcast(payload) {
    console.log("处理频道广播消息");
    const {channel, sender, message} = payload;

    // 把消息追加到本地缓存
    if (!ChatState.channels[channel]) return;
    ChatState.channels[channel].messages.push({sender, message});

    // 如果当前打开的频道就是这个频道，则立即渲染
    if (ChatState.currentChannel === channel) {
        appendMessage(sender, message);
    }
}
function renderMessages(messages, blocked = false, channelName = null) {
    const list = document.getElementById("messageList");
    list.innerHTML = "";

    if (blocked) {
        list.innerHTML = "<div class='welcome-message'><p>您不是该频道成员，无法查看消息</p><button id='joinChannelBtn' class='join-btn'>加入频道</button></div>";
        const joinBtn = document.getElementById("joinChannelBtn");
        joinBtn.onclick = () => {
            if (channelName) {
//                joinBtn.disabled = true;
//                joinBtn.textContent = "正在加入...";
                joinChannel(channelName);
            }
        };
        return;
    }

    messages.forEach(m => appendMessage(m.sender, m.message));
    list.scrollTop = list.scrollHeight;
}

function appendMessage(sender, message) {
    const list = document.getElementById("messageList");
    const div = document.createElement("div");
    div.className = sender === ChatState.currentUser ? "message user" : "message";
    div.innerHTML = `<span class="username">${sender}</span>: ${message}`;
    list.appendChild(div);
}
// ========================================================================

// ======== 处理新创建频道的广播响应 ========
function handleCreateChannelBroadcast(payload) {
    console.log("处理新频道创建的广播消息");
    ChatState.channels[payload.channel] = {members: [payload.user], messages: []};
    initChannels();
}
// ======== 处理删除频道的广播响应 ========
function handleDeleteChannelBroadcast(payload) {
    console.log("处理频被删除的广播消息");
    const {channel, user} = payload;
    delete ChatState.channels[channel];
    if (ChatState.currentChannel === payload.channel) {
        ChatState.currentChannel = null;
        document.getElementById("currentChannel").textContent = "请选择一个频道";
    }
    initChannels();
}

// ======== 处理加入频道的广播响应 ========
function handleJoinChannelBroadcast(payload) {
    console.log("处理用户加入频道的广播消息");
    const {channel, user} = payload;
    if (!ChatState.channels[channel]) ChatState.channels[channel] = {members: [], messages: []};
    if (!ChatState.channels[channel].members.includes(user)) {
        ChatState.channels[channel].members.push(user);
    }
    if (ChatState.currentChannel === channel) renderMemberList(ChatState.channels[channel].members);
}
// ======== 处理加入频道的广播响应 ========
function handleQuitChannelBroadcast(payload) {
    console.log("处理用户退出频道的广播消息");
    const {channel, user} = payload;
    const ch = ChatState.channels[channel];
    if (ch) ch.members = ch.members.filter(u => u !== user);
    if (ChatState.currentChannel === channel) renderMemberList(ch.members);
}


// ======== 渲染成员列表 ========
function renderMemberList(users) {
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


