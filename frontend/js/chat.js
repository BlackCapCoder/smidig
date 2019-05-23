function changeTab (ix) {
  document.querySelector('.tab.active').classList.remove('active');
  document.querySelector('.tab:nth-child('+ix+')').classList.add('active');
}

function openChat (el) {

}

window.addEventListener('load', loadChats)

function loadChats () {
  getMyChats(chats => {
    console.log(chats)
    for (c of chats) {
      addChat(c);
    }

    let h = window.location.hash.substr(1);
    if (h == "") return;
    h = Number(h);
    onChatClicked(h);
  });

  document.querySelector("#chatbox").onkeypress
    = onChatBoxKeyDown;
}

function activeTab () {
  const a = document.querySelector('.tab.active');
  return Array.from(a.parentElement.children).indexOf(a)
}

function addChat (chat)
{
  let chatElem = document.createElement('a')
  var tabNr    = activeTab();

  if (tabNr == 0) {
    chatElem.innerText = "Samtale #" + chat.cid;
  }

  chatElem.href="javascript:onChatClicked(" + chat.cid + ")";
  document.querySelector(".split .tab:nth-child(1)").appendChild(chatElem);
}

function onChatClicked (cid) {
  window.activeChat = cid;
  postReadChat(cid, openChat);
}

function openChat (messages) {
  for (let msg of messages) {
    idlastmessage = msg.mid;
    getUser(msg.sender, usr => {
      addChatMsgElem (usr.username, msg.content);
    });
  }
}

function addChatMsgElem (username, m) {
  const cont = document.querySelector('#chat');
  let el = document.createElement('div');
  el.classList.add("message");
  let msg = document.createElement('span');
  let usr = document.createElement('span');
  usr.innerText = username + ": ";
  msg.innerText = m;
  el.appendChild(usr);
  el.appendChild(msg);
  cont.appendChild(el);
}

setInterval(_ => {
  if (window.activeChat === undefined) return;
  postReadChatSince([activeChat, idlastmessage], openChat)
}, 3000);

function onChatBoxKeyDown (e) {
  if (window.activeChat === undefined)
    return false

  if (e.which !== 13) return;
  const msg = document.querySelector("#chatbox").value;
  if (msg.length === 0) return;

  postPutChat([activeChat, msg], mid => {
    document.querySelector("#chatbox").value = "";
    getWhoami(usr => {
      idlastmessage = mid
      addChatMsgElem(usr.username, msg);
    });
  });
}

