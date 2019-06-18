function changeTab (ix) {
  document.querySelector('.tab.active').classList.remove('active');
  document.querySelector('.chat-button.active').classList.remove('active');
  document.querySelector('.tab:nth-child('+ix+')').classList.add('active');
  document.querySelector('.chat-button:nth-child('+ix+')').classList.add('active');
}

window.addEventListener('load', () => {
  getWhoami(u => {
    window.myid = u.uid;
    loadChats();
  })
});

function loadChats () {
  getMyChats(chats => {
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

function addChat (chat) {
  postChatParticipants(chat.cid, ps => {
    ps = ps.filter (u => u.uid != myid);

    let tab = undefined;
    let contactName = "";

    // if (ps.length == 1) {
    if (!chat.isPublic) { // TODO: p2p is defined as public
      tab = document.querySelector(".split .tab:nth-child(1)");
      getUser (ps[0].uid, u => addChatWithContact(chat, tab, u));
      return;
    } else {
      tab = document.querySelector(".split .tab:nth-child(2)");
      contactName = "Samtale #" + chat.cid;
    }

    let chatElem = document.createElement('a')
    chatElem.classList.add('chat-contact')
    chatElem.innerText = contactName;
    chatElem.href="javascript:onChatClicked(" + chat.cid + ")";

    if (tab === undefined) return;

    tab.appendChild(chatElem);
  });
}

function addChatWithContact (chat, tab, contact) {
  let cont = document.createElement('a')
  let pic  = document.createElement('img')
  let name = document.createElement('span')

  cont.classList.add ('contact');
  cont.setAttribute('data-id', chat.cid)
  cont.href      = "javascript:onChatClicked(" + chat.cid + ")";
  name.innerText = contact.username;
  pic.onerror = el => el.srcElement.src = "icons/Brukerikon-black.png";
  pic.src        = contact.pic;
  name.classList.add('contact-title');

  cont.appendChild(pic);
  cont.appendChild(name);

  if (tab === undefined) return;

  tab.appendChild(cont);
}

function onChatClicked (cid) {
  if (window.activeChat === cid) return;
  window.activeChat = cid;

  document.querySelector("#chat").innerHTML = '';

  function waitforit () {
    const contact = document.querySelector('.contact[data-id="'+cid+'"]');
    if (contact !== null)
      document.querySelector('#chat-title').innerText
        = contact.innerText;
    else
      setTimeout(waitforit, 50);
  }
  waitforit();

  postReadChat(cid, openChat);
}

function openChat (messages) {
  for (let msg of messages) {
    idlastmessage = msg.mid;
    getUser(msg.sender, usr => {
      addChatMsgElem (usr, msg);
    });
  }
}

function addChatMsgElem (usrObj, msgObj) {
  const username = usrObj.username;
  const m        = msgObj.content
  const cont     = document.querySelector('#chat');
  let el         = document.createElement('div');
  let msg        = document.createElement('span');

  let lft = document.createElement('div');
  let lftwrap = document.createElement('div');
  let rgt = document.createElement('div');

  lft.classList.add('left');
  lft.classList.add('wrapper');
  rgt.classList.add('right');

  let pic = document.createElement('img');
  pic.src = usrObj.pic;
  pic.onerror = el => el.srcElement.src = "icons/Brukerikon-black.png";
  pic.classList.add('msg-pic')
  el.appendChild(pic);

  el.classList.add("message");
  el.classList.add(usrObj.uid == myid ? 'mine' : 'not-mine');
  el.setAttribute('style', 'order: ' + msgObj.mid + ';');

  el.appendChild(lft);
  el.appendChild(rgt);

  msg.classList.add('content')
  msg.innerText = m;

  lft.appendChild(lftwrap);
  lftwrap.appendChild(pic);
  rgt.appendChild(msg);

  cont.appendChild(el);
}

setInterval(_ => {
  if (window.activeChat === undefined) return;
  if (window.idlastmessage === undefined) {
    readChat(activeChat, openChat);
  } else {
    postReadChatSince([activeChat, idlastmessage], openChat)
  }
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
      addChatMsgElem(usr, { mid: mid, content: msg });
    });
  });
}

