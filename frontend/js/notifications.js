{

function cont () {
  return document.querySelector("#notifications");
}

function addFriendReq (n) {
  const el = document.createElement ('li');
  el.classList.add('friend-request');

  getUser (n.key, usr => {
    el.setAttribute('data-uid', usr.uid);

    const pic = document.createElement('img');
    pic.classList.add('pic');
    pic.setAttribute('href', usr.pic);
    el.appendChild(pic);

    const msg = document.createElement('a');
    msg.innerText = usr.username + " vil legge deg til som venn";
    msg.href = "profile#" + usr.uid;
    el.appendChild(msg);

    cont().appendChild (el);
  });
}

function acceptFriend (el) {
}
function rejectFriend (el) {
}


window.addEventListener('load', _ => {
  getNotifications (ns => {
    for (let n of ns) switch (n.kind) {
      case ('FriendReq'):
        addFriendReq (n);
        break;
      default:
        console.log(n);
    }
  });
});


}

