let activeTab = 2;

function addEvent (pic, title, text, eid, mems) {
  let evs      = document.querySelector ('#events')
    , ev       = document.createElement ('a')
    , lft      = document.createElement ('div')
    , rgt      = document.createElement ('div')
    , lft2     = document.createElement ('div')
    , rgt2     = document.createElement ('div')
    , el_pic   = document.createElement ('img')
    , el_tit   = document.createElement ('h2')
    , el_txt   = document.createElement ('p')
    , el_mems  = document.createElement ('p')
    ;

  ev.classList.add       ('event');
  lft.classList.add      ('left');
  rgt.classList.add      ('right');
  lft2.classList.add     ('left');
  rgt2.classList.add     ('right');
  el_pic.classList.add   ('profile');
  el_tit.classList.add   ('title');
  el_txt.classList.add   ('text');
  el_mems.classList.add  ('num_members');

  el_mems.classList.add  ('info');

  if (text.length > 256) {
    text = text.substr(0, 256) + "…";
  }

  ev.setAttribute     ('href', 'groupPage#' + eid);
  el_pic.onerror = el => el.srcElement.classList.add('no-image');
  el_pic.setAttribute ('src', pic);
  el_tit.innerText    = title;
  el_txt.innerText    = text;
  el_mems.innerText   = mems == 0? "ingen medlemmer" : mems == 1? "Ett medlem" : mems + " medlemmer";

  ev.appendChild   (lft);
  ev.appendChild   (rgt);
  lft.appendChild  (el_pic);
  rgt.appendChild  (lft2);
  rgt.appendChild  (rgt2);
  lft2.appendChild (el_tit);
  lft2.appendChild (el_txt);
  rgt2.appendChild (el_mems);

  evs.appendChild (ev);

  return ev;
}

function addEvents (evs) {
  for (let e of evs) {
    getGroupMembers(e.gid, ms => {
      addEvent (null, e.title, e.subtitle, e.gid, ms.length);
    });
  }
}

window.addEventListener('load', _ => {
  getListTags (ts => {

    // populate tags
    const oEl = document.querySelector('#tag-options');
    for (let t of ts) {
      const el = document.createElement('option');
      el.setAttribute('data-id', t.tid);
      el.innerText = t.name;
      oEl.appendChild(el);
    }

    // load items
    let hash = window.location.hash.substr(1);

    if (hash == "") {
      postGroups(null, addEvents);
    } else {
      const tb = document.querySelector('#tb-search');
      tb.value = hash;
      onQueryChanged(hash);
    }
  });
});




function onQueryChanged (txt) {
  if (window.queryChangedIntv !== undefined)
    clearTimeout(queryChangedIntv);

  window.queryChangedIntv = setTimeout(_ => {
    postGroups (getSelectedTag(), updateSearch); // TODO
    // postSearchevents ([txt, getSelectedTag()], updateSearch);
  }, 50);
}

function clearItems  () {
  const cont = document.querySelector ('#events');
  while (cont.firstChild) cont.firstChild.remove();
}

function updateSearch (evs) {
  clearItems();
  addEvents(evs)
}

function getSelectedTag () {
  const oEl = document.querySelector('#tag-options');
  const o   = oEl.selectedOptions[0];
  const id  = o.getAttribute('data-id');
  if (id === undefined || id === null) return [];
  return Number(id);
}

function onChangeTab (ix) {
  if (activeTab == ix || ix < 1 || ix > 2) return;
  document.querySelector('.tab-button.active').classList.remove('active');
  document.querySelector('.tab-button:nth-child('+ix+')').classList.add('active');
  activeTab = ix;
  if (ix === 2) loadMine();
}

function loadMine () {
  postGroups(null, updateSearch);
  // getMyEvents(updateSearch);
}
