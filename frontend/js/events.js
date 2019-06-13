let activeTab = 2;

function addEvent (pic, title, text, place, date, eid) {
  let evs      = document.querySelector ('#events')
    , ev       = document.createElement ('a')
    , lft      = document.createElement ('div')
    , rgt      = document.createElement ('div')
    , lft2     = document.createElement ('div')
    , rgt2     = document.createElement ('div')
    , el_pic   = document.createElement ('img')
    , el_tit   = document.createElement ('h2')
    , el_txt   = document.createElement ('p')
    , el_place = document.createElement ('p')
    , el_time  = document.createElement ('p')
    , el_date  = document.createElement ('p')
    ;

  ev.classList.add       ('event');
  lft.classList.add      ('left');
  rgt.classList.add      ('right');
  lft2.classList.add     ('left');
  rgt2.classList.add     ('right');
  el_pic.classList.add   ('profile');
  el_tit.classList.add   ('title');
  el_txt.classList.add   ('text');
  el_place.classList.add ('place');
  el_time.classList.add  ('time');
  el_date.classList.add  ('date');

  el_place.classList.add ('info');
  el_time.classList.add  ('info');
  el_date.classList.add  ('info');
  el_pic.onerror = el => el.srcElement.classList.add('no-image');

  if (text.length > 256) {
    text = text.substr(0, 256) + "…";
  }

  const dt = new Date(date);

  ev.setAttribute     ('href', 'event#' + eid);
  el_pic.setAttribute ('src', pic);
  el_tit.innerText    = title;
  el_txt.innerText    = text;
  el_place.innerText  = place;
  el_date.innerText   = dt.getUTCDay() + '. ' + `Januar Februar Mars April Mai Juni Juli August September Oktober November Desember`.split` `[dt.getUTCMonth()];
  el_time.innerText   = 'Kl ' + dt.getUTCHours() + ':' + dt.getUTCMinutes();

  ev.appendChild   (lft);
  ev.appendChild   (rgt);
  lft.appendChild  (el_pic);
  rgt.appendChild  (lft2);
  rgt.appendChild  (rgt2);
  lft2.appendChild (el_tit);
  lft2.appendChild (el_txt);
  rgt2.appendChild (el_date);
  rgt2.appendChild (el_time);
  rgt2.appendChild (el_place);

  evs.appendChild (ev);

  return ev;
}

function addEvents (evs) {
  for (let e of evs) {
    getUser (e.owner, u => {
      addEvent (u.pic, e.title, e.subtitle, e.place, e.date, e.eid);
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
      getListevents (addEvents);
    } else {
      const tb = document.querySelector('#tb-search');
      tb.value = hash;
      onQueryChanged(hash);
    }
  });
});




function onQueryChanged (txt) {
  if (activeTab != 2) onChangeTab (2); // TODO

  if (window.queryChangedIntv !== undefined)
    clearTimeout(queryChangedIntv);

  window.queryChangedIntv = setTimeout(_ => {
    postSearchevents ([txt, getSelectedTag()], updateSearch);
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
  if (id === undefined || id === null) return;
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
  getMyEvents(updateSearch);
}
