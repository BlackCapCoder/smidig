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
  el_date.classList.add  ('date');

  if (text.length > 256) {
    text = text.substr(0, 256) + "…";
  }

  ev.setAttribute     ('href', 'event.html#' + eid);
  el_pic.setAttribute ('src', pic);
  el_tit.innerText    = title;
  el_txt.innerText    = text;
  el_place.innerText  = place;
  el_date.innerText   = new Date(date).toLocaleDateString();

  ev.appendChild   (lft);
  ev.appendChild   (rgt);
  lft.appendChild  (el_pic);
  rgt.appendChild  (lft2);
  rgt.appendChild  (rgt2);
  lft2.appendChild (el_tit);
  lft2.appendChild (el_txt);
  rgt2.appendChild (el_place);
  rgt2.appendChild (el_date);

  evs.appendChild (ev);

  return ev;
}

getEvents (evs => {
  for (let e of evs) {
    getUser (e.owner, u => {
      addEvent (u.pic, e.title, e.desc, e.place, e.date, e.eid);
    });
  }
});

function onQueryChanged (txt) {
  let evs = document.querySelectorAll ('.event');
  let rx  = new RegExp (txt, 'i');

  for (let e of evs) {
    if (rx.test(e.innerText)) {
      e.classList.remove ('hidden');
    } else {
      e.classList.add    ('hidden');
    }
  }
}
