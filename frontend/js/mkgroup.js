function succ (eid) {
  window.location = "groupPage#" + eid;
}

function err () { alert ("Something went wrong"); }

function submit () {
  let title = document.querySelector("#title").value
    , sub   = document.querySelector("#subtitle").value
    , desc  = document.querySelector("#desc").value
    , place = document.querySelector("#place").value
    , pub   = document.querySelector("#public").checked

  let req =
    { req_title: title
    , req_sub:   sub
    , req_desc:  desc
    , req_pub:   pub
    , req_loc:   place
    , req_tags:  getSelectedTags()
    };

  postMakeGroup (req, succ, err);
}

window.onload = _ => {
  getListTags (ts => {
    const oEl = document.querySelector('#tags');
    for (let t of ts) {
      const el = document.createElement('option');
      el.setAttribute('data-id', t.tid);
      el.innerText = t.name;
      oEl.appendChild(el);
    }
  });
};

function onAddTagClicked () {
  const oEl = document.querySelector('#tags');
  const sEl = document.querySelector('#added-tags');

  for (let o of oEl.selectedOptions) {
    oEl.removeChild(o);
    sEl.appendChild(o);
  }
}

function getSelectedTags () {
  const res = [];

  const sEl = document.querySelector('#added-tags');
  for (let o of sEl.children) {
    const id = o.getAttribute('data-id');
    if (id === undefined || id === null) continue;
    res.push(Number(id));
  }

  return res;
}
