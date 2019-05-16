function succ (eid) { window.location = "event.html#" + eid; }

function err () { alert ("Something went wrong"); }

function submit () {
  let title = document.querySelector("#title").value
    , desc  = document.querySelector("#desc").value
    , place = document.querySelector("#place").value
    , date  = document.querySelector("#date").value
    , time  = document.querySelector("#time").value
    // , tag   = document.querySelector("#tag").value //for tags

  postMkevent (
      { req_title: title
      , req_desc:  desc
      , req_place: place
      , req_date:  date + 'T' + time + 'Z'
      }, succ, err);
}

function onCalendarClick (btn) {
  let now = new Date ();
  let d   = new Date (now.getFullYear(), now.getMonth() + selection,
                    Number (btn.innerText) - daysInThisMonth () + 1);
  document.getElementById("date").value = d.toJSON().split`T`[0];
}

let selection = 1;

function getMonthName () {
  var now  = new Date ();
  var then = new Date (now.getFullYear(), now.getMonth() + selection, 0);
  return `Januar Februar Mars April Mai Juni Juli August September Oktober November Desember`.split` `[then.getMonth()] + ' ' + then.getUTCFullYear();
}
function daysInThisMonth () {
  var now = new Date ();
  return new Date (now.getFullYear(), now.getMonth() + selection, 0).getDate();
}
function firstDayInThisMonth () {
  var now = new Date ();
  return new Date (now.getFullYear(), now.getMonth() + selection,
                   -daysInThisMonth () + 1)
      .getDay();
}

function populateCalendar () {
  let table     = document.querySelector("#calendar");
  let num_days  = daysInThisMonth ();
  let first_day = firstDayInThisMonth () - 1;
  let row       = document.createElement("tr");

  if (first_day == -1) first_day = 6;

  document.querySelectorAll('#calendar > tr').forEach (r => {
    table.removeChild(r);
  });

  document.querySelector('#month-name').innerText = getMonthName();

  for (let i = 0; i < first_day; i++) {
    let col = document.createElement("td");
    row.appendChild (col);
  }

  for (let i = 0; i < num_days; i++) {
    if ((i + first_day) % 7 === 0) {
      table.appendChild(row);
      row = document.createElement("tr");
    }

    let col       = document.createElement("td");
    let btn       = document.createElement("button");
    btn.innerText = i + 1;
    btn.setAttribute('onclick', 'onCalendarClick (this)');
    row.appendChild(col);
    col.appendChild(btn);
  }

  table.appendChild(row);
}

window.onload = _ => {
  populateCalendar ();
};

function calPrev () {
  selection--;
  populateCalendar ();
}

function calNext () {
  selection++;
  populateCalendar ();
}

function validateTime (el) {
  if (/^([0-1][0-9]|2[0-3]):([0-5][0-9])$/.test(el.value))
    el.style.backgroundColor = '#bfa';
  else
    el.style.backgroundColor = '#fba';
}

function calendarDropdown () {
  document.querySelector ('#calendar').classList.toggle('hidden');
}

