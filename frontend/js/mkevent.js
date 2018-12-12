function succ (eid) { window.location = "event.html#" + eid; }

function err () { alert ("Something went wrong"); }

function submit () {
  let title = document.querySelector("#title").value,
      desc  = document.querySelector("#desc").value,
      place = document.querySelector("#place").value,
      date  = document.querySelector("#date").value;

  postMkevent (
      {req_title : title, req_desc : desc, req_place : place, req_date : date},
      succ, err);
}

function onCalendarClick (btn) {
  let now = new Date ();
  let d   = new Date (now.getFullYear(), now.getMonth() + 1,
                    Number (btn.innerText) - daysInThisMonth () + 1);
  document.getElementById("date").value = d.toJSON();
}

function daysInThisMonth () {
  var now = new Date ();
  return new Date (now.getFullYear(), now.getMonth() + 1, 0).getDate();
}
function firstDayInThisMonth () {
  var now = new Date ();
  return new Date (now.getFullYear(), now.getMonth() + 1,
                   -daysInThisMonth () + 1)
      .getDay();
}

window.onload =
    _ => {
      let table     = document.querySelector("#calendar");
      let num_days  = daysInThisMonth ();
      let first_day = firstDayInThisMonth () - 1;
      let row       = document.createElement("tr");

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
    };


function calPrev () {

}

function calNext () {

}

function validateTime (el) {
  let txt = el.value;
  var isValid = /^([0-1][0-9]|2[0-3]):([0-5][0-9])$/.test(el.value);
  
  if (isValid)
  {
  el.style.backgroundColor = '#bfa';
  }
  else
  {
  el.style.backgroundColor = '#fba';
  }
  return isValid;
}

