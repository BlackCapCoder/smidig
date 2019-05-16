function changeTab (ix) {
  document.querySelector('.tab.active').classList.remove('active');
  document.querySelector('.tab:nth-child('+ix+')').classList.add('active');
}

function openChat (el) {

}

