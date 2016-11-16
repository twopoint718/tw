function removeRide(event) {
  let index = event.target.dataset.rideNum;
  let xhr = new XMLHttpRequest();
  xhr.open("POST", "/remove", true);
  xhr.setRequestHeader(
    "Content-Type", "application/x-www-form-urlencoded");
  xhr.send("ride_num=" + index);

  xhr.onload = function() {
    window.location = "/";
  }
  return index;
}
