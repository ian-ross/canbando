exports.editDivContent = function(event) {
  return event.target.innerHTML;
}

exports.blurTarget = function(event) {
  return function() {
    console.log(event.target);
    event.target.blur();
  }
}
