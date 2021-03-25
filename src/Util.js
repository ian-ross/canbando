exports.blurTarget = function(event) {
  return function() {
    console.log(event.target);
    event.target.blur();
  }
}

exports.focusElement = function(elemId) {
  return function() {
    document.getElementById(elemId).focus();
  };
};

exports.blurElement = function(elemId) {
  return function() {
    document.getElementById(elemId).blur();
  };
};
