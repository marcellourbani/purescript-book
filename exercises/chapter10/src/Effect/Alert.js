"use strict";

exports.alert = msg => () =>
  window.alert(msg);

exports.confirm = message => () => window.confirm(message)