// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE
'use strict';

var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function take(n, l) {
  if (l) {
    if (n < 0) {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "take"
          ];
    } else if (n === 0) {
      return /* [] */0;
    } else {
      return /* :: */[
              l[0],
              take(n - 1 | 0, l[1])
            ];
    }
  } else if (n === 0) {
    return /* [] */0;
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "take"
        ];
  }
}

function safe_devide(x, y) {
  try {
    return Caml_int32.div(x, y);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.division_by_zero) {
      return 0;
    } else {
      throw exn;
    }
  }
}

exports.take = take;
exports.safe_devide = safe_devide;
/* No side effect */
