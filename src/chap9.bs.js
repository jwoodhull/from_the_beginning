// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");

function any(f, _l) {
  while(true) {
    var l = _l;
    if (l) {
      if (Curry._1(f, l[0])) {
        return /* true */1;
      } else {
        _l = l[1];
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function for_all(f, _l) {
  while(true) {
    var l = _l;
    if (l) {
      if (Curry._1(f, l[0])) {
        _l = l[1];
        continue ;
        
      } else {
        return /* false */0;
      }
    } else {
      return /* true */1;
    }
  };
}

function member(m) {
  return (function (param) {
      return any((function (i) {
                    return +(i === m);
                  }), param);
    });
}

function member_all(m) {
  return (function (param) {
      return for_all((function (param) {
                    return any((function (i) {
                                  return +(i === m);
                                }), param);
                  }), param);
    });
}

function devideBy(y, x) {
  return Caml_int32.div(x, y);
}

exports.any = any;
exports.for_all = for_all;
exports.member = member;
exports.member_all = member_all;
exports.devideBy = devideBy;
/* No side effect */
