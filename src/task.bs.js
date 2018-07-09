// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_oo_curry = require("bs-platform/lib/js/caml_oo_curry.js");
var CamlinternalOO = require("bs-platform/lib/js/camlinternalOO.js");

var class_tables = [
  0,
  0,
  0
];

function make(computation) {
  if (!class_tables[0]) {
    var $$class = CamlinternalOO.create_table(["fork"]);
    var env = CamlinternalOO.new_variable($$class, "");
    var fork = CamlinternalOO.get_method_label($$class, "fork");
    CamlinternalOO.set_method($$class, fork, (function (self$1, rej, res) {
            return Curry._2(self$1[env][0], rej, res);
          }));
    var env_init = function (env$1) {
      var self = CamlinternalOO.create_object_opt(0, $$class);
      self[env] = env$1;
      return self;
    };
    CamlinternalOO.init_class($$class);
    class_tables[0] = env_init;
  }
  return Curry._1(class_tables[0], [computation]);
}

function of_(value) {
  return make((function (_, res) {
                var shouldIgnore = [/* false */0];
                if (!shouldIgnore[0]) {
                  Curry._1(res, value);
                }
                return (function () {
                    shouldIgnore[0] = /* true */1;
                    return /* () */0;
                  });
              }));
}

function fromPromise(promise) {
  return make((function (rej, res) {
                var shouldIgnore = [/* false */0];
                promise.then((function (value) {
                          return Promise.resolve(shouldIgnore[0] ? 0 : Curry._1(res, value));
                        })).catch((function (err) {
                        return Promise.resolve(shouldIgnore[0] ? 0 : Curry._1(rej, /* Error */[err]));
                      }));
                return (function () {
                    shouldIgnore[0] = /* true */1;
                    return /* () */0;
                  });
              }));
}

function wait(duration, value) {
  return make((function (_, res) {
                var timerID = setTimeout((function () {
                        return Curry._1(res, value);
                      }), duration);
                return (function () {
                    clearTimeout(timerID);
                    return /* () */0;
                  });
              }));
}

function map(cb, t) {
  return make((function (rej, res) {
                return Caml_oo_curry.js3(-1010802366, 1, t, rej, (function (value) {
                              return Curry._1(res, Curry._1(cb, value));
                            }));
              }));
}

function chain(cb, t) {
  return make((function (rej, res) {
                var cancel2 = [(function () {
                      return /* () */0;
                    })];
                var cancel1 = Caml_oo_curry.js3(-1010802366, 3, t, rej, (function (v1) {
                        var next = Curry._1(cb, v1);
                        cancel2[0] = Caml_oo_curry.js3(-1010802366, 2, next, rej, res);
                        return /* () */0;
                      }));
                return (function () {
                    Curry._1(cancel1, /* () */0);
                    return Curry._1(cancel2[0], /* () */0);
                  });
              }));
}

function ap(taskToApply, t) {
  return chain((function (f) {
                return map(f, taskToApply);
              }), t);
}

function join(t) {
  return chain((function (x) {
                return x;
              }), t);
}

function fork(rej, res, t) {
  return Caml_oo_curry.js3(-1010802366, 4, t, rej, res);
}

var create = make;

exports.make = make;
exports.create = create;
exports.of_ = of_;
exports.fromPromise = fromPromise;
exports.wait = wait;
exports.map = map;
exports.chain = chain;
exports.ap = ap;
exports.join = join;
exports.fork = fork;
/* No side effect */
