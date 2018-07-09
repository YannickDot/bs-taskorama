type cb('a, 'd) = 'a => 'd;
type canceller = unit => unit;
type error('a) =
  | Error('a);
type task('b, 'c) = {. fork: (cb('b, unit), cb('c, unit)) => canceller};
let make = computation => {
  let t: task(error('a), 'b) = {pub fork = (rej, res) => computation(rej, res)};
  t;
};
let create = make;
let of_ = value =>
  make((_, res) => {
    let shouldIgnore = ref(false);
    if (! shouldIgnore^) {
      res(value);
    };
    let cancel = () => shouldIgnore := true;
    cancel;
  });
let fromPromise = promise =>
  make((rej, res) => {
    let shouldIgnore = ref(false);
    promise
    |> Js.Promise.then_(value =>
         Js.Promise.resolve(
           if (! shouldIgnore^) {
             res(value);
           },
         )
       )
    |> Js.Promise.catch(err =>
         Js.Promise.resolve(
           if (! shouldIgnore^) {
             rej(Error(err));
           },
         )
       );
    let cancel = () => shouldIgnore := true;
    cancel;
  });
let wait = (duration, value) =>
  make((_, res) => {
    let timerID = Js.Global.setTimeout(() => res(value), duration);
    let cancel = () => Js.Global.clearTimeout(timerID);
    cancel;
  });
let map = (cb, t) =>
  make((rej, res) => {
    let cancel = t#fork(rej, value => res(cb(value)));
    cancel;
  });
let chain = (cb, t) =>
  make((rej, res) => {
    let cancel2 = ref(() => ());
    let cancel1 =
      t#fork(
        rej,
        v1 => {
          let next = cb(v1);
          cancel2 := next#fork(rej, res);
        },
      );
    let cancel = () => {
      cancel1();
      cancel2^();
    };
    cancel;
  });
let ap = (taskToApply, t) => chain(f => map(f, taskToApply), t);
let join = t => chain(x => x, t);
let fork = (rej, res, t) => t#fork(rej, res);
