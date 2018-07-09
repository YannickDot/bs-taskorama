open Task;

Js.log("Hello Taskorama in ReasonML!");

let myTask: task(error(string), int) =
  Task.of_((x, y) => [x, y])
  |> ap(Task.wait(1000, 2))
  |> ap(Task.wait(2000, 2))
  |> map(l => List.fold_left((acc, curr) => acc + curr, 0, l))
  |> map(x => x + 2)
  |> chain(x => of_(x + 2));

let stop = myTask |> fork(Js.log, Js.log);

/* Stop the execution after 1.5s */
Js.Global.setTimeout(_ => stop(), 1500);
