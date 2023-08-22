(*---------------------------------------------------------------------------
   Copyright (c) 2021 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t =
  { tasks : (unit -> unit) Queue.t;
    wakeup : Condition.t; (* There is a task or [finish] is true. *)
    m : Mutex.t;
    workers : Thread.t array;
    mutable finish : bool; }

let rec worker pool =
  Mutex.lock pool.m;
  while not (Queue.length pool.tasks <> 0 || pool.finish)
  do Condition.wait pool.wakeup pool.m done;
  let task = Queue.take_opt pool.tasks in
  Mutex.unlock pool.m;
  match task with
  | None -> Thread.exit ()
  | Some task -> task (); worker pool

let make n =
  let tasks = Queue.create () in
  let wakeup = Condition.create () in
  let m = Mutex.create () in
  let workers = Array.make n (Thread.self () (* just use something *)) in
  let pool = { tasks; wakeup; m; workers; finish = false } in
  for i = 0 to n - 1 do pool.workers.(i) <- Thread.create worker pool done;
  pool

let exec pool task =
  Mutex.lock pool.m;
  Queue.add task pool.tasks;
  Condition.signal pool.wakeup;
  Mutex.unlock pool.m

let finish pool =
  pool.finish <- true;
  for i = 0 to Array.length pool.workers - 1 do
    Mutex.lock pool.m;
    Condition.broadcast pool.wakeup;
    Mutex.unlock pool.m;
    Thread.join pool.workers.(i)
  done
