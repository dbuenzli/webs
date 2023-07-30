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

let rec worker p =
  Mutex.lock p.m;
  while not (Queue.length p.tasks <> 0 || p.finish) do
    Condition.wait p.wakeup p.m
  done;
  let task = Queue.take_opt p.tasks in
  Mutex.unlock p.m;
  match task with
  | None -> Thread.exit ()
  | Some task -> task (); worker p

let create n =
  let tasks = Queue.create () in
  let wakeup = Condition.create () in
  let m = Mutex.create () in
  let workers = Array.make n (Thread.self () (* just use something *)) in
  let p = { tasks; wakeup; m; workers; finish = false } in
  for i = 0 to n - 1 do p.workers.(i) <- Thread.create worker p done;
  p

let exec p task =
  Mutex.lock p.m;
  Queue.add task p.tasks;
  Condition.signal p.wakeup;
  Mutex.unlock p.m

let finish p =
  p.finish <- true;
  for i = 0 to Array.length p.workers - 1 do
    Mutex.lock p.m;
    Condition.broadcast p.wakeup;
    Mutex.unlock p.m;
    Thread.join p.workers.(i)
  done
