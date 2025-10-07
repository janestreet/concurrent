open! Core
open Async
open Await

(** An implementation of concurrency that uses {!Async} to spawn concurrent tasks

    {1 Example}

    {[
      open Async

      let result = Ivar.create ();;

      Concurrent_in_async.schedule_with_concurrent Await.Terminator.never ~f:(fun conc ->
        (* Create a new scope for concurrent tasks to be spawned in *)
        Concurrent.with_scope conc () ~f:(fun s ->
          (* [Concurrent.spawn_onto_initial] allows nonportable tasks to be spawned *)
          Concurrent.spawn_onto_initial s ~f:(fun _ _ _c ->
            Ivar.fill_exn result "hello from another concurrent task"))
        [@nontail])
    ]} *)

(** [scheduler ?monitor ?priority ()] is a handle that allows spawning concurrent tasks in
    the Async scheduler. *)
val scheduler
  :  ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> unit
  -> Capsule.Initial.k Capsule.Access.boxed Concurrent.Scheduler.t
[@@alert
  experimental
    "Effects are not supported in the OCaml 4 runtime, and breaking runtime4 builds does \
     not block continuous release. Please refrain from using effects until the 4 runtime \
     has been deprecated."]

(** [schedule_with_concurrent terminator ~f] schedules [f conc] to be run on the async
    scheduler, where [conc] is an implementation of concurrency which spawns tasks onto
    the async scheduler. The tasks are given {{!Capsule.Access.t} access} to the
    {{!Capsule.Initial.k} initial} capsule, which allows them to run nonportable functions
    via {!Concurrent.spawn_onto_initial}.

    [?monitor] and [?priority], if passed, are forwarded onto {!Async.schedule} *)
val schedule_with_concurrent
  :  ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> Terminator.t @ local
  -> f:(Capsule.Initial.k Capsule.Access.boxed Concurrent.t @ local -> 'a) @ once
  -> 'a Deferred.t
[@@alert
  experimental
    "Effects are not supported in the OCaml 4 runtime, and breaking runtime4 builds does \
     not block continuous release. Please refrain from using effects until the 4 runtime \
     has been deprecated."]
