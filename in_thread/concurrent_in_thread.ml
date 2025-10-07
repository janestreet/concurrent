open Base
open Await
module Expert = Multicore

let rec spawn : type a. (a, unit) Concurrent.spawn_fn =
  fun scope ~f ->
  match
    Expert.spawn
      (fun token ->
        (* Always record backtraces in concurrent threads *)
        Stdlib.Printexc.record_backtrace true;
        Scope.Token.use token ~f:(fun terminator scope ->
          with_concurrent terminator ~f:(fun [@inline] c -> f scope () c [@nontail])
          [@nontail])
        [@nontail])
      (Scope.add scope)
  with
  | Spawned -> ()
  | Failed (token, exn, bt) ->
    Scope.Token.drop token;
    Exn.raise_with_original_backtrace exn bt

and[@inline] create await = exclave_
  (Concurrent.create [@mode portable])
    await
    ~scheduler:((Concurrent.Scheduler.create [@mode portable]) ~spawn)

and[@inline] with_concurrent
  : 'r.
  Terminator.t @ local -> f:(unit Concurrent.t @ local portable -> 'r) @ local once -> 'r
  =
  fun terminator ~f ->
  Await_blocking.with_await terminator ~f:(fun await -> f (create await) [@nontail])
  [@nontail]
;;

let scheduler = (Concurrent.Scheduler.create [@mode portable]) ~spawn

let[@inline] in_scope await scope = exclave_
  (Concurrent.Spawn.create [@mode portable]) ~scope (create await)
;;

let[@inline] spawn_into spawn = exclave_
  in_scope (Concurrent.Spawn.await spawn) (Concurrent.Spawn.scope spawn)
;;
