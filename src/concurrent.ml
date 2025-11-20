open Base
open Await
module Scope = Scope

type 'resource spawn_result =
  | Spawned
  | Failed of 'resource * exn @@ aliased many * Backtrace.t @@ aliased many

type ('resource, 'scope_ctx, 'concurrent_ctx) spawn_fn =
  'scope_ctx Scope.t @ local
  -> f:
       ('scope_ctx Scope.Task_handle.t @ local unique
        -> 'concurrent_ctx @ local
        -> 'concurrent_ctx t @ local portable
        -> 'resource @ contended once portable unique
        -> unit)
     @ once portable
  -> 'resource @ contended once portable unique
  -> 'resource spawn_result @ contended once portable unique

and 'concurrent_ctx t =
  { await : Await.t
  ; scheduler : 'concurrent_ctx scheduler
  }
[@@deriving fields ~getters]

and 'ctx scheduler =
  { spawn : 'resource 'scope_ctx. ('resource, 'scope_ctx, 'ctx) spawn_fn }
[@@unboxed]

and ('scope_ctx, 'concurrent_ctx) spawn =
  { scope : 'scope_ctx Scope.t
  ; concurrent : 'concurrent_ctx t
  }

type packed = T : 'concurrent_ctx t -> packed [@@unboxed]

module Scheduler = struct
  type 'ctx t = 'ctx scheduler =
    { spawn : 'resource 'scope_ctx. ('resource, 'scope_ctx, 'ctx) spawn_fn }
  [@@unboxed]

  type packed = T : 'ctx t -> packed [@@unboxed]

  let%template create
    ~(spawn : 'resource 'scope_ctx. ('resource, 'scope_ctx, _) spawn_fn @ l)
    =
    { spawn } [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)] [@@mode p = (portable, nonportable)]
  ;;

  let spawn_daemon' t scope ~f =
    match
      spawn t scope () ~f:(fun [@inline] h b t () ->
        let #(s, c) = Scope.Task_handle.become_daemon h in
        f s c b t [@nontail])
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;

  let spawn_daemon t scope ~f =
    match
      spawn t scope () ~f:(fun [@inline] h b t () ->
        let #(s, c) = Scope.Task_handle.become_daemon h in
        ignore (f s c b t : unit Or_canceled.t))
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;

  let spawn_with { spawn; _ } scope ~f r =
    spawn scope r ~f:(fun [@inline] h b t r ->
      f (Scope.Task_handle.into_scope h) b t r [@nontail])
  ;;

  let spawn t scope ~f =
    match spawn_with t scope ~f:(fun s c t () -> f s c t) () with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;
end

module Spawn = struct
  type 'concurrent_ctx concurrent = 'concurrent_ctx t

  type ('scope_ctx, 'concurrent_ctx) t = ('scope_ctx, 'concurrent_ctx) spawn =
    { scope : 'scope_ctx Scope.t
    ; concurrent : 'concurrent_ctx concurrent
    }
  [@@deriving fields ~getters]

  type 'scope_ctx packed = T : ('scope_ctx, 'concurrent_ctx) t -> 'scope_ctx packed
  [@@unboxed]

  let%template create concurrent ~scope = exclave_ { scope; concurrent }
  [@@mode p = (portable, nonportable)]
  ;;

  let with_scheduler { scope; concurrent } scheduler = exclave_
    { scope; concurrent = { concurrent with scheduler } }
  ;;

  let await t = exclave_ await (concurrent t)
  let scheduler t = exclave_ scheduler (concurrent t)
  let context { scope; _ } = exclave_ Scope.context scope
  let terminator { scope; _ } = exclave_ Scope.terminator scope
end

[%%template
[@@@mode.default p = (portable, nonportable)]

let create await ~scheduler = exclave_ { await; scheduler }
let into_scope concurrent scope = exclave_ Spawn.(create [@mode p]) concurrent ~scope

let with_scope t b ~f =
  Scope.with_ t.await b ~f:(fun scope -> f ((into_scope [@mode p]) t scope) [@nontail])
  [@nontail]
;;]

let spawn (type a b) ({ scope; concurrent } : (a, b) Spawn.t @ local) ~f =
  Scheduler.spawn concurrent.scheduler scope ~f
;;

let spawn_with (type a b) ({ scope; concurrent } : (a, b) Spawn.t @ local) ~f r =
  Scheduler.spawn_with concurrent.scheduler scope ~f r
;;

let spawn_daemon' (type a b) ({ scope; concurrent } : (a, b) Spawn.t @ local) ~f =
  Scheduler.spawn_daemon' concurrent.scheduler scope ~f
;;

let spawn_daemon (type a b) ({ scope; concurrent } : (a, b) Spawn.t @ local) ~f =
  Scheduler.spawn_daemon concurrent.scheduler scope ~f
;;

let spawn_nonportable ~access s ~f =
  let f = Capsule.Expert.Data.wrap_once ~access f in
  spawn s ~f:(fun ctx access conc ->
    let f = Capsule.Expert.Data.unwrap_once ~access:(Capsule.Access.unbox access) f in
    f ctx access conc [@nontail])
  [@nontail]
;;

let spawn_onto_initial s ~f =
  spawn_nonportable ~access:(Capsule.Access.unbox Capsule.Expert.initial) s ~f
;;

(** Module for (unsafely!) recording the result(s) of a (set of) concurrent task(s) in a
    scope, and accessing those result(s) after the scope ends.

    This module is very unsafe! See SAFETY comments on each function for more of the
    contract callers later in this module must follow *)
module Unsafe_result : sig @@ portable
  type 'a t : value mod contended portable

  val make : unit -> 'a t

  (** SAFETY: This function is unsafe to call without ensuring that no other threads are
      calling either it or [racy_get]. *)
  val racy_fill : 'a t -> 'a @ contended portable -> unit

  (** SAFETY: This function is unsafe to call without ensuring that [racy_fill] has been
      called {i before} it is called. *)
  val racy_get : 'a t -> 'a @ contended portable

  module Array : sig
    type 'a t : value mod contended portable

    val make : len:int -> 'a t

    (** SAFETY: This function is unsafe to call:

        - With an index that is out-of-bounds for the array
        - Concurrently with any other threads calling [racy_fill] on the same index, or
          calling [racy_get] at all *)
    val racy_fill : 'a t -> int -> 'a @ contended portable -> unit

    (** SAFETY: This function is unsafe to call without ensuring that {i all} indices of
        the array have been filled by [racy_fill] {i before} it is called *)
    val racy_get : 'a t -> 'a Iarray.t @ contended portable
  end
end = struct
  type 'a t : value mod contended portable =
    { mutable contents : 'a or_null @@ contended portable }
  [@@unsafe_allow_any_mode_crossing (* See SAFETY comments in the interface *)]

  let make () = { contents = Null }
  let racy_fill t a = t.contents <- This a

  external unsafe_assume_init
    :  'a or_null @ contended portable
    -> 'a @ contended portable
    @@ portable
    = "%identity"

  let racy_get t = unsafe_assume_init t.contents

  module Array = struct
    type 'a t : value mod contended portable =
      { array : 'a portended or_null Uniform_array.t }
    [@@unboxed]
    [@@unsafe_allow_any_mode_crossing (* See SAFETY comments in the interface *)]

    let make ~len = { array = Uniform_array.create ~len Null }

    let racy_fill { array } i a =
      Uniform_array.unsafe_set array i (This { portended = a })
    ;;

    external unsafe_assume_init
      :  'a t
      -> 'a Iarray.t @ contended portable
      @@ portable
      = "%obj_magic"

    let racy_get t = unsafe_assume_init t
  end
end

[%%template
[@@@mode.default p = (portable, nonportable)]

(* SAFETY:

   For the following functions, the safety depends on the properties of [with_scope].
   Notably:

   - Each function [spawn]ed into a scope either runs to completion, or raises
   - If any function is [spawn]ed into a scope, the entire scope raises
   - [with_scope] does not return until all functions [spawn]ed into the scope return or
     raise.

   In each spawn_join function, iter, and map, we must ensure:
   - each result (either [Unsafe_result.t] or, in the case of [map],
     [Unsafe_result.Array.t]) is filled within a task spawned into the scope
   - We don't call [racy_get] until after the scope is finished
*)

let spawn_join t b ~f =
  let result = Unsafe_result.make () in
  (with_scope [@mode p]) t b ~f:(fun s ->
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result ((f [@inlined hint]) s c t)));
  Unsafe_result.racy_get result
;;

let spawn_join2 t b f1 f2 =
  let result1 = Unsafe_result.make () in
  let result2 = Unsafe_result.make () in
  (with_scope [@mode p]) t b ~f:(fun s ->
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result1 ((f1 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result2 ((f2 [@inlined hint]) s c t)));
  #(Unsafe_result.racy_get result1, Unsafe_result.racy_get result2)
;;

let spawn_join3 t b f1 f2 f3 =
  let result1 = Unsafe_result.make () in
  let result2 = Unsafe_result.make () in
  let result3 = Unsafe_result.make () in
  (with_scope [@mode p]) t b ~f:(fun s ->
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result1 ((f1 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result2 ((f2 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result3 ((f3 [@inlined hint]) s c t)));
  #( Unsafe_result.racy_get result1
   , Unsafe_result.racy_get result2
   , Unsafe_result.racy_get result3 )
;;

let spawn_join4 t b f1 f2 f3 f4 =
  let result1 = Unsafe_result.make () in
  let result2 = Unsafe_result.make () in
  let result3 = Unsafe_result.make () in
  let result4 = Unsafe_result.make () in
  (with_scope [@mode p]) t b ~f:(fun s ->
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result1 ((f1 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result2 ((f2 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result3 ((f3 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result4 ((f4 [@inlined hint]) s c t)));
  #( Unsafe_result.racy_get result1
   , Unsafe_result.racy_get result2
   , Unsafe_result.racy_get result3
   , Unsafe_result.racy_get result4 )
;;

let spawn_join5 t b f1 f2 f3 f4 f5 =
  let result1 = Unsafe_result.make () in
  let result2 = Unsafe_result.make () in
  let result3 = Unsafe_result.make () in
  let result4 = Unsafe_result.make () in
  let result5 = Unsafe_result.make () in
  (with_scope [@mode p]) t b ~f:(fun s ->
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result1 ((f1 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result2 ((f2 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result3 ((f3 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result4 ((f4 [@inlined hint]) s c t));
    spawn s ~f:(fun s c t -> Unsafe_result.racy_fill result5 ((f5 [@inlined hint]) s c t)));
  #( Unsafe_result.racy_get result1
   , Unsafe_result.racy_get result2
   , Unsafe_result.racy_get result3
   , Unsafe_result.racy_get result4
   , Unsafe_result.racy_get result5 )
;;

let iter t iarr c ~f =
  (with_scope [@mode p]) t c ~f:(fun s ->
    for idx = 0 to Iarray.length iarr - 1 do
      let a = (Iarray.unsafe_get [@mode portable]) iarr idx in
      spawn s ~f:(fun s c t -> (f [@inlined hint]) s c t a)
    done)
;;

let map t iarr c ~f =
  let len = Iarray.length iarr in
  if len = 0
  then [::]
  else (
    let results = Unsafe_result.Array.make ~len in
    (with_scope [@mode p]) t c ~f:(fun s ->
      for idx = 0 to len - 1 do
        let a = (Iarray.unsafe_get [@mode portable]) iarr idx in
        spawn s ~f:(fun s c t ->
          let result = (f [@inlined hint]) s c t a in
          Unsafe_result.Array.racy_fill results idx result)
      done);
    Unsafe_result.Array.racy_get results)
;;

let spawn_join_n t b ~n ~f =
  if n = 0
  then [::]
  else (
    let results = Unsafe_result.Array.make ~len:n in
    (with_scope [@mode p]) t b ~f:(fun s ->
      for i = 0 to n - 1 do
        spawn s ~f:(fun s c t ->
          let result = (f [@inlined hint]) s c t i in
          Unsafe_result.Array.racy_fill results i result)
      done);
    Unsafe_result.Array.racy_get results)
;;]
