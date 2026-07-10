open Base
open Await
module Scope = Await.Scope
open Types

include%template struct
  type 'a modality = { modality : 'a @@ many } [@@mode unique] [@@unboxed]
  type 'a modality = { modality : 'a @@ aliased many } [@@mode aliased] [@@unboxed]

  external unwrap_iarray
    :  ('a modality[@mode u]) iarray @ contended portable unique
    -> ('a iarray modality[@mode u]) @ contended portable unique
    @@ portable
    = "%identity"
  [@@mode u = (unique, aliased)]
end

type 'concurrent_ctx t = 'concurrent_ctx concurrent =
  { await : Await.t
  ; scheduler : 'concurrent_ctx scheduler
  }
[@@deriving fields ~getters]

let sync t = exclave_ Await.sync (await t)

module Spawn = struct
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

[@@@mode.default u = (unique, aliased)]

let with_scope { await; scheduler } b ~f =
  (Scope.with_ await b ~f:(fun await scope : (_ modality[@mode u]) ->
     { modality = f ((into_scope [@mode p]) { await; scheduler } scope) }))
    .modality
;;]

module Task0 = struct
  type 'f t = 'f task =
    #{ fn : 'f
     ; name : string or_null
     ; affinity : int or_null
     }

  let[@inline] map_fn #{ fn; name; affinity } f = #{ fn = f fn; name; affinity }
end

let[@inline] task ?name ?affinity f : _ @ once =
  #{ Task0.fn = f; name = Or_null.of_option name; affinity = Or_null.of_option affinity }
;;

let[@inline] nonportable_task ?name ?affinity f =
  #{ Task0.fn = f; name = Or_null.of_option name; affinity = Or_null.of_option affinity }
;;

module Scheduler = struct
  type ('resource : value_or_null, 'scope_ctx, 'concurrent_ctx) spawn_fn =
    ('resource, 'scope_ctx, 'concurrent_ctx) Types.spawn_fn

  type 'ctx t = 'ctx scheduler =
    { spawn :
        ('resource : value_or_null) 'scope_ctx. ('resource, 'scope_ctx, 'ctx) spawn_fn
      @@ unyielding
    }
  [@@unboxed] [@@deriving fields ~getters]

  type packed = T : 'ctx t -> packed [@@unboxed]

  let%template create
    ~(spawn :
        ('resource : value_or_null) 'scope_ctx. ('resource, 'scope_ctx, _) spawn_fn
        @ l unyielding)
    =
    { spawn } [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)] [@@mode p = (portable, nonportable)]
  ;;

  let%template rec with_context (t : _ t) ctx =
    (let spawn : type (r : value_or_null) s. (r, s, _) spawn_fn =
       fun scope task resource ->
       t.spawn
         scope
         (Task0.map_fn task (fun f ->
            fun [@inline] h ctx1 conc r ->
            ctx ctx1 conc ~f:(fun ctx2 ->
              f
                h
                ctx2
                { conc with scheduler = (with_context [@alloc stack]) conc.scheduler ctx }
                r [@nontail])
            [@nontail]))
         resource
     in
     { spawn })
    [@exclave_if_stack a]
  [@@alloc a @ l = (stack_local, heap_global)]
  ;;

  let spawn_daemon' t scope task =
    match
      spawn
        t
        scope
        (Task0.map_fn task (fun fn ->
           fun [@inline] h b t () ->
           let #(s, c) = Scope.Task_handle.become_daemon h in
           fn s c b t [@nontail]))
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;

  let spawn_daemon t scope task =
    match
      spawn
        t
        scope
        (Task0.map_fn task (fun fn ->
           fun [@inline] h b t () ->
           let #(s, c) = Scope.Task_handle.become_daemon h in
           ignore (fn s c b t : unit Or_canceled.t)))
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;

  let spawn_with t scope task r =
    spawn
      t
      scope
      (Task0.map_fn task (fun fn ->
         fun [@inline] h b t r -> fn (Scope.Task_handle.into_scope h) b t r [@nontail]))
      r [@nontail]
  ;;

  let spawn t scope task =
    match
      spawn_with
        t
        scope
        (Task0.map_fn task (fun fn -> fun [@inline] s c t () -> fn s c t))
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  ;;
end

let with_context t ctx = exclave_
  { t with scheduler = (Scheduler.with_context [@alloc stack]) t.scheduler ctx }
;;

(** Module for (unsafely!) recording the result(s) of a (set of) concurrent task(s) in a
    scope, and accessing those result(s) after the scope ends.

    This module is very unsafe! See SAFETY comments on each function for more of the
    contract callers later in this module must follow *)
module Unsafe_result : sig @@ portable
  type 'a t : value mod contended portable

  val make : unit -> 'a t @ unique

  (** SAFETY: This function is unsafe to call without ensuring that no other threads are
      calling either it or [racy_get]. *)
  val racy_fill : 'a t -> 'a @ contended portable unique -> unit

  (** SAFETY: This function is unsafe to call without ensuring that [racy_fill] has been
      called {i before} it is called. It is also unsafe to call on the same
      [Unsafe_result.t] more than once. *)
  val racy_get : 'a t -> 'a @ contended portable unique

  module Array : sig
    type ('a : value mod non_float) t : value mod contended portable

    val make : len:int -> 'a t

    (** SAFETY: This function is unsafe to call:

        - With an index that is out-of-bounds for the array
        - Concurrently with any other threads calling [racy_fill] on the same index, or
          calling [racy_get] at all
        - With the return value of [racy_get] accessible anywhere *)
    val racy_fill : 'a t -> int -> 'a @ contended portable unique -> unit

    (** SAFETY: This function is unsafe to call without ensuring that {i all} indices of
        the array have been filled by [racy_fill] {i before} it is called. It is also
        unsafe to call on the same [Unsafe_result.Array.t] more than once. *)
    val racy_get_promise_no_mutation : 'a t -> 'a Iarray.t @ contended portable unique
  end
end = struct
  type 'a t : value mod contended portable =
    { mutable contents : 'a or_null @@ contended portable }
  [@@unsafe_allow_any_mode_crossing (* See SAFETY comments in the interface *)]

  let make () = { contents = Null }
  let racy_fill t a = t.contents <- This a

  external unsafe_assume_init_promise_no_mutation
    :  'a or_null @ contended portable
    -> 'a @ contended portable
    @@ portable
    = "%identity"

  let racy_get t =
    (* SAFETY: [racy_fill] stored a unique result, and [racy_get] is unsafe to call more
       than once. *)
    (Obj.magic_unique [@mode portable contended])
      (unsafe_assume_init_promise_no_mutation t.contents)
  ;;

  module Array = struct
    type ('a : value mod non_float) t : value mod contended portable =
      { array : 'a portended or_null Array.t }
    [@@unboxed]
    [@@unsafe_allow_any_mode_crossing (* See SAFETY comments in the interface *)]

    let make (type a : value mod non_float) ~len =
      { array = Array.init len ~f:(fun _ : a portended or_null -> Null) }
    ;;

    let racy_fill { array } i a = Array.unsafe_set array i (This { portended = a })

    external unsafe_assume_init
      :  'a t
      -> 'a Iarray.t @ contended portable
      @@ portable
      = "%array_to_iarray"

    let racy_get_promise_no_mutation t =
      (* SAFETY: [racy_fill] stored a unique result, and [racy_get_promise_no_mutation] is
         unsafe to call more than once. *)
      (Obj.magic_unique [@mode portable contended]) (unsafe_assume_init t)
    ;;
  end
end

module Task = struct
  include Task0

  let spawn { scope; concurrent } task = Scheduler.spawn concurrent.scheduler scope task

  let spawn_with { scope; concurrent } task r =
    Scheduler.spawn_with concurrent.scheduler scope task r
  ;;

  let spawn_daemon { scope; concurrent } task =
    Scheduler.spawn_daemon concurrent.scheduler scope task
  ;;

  let spawn_daemon' { scope; concurrent } task =
    Scheduler.spawn_daemon' concurrent.scheduler scope task
  ;;

  [%%template
  [@@@mode.default p = (portable, nonportable)]
  [@@@mode.default u = (unique, aliased)]

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
     - We don't call [racy_get_promise_no_mutation] until after the scope is finished
  *)

  let[@inline] racy_wrap_task result (task : _ t) =
    map_fn task (fun fn ->
      fun [@inline] s c t ->
      Unsafe_result.racy_fill
        result
        ({ modality = (fn [@inlined hint]) s c t } : (_ modality[@mode u])))
  ;;

  let spawn_join t b task =
    let result = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p u]) result task));
    (Unsafe_result.racy_get result).modality
  ;;

  let spawn_join2 t b task1 task2 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p u]) result1 task1);
      spawn s ((racy_wrap_task [@mode p u]) result2 task2));
    #((Unsafe_result.racy_get result1).modality, (Unsafe_result.racy_get result2).modality)
  ;;

  let spawn_join3 t b task1 task2 task3 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p u]) result1 task1);
      spawn s ((racy_wrap_task [@mode p u]) result2 task2);
      spawn s ((racy_wrap_task [@mode p u]) result3 task3));
    #( (Unsafe_result.racy_get result1).modality
     , (Unsafe_result.racy_get result2).modality
     , (Unsafe_result.racy_get result3).modality )
  ;;

  let spawn_join4 t b task1 task2 task3 task4 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    let result4 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p u]) result1 task1);
      spawn s ((racy_wrap_task [@mode p u]) result2 task2);
      spawn s ((racy_wrap_task [@mode p u]) result3 task3);
      spawn s ((racy_wrap_task [@mode p u]) result4 task4));
    #( (Unsafe_result.racy_get result1).modality
     , (Unsafe_result.racy_get result2).modality
     , (Unsafe_result.racy_get result3).modality
     , (Unsafe_result.racy_get result4).modality )
  ;;

  let spawn_join5 t b task1 task2 task3 task4 task5 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    let result4 = Unsafe_result.make () in
    let result5 = Unsafe_result.make () in
    (with_scope [@mode p]) t b ~f:(fun s ->
      spawn s ((racy_wrap_task [@mode p u]) result1 task1);
      spawn s ((racy_wrap_task [@mode p u]) result2 task2);
      spawn s ((racy_wrap_task [@mode p u]) result3 task3);
      spawn s ((racy_wrap_task [@mode p u]) result4 task4);
      spawn s ((racy_wrap_task [@mode p u]) result5 task5));
    #( (Unsafe_result.racy_get result1).modality
     , (Unsafe_result.racy_get result2).modality
     , (Unsafe_result.racy_get result3).modality
     , (Unsafe_result.racy_get result4).modality
     , (Unsafe_result.racy_get result5).modality )
  ;;

  let spawn_join_n t b ~n ~f =
    if n = 0
    then [::]
    else (
      let results = Unsafe_result.Array.make ~len:n in
      (with_scope [@mode p]) t b ~f:(fun s ->
        for i = 0 to n - 1 do
          let task = f i in
          spawn
            s
            (Task0.map_fn task (fun fn ->
               fun [@inline] s c t ->
               let result =
                 ({ modality = (fn [@inlined hint]) s c t } : (_ modality[@mode u]))
               in
               Unsafe_result.Array.racy_fill results i result))
        done);
      ((unwrap_iarray [@mode u])
         (Unsafe_result.Array.racy_get_promise_no_mutation results))
        .modality)
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
          let task = f a in
          spawn
            s
            (Task0.map_fn task (fun fn ->
               fun [@inline] s c t ->
               let result =
                 ({ modality = (fn [@inlined hint]) s c t } : (_ modality[@mode u]))
               in
               Unsafe_result.Array.racy_fill results idx result))
        done);
      ((unwrap_iarray [@mode u])
         (Unsafe_result.Array.racy_get_promise_no_mutation results))
        .modality)
  ;;

  type ('s, 'c) race =
    { wrap_task :
        'r.
        ('s Scope.t @ local
         -> (Cancellation.t @ local
             -> 'c @ local
             -> 'c concurrent @ local portable
             -> 'r Or_canceled.t @ contended portable u)
            @ local once)
          t
        @ once portable
        -> ('r Or_canceled.t modality[@mode u]) Unsafe_result.t @ portable
        -> ('s Scope.t @ local
            -> ('c @ local -> ('c concurrent @ local portable -> unit) @ local once)
               @ local once)
             t
           @ once portable
    }
  [@@unboxed]

  let[@inline] with_race t b ~f =
    Cancellation.with_ (fun cancel ->
      let cancel = Cancellation.Expert.globalize cancel in
      let wrap_task task result =
        Task0.map_fn task (fun fn ->
          fun [@inline] s c t ->
          let res =
            ({ modality = (fn [@inlined hint]) s cancel c t } : (_ modality[@mode u]))
          in
          Cancellation.Source.cancel (Cancellation.source cancel |> Or_null.value_exn);
          Unsafe_result.racy_fill result res)
      in
      (with_scope [@mode p]) t b ~f:(fun s : unit -> f s { wrap_task } [@nontail])
      [@nontail])
    [@nontail]
  ;;

  let race2 t b task1 task2 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    (with_race [@mode p u]) t b ~f:(fun s { wrap_task } ->
      spawn s (wrap_task task1 result1);
      spawn s (wrap_task task2 result2));
    #((Unsafe_result.racy_get result1).modality, (Unsafe_result.racy_get result2).modality)
  ;;

  let race3 t b task1 task2 task3 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    (with_race [@mode p u]) t b ~f:(fun s { wrap_task } ->
      spawn s (wrap_task task1 result1);
      spawn s (wrap_task task2 result2);
      spawn s (wrap_task task3 result3));
    #( (Unsafe_result.racy_get result1).modality
     , (Unsafe_result.racy_get result2).modality
     , (Unsafe_result.racy_get result3).modality )
  ;;

  let race4 t b task1 task2 task3 task4 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    let result4 = Unsafe_result.make () in
    (with_race [@mode p u]) t b ~f:(fun s { wrap_task } ->
      spawn s (wrap_task task1 result1);
      spawn s (wrap_task task2 result2);
      spawn s (wrap_task task3 result3);
      spawn s (wrap_task task4 result4));
    #( (Unsafe_result.racy_get result1).modality
     , (Unsafe_result.racy_get result2).modality
     , (Unsafe_result.racy_get result3).modality
     , (Unsafe_result.racy_get result4).modality )
  ;;

  let race5 t b task1 task2 task3 task4 task5 =
    let result1 = Unsafe_result.make () in
    let result2 = Unsafe_result.make () in
    let result3 = Unsafe_result.make () in
    let result4 = Unsafe_result.make () in
    let result5 = Unsafe_result.make () in
    (with_race [@mode p u]) t b ~f:(fun s { wrap_task } ->
      spawn s (wrap_task task1 result1);
      spawn s (wrap_task task2 result2);
      spawn s (wrap_task task3 result3);
      spawn s (wrap_task task4 result4);
      spawn s (wrap_task task5 result5));
    #( (Unsafe_result.racy_get result1).modality
     , (Unsafe_result.racy_get result2).modality
     , (Unsafe_result.racy_get result3).modality
     , (Unsafe_result.racy_get result4).modality
     , (Unsafe_result.racy_get result5).modality )
  ;;]

  [%%template
  [@@@mode.default p = (portable, nonportable)]

  let iter t iarr c ~f =
    (with_scope [@mode p]) t c ~f:(fun s ->
      for idx = 0 to Iarray.length iarr - 1 do
        let a = (Iarray.unsafe_get [@mode portable]) iarr idx in
        let task = f a in
        spawn s task
      done)
  ;;]

  let spawn_nonportable ~access s t =
    let #{ fn; affinity; name } = t in
    spawn
      s
      #{ Task0.fn =
           (let fn = Capsule.Prim.Data.wrap_once ~access fn in
            fun [@inline] ctx access conc ->
              let fn =
                Capsule.Prim.Data.unwrap_once ~access:(Capsule.Access.unbox access) fn
              in
              fn ctx access conc)
       ; affinity
       ; name
       }
  ;;

  let spawn_onto_initial s t = spawn_nonportable ~access:Capsule.Initial.access s t
end

type packed = T : 'concurrent_ctx concurrent -> packed [@@unboxed]

type ('resource : value_or_null) spawn_result = 'resource Types.spawn_result =
  | Spawned
  | Failed of 'resource * exn @@ aliased many * Backtrace.t @@ aliased many

let spawn s ~f = Task.spawn s (task f)
let spawn_with s ~f r = Task.spawn_with s (task f) r
let spawn_daemon s ~f = Task.spawn_daemon s (task f)
let spawn_daemon' s ~f = Task.spawn_daemon' s (task f)

let spawn_nonportable ~access s ~f =
  let f = Capsule.Prim.Data.wrap_once ~access f in
  spawn s ~f:(fun ctx access conc ->
    let f = Capsule.Prim.Data.unwrap_once ~access:(Capsule.Access.unbox access) f in
    f ctx access conc [@nontail])
  [@nontail]
;;

let spawn_onto_initial s ~f = spawn_nonportable ~access:Capsule.Initial.access s ~f

[%%template
[@@@mode.default p = (portable, nonportable)]
[@@@mode.default u = (unique, aliased)]

let spawn_join t c ~f = (Task.spawn_join [@mode p u]) t c (task f)
let spawn_join2 t c f1 f2 = (Task.spawn_join2 [@mode p u]) t c (task f1) (task f2)

let spawn_join3 t c f1 f2 f3 =
  (Task.spawn_join3 [@mode p u]) t c (task f1) (task f2) (task f3)
;;

let spawn_join4 t c f1 f2 f3 f4 =
  (Task.spawn_join4 [@mode p u]) t c (task f1) (task f2) (task f3) (task f4)
;;

let spawn_join5 t c f1 f2 f3 f4 f5 =
  (Task.spawn_join5 [@mode p u]) t c (task f1) (task f2) (task f3) (task f4) (task f5)
;;

let spawn_join_n t c ~n ~f =
  (Task.spawn_join_n [@mode p u]) t c ~n ~f:(fun [@inline] n ->
    task (fun [@inline] s c t -> (f [@inlined hint]) s c t n))
;;

let map t l c ~f =
  (Task.map [@mode p u]) t l c ~f:(fun a ->
    task (fun [@inline] s c t -> (f [@inlined hint]) s c t a))
;;

let race2 t c f1 f2 = (Task.race2 [@mode p u]) t c (task f1) (task f2)
let race3 t c f1 f2 f3 = (Task.race3 [@mode p u]) t c (task f1) (task f2) (task f3)

let race4 t c f1 f2 f3 f4 =
  (Task.race4 [@mode p u]) t c (task f1) (task f2) (task f3) (task f4)
;;

let race5 t c f1 f2 f3 f4 f5 =
  (Task.race5 [@mode p u]) t c (task f1) (task f2) (task f3) (task f4) (task f5)
;;]

[%%template
[@@@mode.default p = (portable, nonportable)]

let iter t l c ~f =
  (Task.iter [@mode p]) t l c ~f:(fun a ->
    task (fun [@inline] s c t -> (f [@inlined hint]) s c t a))
;;]
