open! Base
open Async
open Await

let send_exn = Capsule.Initial.Data.wrap Monitor.send_exn

let scheduler ?monitor ?priority () =
  let open struct
    type spawn =
      { spawn :
          ('r : value_or_null) 'a.
          ('r, 'a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.Scheduler.spawn_fn
        @@ unyielding
      }
    [@@unboxed]
  end in
  let rec spawn
    : type (r : value_or_null) a.
      (r, a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.Scheduler.spawn_fn
    =
    fun scope #{ fn; affinity = _; name = _ } r ->
    let token = Concurrent.Scope.add scope in
    schedule ?monitor ?priority (fun () ->
      try
        Await_in_async.Expert.with_await Terminator.unkillable ~f:(fun await ->
          Concurrent.Scope.Token.use token ~f:(fun terminator task_handle ->
            let spawn = (Capsule.Initial.Data.wrap [@mode local]) { spawn } in
            Capsule.Prim.Password.with_current Capsule.Initial.access (fun password ->
              let scheduler =
                (Concurrent.Scheduler.create [@alloc stack] [@mode portable])
                  ~spawn:
                    (fun
                      (type (s : value_or_null) b)
                      (scope : b Concurrent.Scope.t @ local)
                      task
                      (r : s)
                    ->
                    Capsule.Prim.access ~password ~f:(fun access ->
                      let { spawn } = Capsule.Prim.Data.Local.unwrap ~access spawn in
                      spawn scope task r [@nontail])
                    [@nontail])
              in
              let concurrent =
                (Concurrent.create [@mode portable])
                  (Await.with_terminator await terminator)
                  ~scheduler
              in
              fn
                task_handle
                (Capsule.Access.box Capsule.Initial.access)
                concurrent
                r [@nontail])
            [@nontail])
          [@nontail])
        [@nontail]
      with
      | exn -> Monitor.send_exn (Monitor.current ()) exn);
    Spawned
  in
  Concurrent.Scheduler.create ~spawn
;;

let[@inline] schedule_with_concurrent ?monitor ?priority terminator ~f =
  Await_in_async.schedule_with_await
    ?monitor
    ?priority
    (Terminator.Expert.globalize terminator)
    ~f:(fun await ->
      let concurrent =
        Concurrent.create await ~scheduler:(scheduler ?monitor ?priority ())
      in
      f concurrent [@nontail])
;;

let[@inline] spawn_deferred s ~f =
  Concurrent.spawn_onto_initial s ~f:(fun s c t ->
    Await_in_async.await_deferred
      (Concurrent.await t)
      ((f [@inlined hint]) s c t) [@nontail])
  [@nontail]
;;

let spawn_join ?(monitor = Monitor.current ()) sched ~f =
  let scope =
    let execution_context =
      Async_kernel_scheduler.current_execution_context () |> Capsule.Initial.Data.wrap
    in
    let monitor = { aliased = monitor } |> Capsule.Initial.Data.wrap in
    Concurrent.Scope.Global.create () ~on_exit:(fun _scope maybe_exn ->
      Or_null.iter maybe_exn ~f:(fun (exn, bt) ->
        Async_kernel_scheduler.portable_enqueue_job
          execution_context
          (Capsule.Data.create (fun () : _ ->
             fun #(access, { aliased = monitor }) ->
             let send_exn = Capsule.Data.unwrap ~access send_exn in
             send_exn monitor exn ~backtrace:(`This bt)))
          monitor))
  in
  let ivar = Ivar.Portable.create () in
  Concurrent.Scheduler.spawn
    sched
    scope
    (Concurrent.task (fun _ ctx concurrent ->
       let result = f ctx concurrent in
       Ivar.Portable.fill_if_empty ivar { portended = result }));
  Ivar.Portable.read ivar
  |> Deferred.map ~f:(fun { portended } -> Modes.Contended.cross portended)
;;

module Portable = struct
  let with_await = Capsule.Initial.Data.wrap Await_in_async.Expert.with_await
  let monitor_send_exn = Capsule.Initial.Data.wrap Monitor.send_exn
  let monitor_current = Capsule.Initial.Data.wrap Monitor.current

  let scheduler () =
    let execution_context =
      Async_kernel_scheduler.current_execution_context () |> Capsule.Initial.Data.wrap
    in
    let rec spawn
      : type (r : value_or_null) a.
        (r, a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.Scheduler.spawn_fn
      =
      fun scope #{ fn; affinity = _; name = _ } r ->
      let token = Concurrent.Scope.add scope in
      Async_kernel_scheduler.portable_enqueue_job
        execution_context
        (Capsule.Prim.Data.create_once (fun () : _ ->
           fun #(access, token) ->
           let with_await = Capsule.Data.unwrap ~access with_await in
           try
             with_await Terminator.unkillable ~f:(fun await ->
               Concurrent.Scope.Token.use token ~f:(fun terminator task_handle ->
                 let concurrent =
                   (Concurrent.create [@mode portable])
                     (Await.with_terminator await terminator)
                     ~scheduler:((Concurrent.Scheduler.create [@mode portable]) ~spawn)
                 in
                 fn task_handle (Capsule.Access.box access) concurrent r [@nontail])
               [@nontail])
             [@nontail]
           with
           | exn ->
             (Capsule.Data.unwrap ~access monitor_send_exn)
               ((Capsule.Data.unwrap ~access monitor_current) ())
               exn))
        (Capsule.Prim.Data.create_unique (fun () -> token));
      Spawned
    in
    (Concurrent.Scheduler.create [@mode portable]) ~spawn
  ;;
end
