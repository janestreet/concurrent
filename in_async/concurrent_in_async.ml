open Async
open Await

[@@@alert "-experimental"]

let scheduler ?monitor ?priority () =
  let open struct
    type spawn =
      { spawn : 'a. ('a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.spawn_fn }
    [@@unboxed]
  end in
  let rec spawn : type a. (a, Capsule.Initial.k Capsule.Access.boxed) Concurrent.spawn_fn =
    fun scope ~f ->
    let token = Scope.add scope in
    schedule ?monitor ?priority (fun () ->
      try
        Await_in_async.Expert.with_await Terminator.never ~f:(fun await ->
          Scope.Token.use token ~f:(fun terminator task_handle ->
            let spawn = (Capsule.Initial.Data.wrap [@mode local]) { spawn } in
            Capsule.Expert.Password.with_current
              (Capsule.Access.unbox Capsule.Initial.access)
              (fun password ->
                 let scheduler =
                   (Concurrent.Scheduler.create [@alloc stack] [@mode portable])
                     ~spawn:(fun (type b) (scope : b Scope.t @ local) ~f ->
                       Capsule.Expert.Data.Local.iter ~password spawn ~f:(fun { spawn } ->
                         spawn scope ~f)
                       [@nontail])
                 in
                 let concurrent =
                   (Concurrent.create [@mode portable])
                     (Await.with_terminator await terminator)
                     ~scheduler
                 in
                 f task_handle Capsule.Expert.initial concurrent [@nontail])
            [@nontail])
          [@nontail])
        [@nontail]
      with
      | exn -> Monitor.send_exn (Monitor.current ()) exn)
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
