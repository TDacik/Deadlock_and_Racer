(* Entry point of DeadlockF.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module Result = LocksetAnalysis.Result

let show_version () =
  Format.printf "%s\n" Deadlock.version

let report locksets deadlocks = match deadlocks with
  | [] -> Deadlock.result "No deadlock found"
  | xs -> List.iter (fun dl -> Deadlock.result "%s" (DeadlockAnalysis.Deadlock.report dl)) xs

let main () =
  Profiler.add "Start";
  Deadlock.debug "Start";

  (* Select mode of value analysis and instantiate all other components which are using it *)
  let module ValueAnalysis = (val Core.init ())  in
  let module ThreadAnalysis = ThreadAnalysis.Make(ValueAnalysis) in
  let module LocksetAnalysis = LocksetAnalysis.Make(ValueAnalysis) in

  let thread_res = ThreadAnalysis.compute () in
  Profiler.add "Thread analysis";

  let lockset_res = LocksetAnalysis.compute thread_res in
  Profiler.add "Lockset analysis";

  let deadlocks = DeadlockAnalysis.compute lockset_res in
  Profiler.add "Deadlock analysis";

  (* Register results for GUI *)
  CC_results.locksets := Some lockset_res;
  CC_results.threads := Some thread_res;

  report lockset_res deadlocks;

  if not @@ Deadlock.JsonOutput.is_default () then
    DeadlockAnalysis.Result.out_json deadlocks @@ Deadlock.JsonOutput.get ()
  else ();
  Profiler.finish ()

let run () =
  if Deadlock.Version.get () then show_version ()
  else if Deadlock.Enabled.get () then main ()
  else ()

let () =
  Boot.Main.extend run
