(* Entry point of RacerF.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let show_version () =
  Format.printf "%s\n" Racer.version

let main () =
  Profiler.add "Start";
  Racer.feedback "Racer is running";

  (* Select mode of value analysis and instantiate all other components which are using it *)
  let module ValueAnalysis = (val Core.init ()) in
  let module ThreadAnalysis = ThreadAnalysis.Make(ValueAnalysis) in
  let module ParallelAnalysis = ParallelAnalysis.Make(ValueAnalysis) in
  let module LocksetAnalysis = LocksetAnalysis.Make(ValueAnalysis) in
  let module RaceAnalysis = RaceAnalysis.Make(ValueAnalysis) in

  (* TODO: some caching *)
  let thread_graph = ThreadAnalysis.compute () in
  Profiler.add "Thread analysis";

  let lockset_res = LocksetAnalysis.compute thread_graph in
  Profiler.add "Lockset analysis";

  let parallel_res = ParallelAnalysis.compute thread_graph in
  Profiler.add "May-run-in-parallel";

  let res = RaceAnalysis.compute thread_graph parallel_res lockset_res in
  RaceAnalysis.Result.report res;
  Profiler.add "Race analysis";

  PostProcessing.run res;

  if not @@ Racer.JsonOutput.is_default () then
    RaceAnalysis.Result.out_json res @@ Racer.JsonOutput.get ()
  else ();

  if not @@ Racer.SVWitnessPath.is_default () && RaceAnalysis.Result.has_must_race res then
    let race = RaceAnalysis.Result.choose res in
    WitnessGenerator1.dump_trace race @@ Racer.SVWitnessPath.get ()
  else ();

  Profiler.finish ();
  if Racer.Profile.get() then Profiler.report () else ()

let run () =
  if Racer.Version.get () then show_version ()
  else if Racer.Enabled.get () then main ()
  else ()

let () = Boot.Main.extend run
