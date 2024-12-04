open ValueAnalysis_sig

include Core0

module Path = Frama_c_kernel.Filepath.Normalized

let value_analysis () = match ValueAnalysisType.get () with
  | `Syntactic -> (module Syntactic : VALUE_ANALYSIS)
  | `Eva -> (module EVA : VALUE_ANALYSIS)
  | `Alias -> (module AliasF : VALUE_ANALYSIS)

let get_paths () =
  let paths = Core0.ConcurrencyModels.get () in
  match paths with
  | [] ->
    let dir = Share.get_dir "models" in
    let dir_path = Format.asprintf "%a" Path.pp_abs dir in
    let models = Array.to_list @@ Sys.readdir dir_path in
    List.map (fun f -> Share.get_file @@ "models/" ^ f) models
  | _ -> paths

let load_models () =
  get_paths ()
  |> List.map (fun f -> Format.asprintf "%a" Path.pp_abs f)
  |> List.iter ConcurrencyModel_load.load

let add_builtins () = ()

(** API *)

let init () =
  load_models ();
  add_builtins ();
  (*
  ConcurrencyModel_data.print ();
  (if NonDeterminization.get () then Nondetermizer.run ()); *)
  (*Inline.run ();*)
  Imprecision.check ();
  let module ValueAnalysis = (val value_analysis ()) in
  ValueAnalysis.check_imprecision ();
  (module ValueAnalysis : VALUE_ANALYSIS)

(** Following is intended only for testing thread analysis without running Deadlock/Racer. *)

let run () =
  if JustThreadAnalysis.get () then
    let module ValueAnalysis = (val init ()) in
    let module ThreadAnalysis = ThreadAnalysis.Make(ValueAnalysis) in
    let threads =  ThreadAnalysis.compute () in
    CC_results.threads := Some threads
  else ();

  if not @@ Core0.JsonOutput.is_default () then
    CC_results.out_json @@ Core0.JsonOutput.get ()
  else ()

let () =
  Boot.Main.extend run
