(* YAML report.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

open Cil_types
open Cil_datatype

open RaceAnalysis.Result

let path_to_string path = Format.asprintf "%a" Frama_c_kernel.Filepath.pretty path

let yaml_location loc =
  let pos = fst loc in (* TODO: why fst? *)
  `O [
    "file",   `String (path_to_string @@ Filepos.path pos);
    "line",   `Float (float_of_int @@ Filepos.line pos);
    (* According to documentation, Frama-C cannot track columns.
    "column", `Float (float_of_int @@ Filepos.column pos); *)
  ]

let yaml_intervals intervals =
  match Int_Intervals.project_singleton intervals with
  | None -> `String "top"
  | Some (min, max) ->
    `O [
      "start",  `Float (float_of_int @@ Z.to_int min);
      "end",    `Float (float_of_int @@ Z.to_int max);
    ]

let yaml_access access =
  let open MemoryAccess in
  `O [
    "kind",     `String (MemoryAccess.show_kind access.kind);
    "offset",    yaml_intervals @@ get_offset access;
    "thread",   `String (Format.asprintf "%a" Kernel_function.pretty @@ Thread.get_entry_point @@ MemoryAccess.get_thread access);
    "location",  yaml_location @@ Stmt.loc @@ MemoryAccess.get_stmt access;
  ]

let yaml_base base = match base with
  | Base.Var (var, _) ->
    `O [
      "name",         `String var.vname;
      "type",         `String "variable";
      "declaration",  yaml_location var.vdecl;
    ]
  | Allocated (var, _, _) ->
    `O [
      "name",         `String var.vname;
      "type",         `String "dynamic";
      "allocation",   yaml_location var.vdecl;
    ]

let yaml_data_race race =
    let open Race in
    let fst, snd = race.accesses in
    `O [
      "base",     yaml_base race.base;
      "offset",   yaml_intervals race.offset;
      "ranking",  `String (Race.show_kind race.kind);
      "access1",  (yaml_access @@ fst);
      "access2",  (yaml_access @@ snd);
    ]

let yaml_data_races res = `A (List.map yaml_data_race res.races)

let yaml_sources () =
  `A (List.map (fun f -> `String (path_to_string f)) (Core0.OriginalSources.get ()))

let yaml_producer () =
  `O [
    "name",     `String "RacerF";
    "version",  `String Racer.version;
  ]

let yaml_metadata () =
  `O [
    "source_files", (yaml_sources ());
    "producer",   (yaml_producer ());
    "creation_time", `String (WitnessUtils.now ());
  ]

let mk_yaml results =
  `O [
    "metadata",   yaml_metadata ();
    "data_races", yaml_data_races results;
  ]

let output results filepath =
  let file = Format.asprintf "%a" Frama_c_kernel.Filepath.pretty filepath in
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  let yaml = mk_yaml results in
  Out_channel.output_string channel @@ Yaml.to_string_exn ~scalar_style:`Plain yaml;
  close_out channel
