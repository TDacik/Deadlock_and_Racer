(* Witness generator for format 2.0
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024

let get_machdep () = match Cil.theMachine.theMachine.machdep_name with
  | "machdep_gcc_x86_32" -> "ILP32"
  | "machdep_gcc_x86_64" -> "LP64"
  | other -> other

let producer metadata = [
  ("name",    `String metadata.producer);
  ("version", `String metadata.version);
]

let task metadats =  [
  ("input_files",      `A ());
  ("input_file_hashes", `O (hash_files ()));
  ("specification",    `String spec);
  ("data_model",       `String (get_machdep ()));
  ("language",         `String language);
]

let metadata uuid time =  [
  ("format_version", `String format_version);
  ("uuid",           `String uuid);
  ("creation_time",  `String time);
  ("producer",       `O producer);
  ("task",           `O (task ()));
]

let target_loc race =
  let open Race in
  let cs = (fst race.accesses).callstack in
  let stmt = Callstack.get_event cs in
  let file, line = Print_utils.descr_stmt stmt in
  let line = Float.of_int line in
  let fn = Callstack.last_call cs in
  let fn = Format.asprintf "%a" Kernel_function.pretty fn in
  [
    ("file_name", `String file);
    ("line",      `Float line);
    ("column",    `Float 1.0);
    ("function",  `String fn);
  ]

let final trace = [
  ("action",   `String "follow");
  ("type",     `String "target");
  ("location", `O (target_loc trace));
]

let waypoints trace : Yaml.value list = [
  `O [("waypoint", `O (final trace))];
]

let content trace : Yaml.value list = [
  `O [("segment", `A (waypoints trace))];
]

let witness trace ~uuid ~time : Yaml.value = `A [ `O [
  ("entry_type",  `String "violation_sequence");
  ("metadata",    `O (metadata uuid time));
  ("content",     `A (content trace));
]]

let mk_yaml trace =
  let uuid = Uuidm.to_string @@ Uuidm.v4_gen (Random.State.make_self_init ()) () in
  let t = Unix.localtime @@ Unix.time () in
  let time =
    Format.sprintf "%04u-%02u-%02uT%02u:%02u:%02u"
      (1900 + t.tm_year) (1 + t.tm_mon) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  in
  witness ~uuid ~time trace

let dump_trace trace filepath =
  let file = Format.asprintf "%a" Frama_c_kernel.Filepath.Normalized.pp_abs filepath in
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  let yaml = mk_yaml trace in
  Out_channel.output_string channel (Yaml.to_string_exn yaml);
  close_out channel
*)
