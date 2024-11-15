(* Parsing of concurrency model specification from yaml files.
 *
 * TODO: simplify & clean up
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Yaml

open ConcurrencyModel_data

(** Utils *)

let parse_fn_name yaml = match Util.find "function" yaml with
  | Ok (Some value) -> Util.to_string_exn value
  | _ -> failwith "Missing function name"

let parse_int key yaml = match Util.find key yaml with
  | Ok (Some value) -> Float.to_int @@ Util.to_float_exn value
  | _ -> failwith @@ "Missing argument position " ^ key

let parse_bool key yaml = match Util.find key yaml with
  | Ok (Some value) -> Util.to_bool_exn value
  | _ -> failwith @@ "Missing boolean value " ^ key

let parse_fn1 key yaml = (parse_fn_name yaml, parse_int key yaml)

let parse_fn_lock yaml =
  let open ConcurrencyModel_types.LockKind in
  let kind = {
    blocking = parse_bool "blocking" yaml;
    read_lock = parse_bool "read-lock" yaml;
    reentrant = parse_bool "reentrant" yaml;
  }
  in
  (parse_fn_name yaml, (parse_int "lock-position" yaml, kind))

let load_or_skip apply key yaml = match Util.find key yaml with
  | Ok (Some value) -> apply value
  | _ -> ()

let load_or_fail apply key yaml = match Util.find key yaml with
  | _ -> ()

(** Locks *)

let load_types yaml = match yaml with
  |`A xs -> Lock_types.add_list @@ List.map Util.to_string_exn xs
  | _ -> ()

let load_init = function
  | `A xs -> Lock_init_functions.add_list @@ List.map (parse_fn1 "lock-position") xs
  | _ -> ()

let load_destroy = function
  | `A xs -> Lock_destroy_functions.add_list @@ List.map (parse_fn1 "lock-position") xs
  | _ -> ()

let load_unlock = function
  | `A xs -> Unlock_functions.add_list @@ List.map (parse_fn1 "lock-position") xs
  | _ -> ()

let load_lock = function
  | `A xs -> Lock_functions.add_list @@ List.map parse_fn_lock xs
  | _ -> ()

let load_locks yaml =
  load_or_skip load_types "types" yaml;
  load_or_skip load_init "init" yaml;
  load_or_skip load_destroy "destroy" yaml;
  load_or_skip load_lock "lock" yaml;
  load_or_skip load_unlock "unlock" yaml

(** Threads *)

let parse_fn_create yaml =
  let id = parse_int "thread-id-position" yaml in
  let entry_point = parse_int "entry-point-position" yaml in
  let arg = parse_int "argument-position" yaml in
  (parse_fn_name yaml, (id, entry_point, arg))

let load_thread_types = function
  | `A xs -> Thread_types.add_list @@ List.map Util.to_string_exn xs
  | _ -> ()

let load_thread_create = function
  | `A xs -> Thread_create_functions.add_list @@ List.map parse_fn_create xs
  | _ -> ()

let load_thread_join = function
  | `A xs -> Thread_join_functions.add_list @@ List.map (parse_fn1 "thread-id-position") xs
  | _ -> ()

let load_threads yaml =
  load_or_skip load_thread_types "types" yaml;
  load_or_skip load_thread_create "create" yaml;
  load_or_skip load_thread_join "join" yaml

(** Atomic *)

let load_seq_start = function
  | `A xs -> AtomicSequenceStart.add_list @@ List.map Util.to_string_exn xs
  | _ -> ()

let load_seq_end = function
  | `A xs -> AtomicSequenceEnd.add_list @@ List.map Util.to_string_exn xs
  | _ -> ()

let load_functions = function
  | `A xs -> AtomicFunctions.add_list @@ List.map Util.to_string_exn xs
  | _ -> ()

let load_atomic yaml =
  load_or_skip load_seq_start "sequence-start" yaml;
  load_or_skip load_seq_end "sequence-end" yaml;
  load_or_skip load_functions "functions" yaml

(** Conditions *)

let parse_fn_wait yaml =
  let cond = parse_int "condition-position" yaml in
  let lock = parse_int "lock-position" yaml in
  (parse_fn_name yaml, (cond, lock))

let load_cond_types = function
  | `A xs -> Condition_types.add_list @@ List.map Util.to_string_exn xs
  | _ -> ()

let load_cond_wait = function
  | `A xs -> Condition_wait_functions.add_list @@ List.map parse_fn_wait xs
  | _ -> ()

let load_conditions yaml =
  load_or_skip load_cond_types "types" yaml;
  load_or_skip load_cond_wait "wait" yaml

let load_model value =
  load_or_skip load_locks "Locks" value;
  load_or_skip load_threads "Threads" value;
  load_or_skip load_atomic "Atomic" value;
  load_or_skip load_conditions "Conditions" value

let load path =
  Format.printf "Loading %s\n" path;
  let content = In_channel.with_open_text path In_channel.input_all in
  let yaml = Yaml.of_string_exn content in
  load_model yaml
