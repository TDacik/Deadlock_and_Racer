(* Witness generator for format 1.0
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open WitnessUtils

let machdep () = match Machine.machdep_name () with
  | "machdep_gcc_x86_32" -> "32bit"
  | "machdep_gcc_x86_64" -> "64bit"
  | other -> other

let write_line ?default fmt id name typ for_ =
  Format.fprintf fmt
    "<key id=\"%s\" attr.name=\"%s\" attr.type=\"%s\" for=\"%s\"" id name typ for_;
  match default with
  | None -> Format.fprintf fmt "/>\n"
  | Some default ->
      Format.fprintf fmt ">\n  <default>%s</default>\n" default;
      Format.fprintf fmt "</key>\n"

let write_data fmt key value =
  Format.fprintf fmt "  <data key=\"%s\">%s</data>\n" key value

let write_header fmt =
  write_line fmt "witness-type" "witness-type" "string" "graph";
  write_line fmt "producer" "producer" "string" "graph";
  write_line fmt "sourcecodelang" "sourcecodeLanguage" "string" "graph";
  write_line fmt "programfile" "programfile" "string" "graph";
  write_line fmt "programhash" "programhash" "string" "graph";
  write_line fmt "creationtime" "creationtime" "string" "graph";
  write_line fmt "specification" "specification" "string" "graph";
  write_line fmt "architecture" "architecture" "string" "graph";

  write_line fmt "entry" "isEntryNode" "boolean" "node" ~default:"false";
  write_line fmt "violation" "isViolationNode" "boolean" "node" ~default:"false";
  write_line fmt "endline" "endline" "int" "node"

let write_nodes fmt race =
  let open Race in
  Format.fprintf fmt "<node id=\"start\">\n";
  write_data fmt "entry" "true";
  Format.fprintf fmt "</node>\n";

  Format.fprintf fmt "<node id=\"access1\">\n";
  write_data fmt "violation" "true";
  Format.fprintf fmt "</node>\n";

  Format.fprintf fmt "<node id=\"access2\">\n";
  write_data fmt "violation" "true";
  Format.fprintf fmt "</node>\n";

  let cs = (fst race.accesses).callstack in
  let stmt = Callstack.get_event cs in
  let _, line1 = Print_utils.descr_stmt stmt in

  let cs = (snd race.accesses).callstack in
  let stmt = Callstack.get_event cs in
  let _, line2 = Print_utils.descr_stmt stmt in

  Format.fprintf fmt "<edge id=\"E1\" source=\"start\" target=\"access1\">\n";
  write_data fmt "enterFunction" "main";
  write_data fmt "createThread" "0";
  write_data fmt "endline" (string_of_int line1);
  Format.fprintf fmt "</edge>\n";

  Format.fprintf fmt "<edge id=\"E2\" source=\"start\" target=\"access2\">\n";
  write_data fmt "enterFunction" "main";
  write_data fmt "createThread" "0";
  write_data fmt "endline" (string_of_int line2);
  Format.fprintf fmt "</edge>\n"

let write_graph fmt metadata race =
  Format.fprintf fmt "<graph edgedefault=\"directed\">\n";
  write_data fmt "producer" metadata.producer;
  write_data fmt "sourcecodelang" metadata.language;
  write_data fmt "programfile" @@ fst @@ List.hd metadata.files;
  write_data fmt "programhash" @@ snd @@ List.hd metadata.files;
  write_data fmt "specification" metadata.spec;
  write_data fmt "architecture" @@ machdep ();
  write_data fmt "creationtime" metadata.time;
  write_data fmt "witness-type" "violation_witness";
  write_nodes fmt race;
  Format.fprintf fmt "</graph>\n"

let write_witness fmt metadata race =
  Format.fprintf fmt "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  Format.fprintf fmt "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n";
  Format.fprintf fmt "         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n\n";
  write_header fmt;
  write_graph fmt metadata race;
  Format.fprintf fmt "</graphml>\n"

let dump_trace race filepath =
  let metadata = WitnessUtils.metadata () in
  let file = Format.asprintf "%a" Frama_c_kernel.Filepath.Normalized.pp_abs filepath in
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  let fmt = Format.formatter_of_out_channel channel in
  write_witness fmt metadata race;
  close_out channel
