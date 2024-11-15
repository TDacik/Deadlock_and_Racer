(** References to results for GUI modules *)

let locksets = ref (None : LocksetAnalysis.Result.t option)

let threads = ref (None : ThreadAnalysis.Result.t option)

let threads_to_json threads =
  let show_edge (p, _, c) = [
    "parent", `String (Thread.show p);
    "child",  `String (Thread.show c)]
  in
  ThreadAnalysis.Result.get_edges threads
  |> List.map (fun t -> (`Assoc (show_edge t)))
  |> (fun xs -> `List xs)


let to_json () =
  let threads = Option.get !threads in
  `Assoc [
    "thread-graph", threads_to_json threads;
  ]

let out_json filepath =
  let file = Format.asprintf "%a" Frama_c_kernel.Filepath.Normalized.pp_abs filepath in
  let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
  Yojson.Basic.(pretty_to_channel channel (to_json ()));
  close_out channel
