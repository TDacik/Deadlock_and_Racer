(* Deadlock detection in results of lockset analysis.
 *
 * TODO: detect multiple independent deadlocks if possible
 *
 * STEP 1: Compute a minimal set of edges which would make the graph acyclic.
 *
 * STEP 2: Find a cycles containing those edges.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz, 2024 *)

module Self = Deadlock

module Logger = Deadlock.Logger(struct let dkey = "deadlock-detection" end)

module Weight = struct
  include Int

  type edge = Lockgraph.E.t
  let weight _ = -1
end

module BF = Graph.Path.BellmanFord
  (Lockgraph)
  (Weight)

module Dependency = struct

  type t = Thread.t * Lock.t * Lock.t * Trace.t

  let show (thread, x, y, trace) =
    Format.asprintf "Thread %s: %s -> %s:\n%s"
      (Thread.show thread)
      (Lock.show x)
      (Lock.show y)
      (Trace.show trace)

  let to_json (thread, lock1, lock2, trace) =
    `Assoc [
      "thread", `String (Thread.show thread);
      "lock 1", `String (Lock.show lock1);
      "lock 2", `String (Lock.show lock2);
      "stmt",   `String (Format.asprintf "%a" Print_utils.pretty_stmt_short
                         @@ Trace.get_stmt trace);
    ]

end

module Deadlock = struct

  type kind = Must | May

  type t = kind * Dependency.t list

  let is_must (kind, _) = kind = Must
  let is_may (kind, _) = kind = May

  let mk kind (cycle : Lockgraph.E.t list) =
    let deps =
      List.fold_left (fun acc (lock1, traces, lock2) ->
        let trace = List.hd traces in (* TODO: trace ranking*)
        (Trace.get_thread trace, lock1, lock2, trace) :: acc
      ) [] cycle
    in
    (kind, deps)

  let report (kind, dl) =
    let prefix = match kind with Must -> "Must" | May -> "May" in
    prefix ^ " deadlock: \n" ^ String.concat "\n\n" @@ List.map Dependency.show dl

  let to_json (_, dl) =
    `List (List.map Dependency.to_json dl)

end

module Result = struct

  type t = Deadlock.t list

  let to_json dls =
    `Assoc ["deadlocks", `List (List.map Deadlock.to_json dls)]

  let out_json res filepath =
    let file = Format.asprintf "%a" Frama_c_kernel.Filepath.Normalized.pp_abs filepath in
    let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
    Yojson.Basic.(pretty_to_channel channel (to_json res));
    close_out channel

end

let preprocess lock_graph =
  if Self.DoubleLocks.get () then lock_graph
  else
    Lockgraph.fold_edges_e (fun (src, label, dst) acc ->
      if Lock.equal src dst then acc
      else Lockgraph.add_edge_e acc (src, label, dst)
    ) lock_graph Lockgraph.empty

let is_must_trace locksets lock1 lock2 trace =
  let stmt = Trace.get_stmt trace in
  let must_ls = LocksetAnalysis.Result.stmt_must_lockset locksets stmt in
  Lock.Set.mem lock1 must_ls && Lock.Set.mem lock2 must_ls

let remove_may_edges locksets lock_graph =
  Lockgraph.fold_edges_e (fun (src, traces, dst) acc ->
    let traces = List.filter (is_must_trace locksets src dst) traces in
    match traces with
    | [] -> acc
    | ts -> Lockgraph.add_edge_e acc (src, ts, dst)
  ) lock_graph Lockgraph.empty

let compute locksets =
  Logger.feedback "Started";
  let lockgraph = LocksetAnalysis.Result.get_lockgraph locksets in
  let lockgraph = preprocess lockgraph in
  let must_lockgraph = remove_may_edges locksets lockgraph in

  (** First check must lockgraph *)
  try let cycle = BF.find_negative_cycle must_lockgraph in
    [Deadlock.mk Deadlock.Must cycle]
  with Not_found ->
    (** Check may lockgraph *)
      try let cycle = BF.find_negative_cycle lockgraph in
        [Deadlock.mk Deadlock.May cycle]
      with Not_found -> []

let report flag =
  if flag then Format.printf "Deadlock found\n"
  else ()
