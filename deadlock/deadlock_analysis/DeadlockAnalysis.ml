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

  type t = Dependency.t list

  let mk (cycle : Lockgraph.E.t list) =
    List.fold_left (fun acc (lock1, traces, lock2) ->
      let trace = List.hd traces in (* TODO*)
      (Trace.get_thread trace, lock1, lock2, trace) :: acc
    ) [] cycle

  let report dl = String.concat "\n\n" @@ List.map Dependency.show dl

  let to_json dl =
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

let compute lock_graph =
  let lock_graph = preprocess lock_graph in
  try let cycle = BF.find_negative_cycle lock_graph in
    [Deadlock.mk cycle]
  with Not_found -> []

let report flag =
  if flag then Format.printf "Deadlock found\n"
  else ()
