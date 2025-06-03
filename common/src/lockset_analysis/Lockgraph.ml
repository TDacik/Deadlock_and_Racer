(* Lockgraph.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

module Edge = struct
  type t = Trace.t list
  let compare = List.compare Trace.compare
  let default = []
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Lock)(Edge)
include G

let show g =
  let show_trace_info = function
    | [] -> ""
    | xs -> Format.asprintf "(%d times)" (List.length xs)
  in
  if is_empty g then "{}"
  else
  G.fold_edges_e (fun (lock1, traces, lock2) acc ->
    let e =
      Format.asprintf "%s -> %s %s"
        (Lock.show lock1) (Lock.show lock2)
        (show_trace_info traces)
    in
    Format.asprintf "%s\n%s" acc e
  ) g ""

include Datatype.Printable(struct
  type nonrec t = t
  let show = show
end)

(* Create new edge. If it already exists, update it by adding labels. *)
let add_edge_e g (lock1, traces, lock2) =
  try
    let edge = find_edge g lock1 lock2 in
    let label = E.label edge in
    let new_label = traces @ label in
    let new_edge = E.create lock1 new_label lock2 in
    let g = remove_edge_e g edge in
    add_edge_e g new_edge
  with Not_found ->
    let edge = E.create lock1 traces lock2 in
    add_edge_e g edge

let update_on_lock g held locked callstack =
  try (* Hack for master-lock *)
    let held = Lock.Set.elements held in
    BatList.cartesian_product held locked
    |> List.fold_left (fun g (x, y) ->
      let trace = Trace.mk (Lock.get_callstack x) callstack in
      add_edge_e g (x, [trace], y)
    ) g
  with _ -> g


let get_traces (_, traces, _) = traces
