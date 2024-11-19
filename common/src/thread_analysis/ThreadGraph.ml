(* Thread-graph
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

open Cil_datatype

module Edge = struct
  include Stmt.Set
  let default = Stmt.Set.empty
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Thread)(Edge)
include G

let update_edge g parent child stmt =
  try
    let e = G.find_edge g parent child in
    let g' = G.remove_edge g parent child in
    let stmts = Stmt.Set.add stmt @@ E.label e in
    Core0.debug "Updating edge %s -{ %a }-> %s"
      (Thread.show parent)
      Print_utils.pretty_stmt_short stmt
      (Thread.show child);
    G.add_edge_e g' (parent, stmts, child)
  with Not_found ->
    let stmts = Stmt.Set.singleton stmt in
    Core0.debug "Creating edge %s -{ %a }-> %s"
      (Thread.show parent)
      Print_utils.pretty_stmt_short stmt
      (Thread.show child);
    G.add_edge_e g (parent, stmts, child)

let update_edge' g parent child stmt =
  let g0 = update_edge g parent child stmt in
  match Core0.ThreadApproximation.get () with
    | `Under -> g0
    | `Over -> update_edge g0 child parent stmt

let get_main g =
  Option.get @@ G.fold_vertex (fun t acc -> match acc with
    | None when Thread.is_main t -> Some t
    | None -> None
    | Some t -> Some t
   ) g None

module Topological = Graph.Topological.Make(G)

let fold_vertex = Topological.fold

let show g =
  if G.nb_vertex g = 1 then Thread.show (get_main g)
  else
    let show_label xs =
      if Stmt.Set.cardinal xs == 1 then ""
      else Format.asprintf "(%d)" (Stmt.Set.cardinal xs)
    in
    G.fold_edges_e (fun (parent, stmts, child) acc ->
      let e = Format.asprintf "%s -> %s %s"
        (Thread.show parent)
        (Thread.show child)
        (show_label stmts)
      in
      Format.asprintf "%s\n%s" acc e
    ) g ""

include Datatype.Printable(struct
  type nonrec t = t
  let show = show
end)

let initial ?(mirror=false) main = match Core0.ThreadApproximation.get (), mirror with
  | `Over, true -> G.add_edge G.empty main main
  | _ -> G.add_vertex G.empty main
