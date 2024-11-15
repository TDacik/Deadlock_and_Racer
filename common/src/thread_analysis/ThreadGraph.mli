(* Thread-graph
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

open Cil_datatype

include Graph.Sig.P
  with type V.t = Thread.t
   and type E.t = Thread.t * Stmt.Set.t * Thread.t

include Datatype_sig.PRINTABLE with type t := t

val fold_vertex : (Thread.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold on vertices in topological order. *)

val update_edge : t -> Thread.t -> Thread.t -> Stmt.t -> t

val update_edge' : t -> Thread.t -> Thread.t -> Stmt.t -> t

val initial : ?mirror:bool -> Thread.t -> t

val get_main : t -> Thread.t
