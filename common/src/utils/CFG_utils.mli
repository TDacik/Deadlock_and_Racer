(* CIL and globals utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

open Cil_datatype

val filter_kfs : (Kernel_function.t -> bool) -> Kernel_function.t list

val filter_stmts : (Stmt.t -> bool) -> Stmt.t list

val is_exit : Stmt.t -> bool

val is_unique_stmt : Stmt.t -> bool

val transitive_callers : Stmt.t -> Kernel_function.t list

val find_allocation_target : Base.t -> Varinfo.t option

val find_allocation_stmt : Base.t -> Stmt.t

val extract_atomic_expressions : Exp.t -> Exp.t list
(** Return list of all atomic expressions contained in input.
    Expression is atomic iff it it does not have any proper sub-expression. *)
