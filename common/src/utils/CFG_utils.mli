(* CIL and globals utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

open Cil_datatype

val filter_kfs : (Kernel_function.t -> bool) -> Kernel_function.t list

val filter_stmts : (Stmt.t -> bool) -> Stmt.t list

val is_unique_stmt : Stmt.t -> bool

val transitive_callers : Stmt.t -> Kernel_function.t list

val find_allocation_target : Base.t -> Varinfo.t option
