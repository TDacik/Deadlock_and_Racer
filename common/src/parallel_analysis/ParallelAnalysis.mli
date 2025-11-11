(* May-run-in-parallel analysis.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_datatype

open ValueAnalysis_sig

module Result : sig

  type t

  val active_threads: t -> Callstack.t -> Thread.Powerset.t

  val may_active_threads : t -> Callstack.t -> Thread.Set.t

  val must_active_threads : t -> Callstack.t -> Thread.Set.t

  val may_active_stmt : t -> Stmt.t -> Thread.Set.t

  val may_run_in_parallel : t -> Callstack.t -> Callstack.t -> bool

  val must_run_in_parallel : t -> Callstack.t -> Callstack.t -> bool

  val show : t -> string

end


module Make (_ : VALUE_ANALYSIS) : sig

  val compute : ThreadAnalysis.Result.t -> Result.t

end
