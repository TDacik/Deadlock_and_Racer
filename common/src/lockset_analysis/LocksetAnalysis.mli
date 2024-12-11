(* Lockset analysis
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz, 2020 *)

open Cil_datatype

open ValueAnalysis_sig

module Result : sig

  type t

  val get_lockgraph : t -> Lockgraph.t

  val stmt_locksets :
    t -> ?callstack:Callstack.t -> ?state:Cvalue.Model.t -> Stmt.t -> Lock.PowerSet.t

  val stmt_must_lockset : t -> Stmt.t -> Lock.Set.t

  val show_stmt : t -> Stmt.t -> string

  val show_fn : t -> Kernel_function.t -> string

end

module Make (_ : VALUE_ANALYSIS) : sig

  val compute : ThreadAnalysis.Result.t -> Result.t

end
