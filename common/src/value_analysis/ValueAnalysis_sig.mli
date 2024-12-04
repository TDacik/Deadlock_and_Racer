(* Signatures for value analysis backends.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_datatype

type concrete := Base.t * Integer.t

module type VALUE_ANALYSIS_BASE = sig

  val name : string

  val init : Kernel_function.t -> unit

  (** {2 Queries} *)

  val is_reachable : Stmt.t -> bool

  val eval_expr : ?callstack:Callstack.t -> Stmt.t -> Exp.t -> Cvalue.V.t

  val eval_expr_concretised : ?callstack:Callstack.t -> Stmt.t -> Exp.t -> concrete list

  val stmt_state : ?after:bool -> ?callstack:Callstack.t -> Stmt.t -> Cvalue.Model.t

  val eval_call : Stmt.t -> Exp.t -> Kernel_function.t list
  (** Evaluate what is called by a non-trivial expression. *)

  val eval_fn_pointer : Stmt.t -> Exp.t -> Kernel_function.t list
  (** Evaluate possible targets of a non-trivial function pointer. *)

  val memory_accesses : ?local:bool -> Stmt.t -> MemoryAddress.t list * MemoryAddress.t list
  (** Return lists of writes and reads at the given statement.
      By default, bases of locals are ignored. *)

  val expr_reads : ?local:bool -> Stmt.t -> Exp.t -> MemoryAddress.t list

end


module type VALUE_ANALYSIS = sig

  include VALUE_ANALYSIS_BASE

  val update_thread : Thread.t -> Exp.Set.t -> Thread.InitialState.t -> unit

  val set_active_thread : Thread.t -> unit

  val get_active_thread : unit -> Thread.t

  val check_imprecision : unit -> unit

end
