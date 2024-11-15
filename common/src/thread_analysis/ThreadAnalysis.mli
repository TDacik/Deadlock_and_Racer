open Cil_datatype

open ValueAnalysis_sig

module Result : sig

  type t = ThreadAnalysis0.t

  val is_thread : t -> Kernel_function.t -> bool
  (** Checks whether a kernel function is an entry point of some thread. *)

  val find_thread : t -> Kernel_function.t -> Thread.t
  (** Find a thread with given kernel function as its entry point. *)

  val get_threads : t -> ThreadGraph.t

  val get_thread_list : t -> Thread.t list

  val get_edges : t -> (Thread.t * Stmt.Set.t * Thread.t) list

  val get_initial_state : t -> Thread.t -> Thread.InitialState.t

  val is_unique : t -> Thread.t -> bool

  val find_by_id_opt : t -> ExpStructEq.t -> Thread.t option

  val stmt_active_threads : Stmt.t -> t -> Thread.Set.t
  (** Return the set of possibly active threads at the given statement. *)

  val stmt_is_parallel : Stmt.t -> t -> bool
  (** True iff statement can be reached by multiple concrete threads. *)

end

module Make (_ : VALUE_ANALYSIS) : sig

  val compute : unit -> Result.t

end
