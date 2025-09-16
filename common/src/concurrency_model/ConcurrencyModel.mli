(* Functions over conccurency model.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open Cil_datatype

open ConcurrencyModel_types

type call_type =
  | Direct of Kernel_function.t
  | Extern of Varinfo.t
  | Pointer of Exp.t

type stmt =
  | Lock of Exp.t * LockKind.t * Lval.t option
  | Unlock of Exp.t
  | Lock_init of Exp.t
  | Lock_destroy of Exp.t

  | Condition_init of Exp.t
  | Condition_wait of Exp.t * Exp.t (* Condition, lock *)
  | Condition_signal of Exp.t

  | Thread_create of Exp.t * Exp.t * Exp.t
  (* Thread ID * Entry point * Argument *)

  | Thread_join of Exp.t
  (* Thread ID *)

  | Atomic_seq_start
  | Atomic_seq_end
  | Atomic_call of Exp.t * Exp.t list

  | Call of call_type * Lval.t option * Exp.t * Exp.t list

  | Return
  | Other

val classify_stmt : Stmt.t -> stmt

val get_thread_arg : Stmt.t -> Exp.t

val is_atomic_fn : Varinfo.t -> bool
