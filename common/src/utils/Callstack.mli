
(* Callstacks.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_datatype

module Call : sig

  type t = Kernel_function.t * Stmt.t [@@deriving compare, equal]

  val show : t -> string

end

type t = {
  thread : Thread.t;
  calls : Call.t List.t;
  event : Stmt.t Option.t;
}

val empty : Thread.t -> t
(** Create an empty for given thread. *)

val push : Stmt.t -> Kernel_function.t -> t -> t

val push_event : Stmt.t -> t -> t

val pop_call : t -> t


val get_thread : t -> Thread.t

val get_event : t -> Stmt.t

val top: t -> Kernel_function.t


val mem_call : Kernel_function.t -> t -> bool


val convert_eva : Eva.Callstack.callstack -> Call.t list


val show : ?event:string -> t -> string

val show_event : ?prefix:string -> t -> string

val show_call_list : ?short:bool -> ?indent:int -> Call.t list -> string

val show_short : int -> t -> string
