(* Representation of a thread - entry point & initial state
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

module InitialState : sig

  type t = Cvalue.Model.t * Cvalue.V.t [@@deriving compare, equal]

  val mk : Cvalue.Model.t -> Cvalue.V.t -> t

  val show : t -> string

  val bottom : t

  val top : t

  val join : t -> t -> t

  val widen : t -> t -> t

end

type t

include Datatype_sig.PRINTABLE with type t := t
include Datatype_sig.COMPARABLE with type t := t
include Datatype_sig.COLLECTIONS with type t := t

module Powerset : Powerset_sig.POWERSET
  with type elt := t
   and type set := Set.t

val mk : ?is_main:bool -> ?stmt:Cil_datatype.Stmt.t -> Kernel_function.t -> t

val get_name : t -> string

val get_entry_point : t -> Kernel_function.t

val is_main : t -> bool

val hash : t -> int
