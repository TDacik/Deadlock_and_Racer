(* Representation of locks, locksets and sets of locksets.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open ConcurrencyModel_types

type t = {
  base: Base.t;
  kind: LockKind.t;
  offset: Integer.t;
  status: Cil_types.lval Option.t;
  callstack: Callstack.t Option.t;
}

include Datatype_sig.PRINTABLE with type t := t
include Datatype_sig.COMPARABLE with type t := t

val global_lock : unit -> t

val hash : t -> int

val mk : ?callstack:Callstack.t -> ?kind:LockKind.t -> ?status:Cil_types.lval -> Base.t -> Integer.t -> t

val get_callstack : t -> Callstack.t

module Set : sig
  include BatSet.S with type elt = t
  include Datatype_sig.PRINTABLE with type t := t

  val rw_disjoint : t -> t -> bool

end


module PowerSet : sig
  include BatSet.S with type elt = Set.t
  include Datatype_sig.PRINTABLE with type t := t

  val union_list : t list -> t

  val add_each : Set.t -> Set.elt list -> t

  val remove_each : Set.t -> Set.elt list -> t

  val flatten_union : t -> Set.t

  val flatten_inter : t -> Set.t

end
