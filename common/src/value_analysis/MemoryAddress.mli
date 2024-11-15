(* Representation of a memory address as used by Racer.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

type t = Base.t * Int_Intervals.t

include Datatype_sig.PRINTABLE with type t := t
include Datatype_sig.COMPARABLE with type t := t
include Datatype_sig.COLLECTIONS with type t := t

val base : t -> Base.t

val offset : t -> Int_Intervals.t
