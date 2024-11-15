(* Representation of a memory address as used by Racer.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

type t = Base.t * Int_Intervals.t [@@deriving compare, equal]

let show (base, offset) =
  Format.asprintf "%a[%a]" Base.pretty base Int_Intervals.pretty offset

let base (base, _) = base

let offset (_, offset) = offset

module Self = struct
  type nonrec t = t
  let compare = compare
  let show = show
end

include Datatype.Printable(Self)
include Datatype.Collections(Self)
