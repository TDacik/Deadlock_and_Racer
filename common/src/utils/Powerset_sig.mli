module type SET = sig
  type t
  include BatSet.S with type t := t
  include Datatype_sig.SHOW with type t := t
end

module type POWERSET = sig

  type elt
  type set
  type t

  include BatSet.S with type elt := set and type t := t
  include Datatype_sig.PRINTABLE with type t := t

  val concat_map : (set -> t) -> t -> t
  (** Apply the function to each element and return union of results. *)

  val flatten_union : t -> set

  val flatten_inter : t -> set

end
