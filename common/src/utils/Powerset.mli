open Powerset_sig

module Make (Base : SET) : POWERSET
  with type elt := Base.elt
   and type set := Base.t
