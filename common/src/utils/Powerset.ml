(* Utilities for manipulation sets of sets.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Powerset_sig

module Make (Base : SET) = struct

  include BatSet.Make(Base)

  type elt = Base.elt [@@ocaml.warning "-34"]
  type set = Base.t   [@@ocaml.warning "-34"]

  let concat_map (fn : set -> t) (xss : t) =
    fold (fun xs acc -> union acc (fn xs)) xss empty

  let flatten_union lss = fold Base.union lss Base.empty

  let flatten_inter lss =
    if is_empty lss then assert false
    else fold Base.inter lss (choose lss)

  let show xs =
    elements xs
    |> List.map Base.show
    |> String.concat ", "
    |> Format.asprintf "{%s}"

  include Datatype.Printable(struct
    type nonrec t = t
    let show = show
  end)

end
