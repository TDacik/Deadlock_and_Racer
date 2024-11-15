(* Smart datatypes
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Datatype_sig

module Printable (M : SHOW) : PRINTABLE with type t := M.t
(** Builds pretty-printing functions from a show function *)

module Comparable (M : COMPARISON) : COMPARABLE with type t := M.t
(** Builds equality from compare function *)

module Collections (M : COMPARISON) : COLLECTIONS with type t := M.t
(** Builds set and map over type t *)
