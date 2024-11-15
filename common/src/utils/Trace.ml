(* Trace is a pair of callstack with non-empty common prefix.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

module Callstack = RelaxedCallstack
open Callstack

type t = {
  common_prefix : Callstack.t;
  first : Callstack.t;
  second : Callstack.t;
} [@@deriving compare, equal]

let get_stmt self = Callstack.get_event self.second

let rec common_prefix calls1 calls2 = match calls1, calls2 with
  | [], xs -> ([], [], xs)
  | xs, [] -> ([], xs, [])
  | (x :: xs), (y :: ys) when Callstack.Call.equal x y ->
    let common', rest1, rest2 = common_prefix xs ys in
    (x :: common', rest1, rest2)
  | xs, ys -> ([], xs, ys)

let mk cs1 cs2 =
  assert (Thread.equal cs1.thread cs2.thread);
  let common, rest1, rest2 = common_prefix cs1.calls cs2.calls in
  {
    common_prefix = {cs2 with calls = common};
    first = {cs1 with calls = rest1};
    second = {cs2 with calls = rest2};
  }

let get_thread self = Callstack.get_thread self.common_prefix

(* TODO *)
let show self =
  Format.asprintf "In thread %s: ...."
    (Thread.show @@ get_thread self)
