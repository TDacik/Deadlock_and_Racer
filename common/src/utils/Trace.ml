(* Trace is a pair of callstack with non-empty common prefix.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open Cil_datatype

module Callstack = RelaxedCallstack
open Callstack

(** Comparision for calls can be ignored becuase this information is so far
    not used by the deadlock detection. *)
module Calls = struct
  type t = Call.t list
  let compare _ _ = 0
  let equal _ _ = true
end

type t = {
  thread : Thread.t;
  common_prefix : Calls.t;

  first : Calls.t;
  first_event: Stmt.t;

  second : Calls.t;
  second_event: Stmt.t;
} [@@deriving compare, equal]

let get_stmt self = self.second_event
let get_thread self = self.thread
let get_events self = (self.first_event, self.second_event)

let rec common_prefix calls1 calls2 = match calls1, calls2 with
  | [], xs -> ([], [], xs)
  | xs, [] -> ([], xs, [])
  | (x :: xs), (y :: ys) when Callstack.Call.equal x y ->
    let common', rest1, rest2 = common_prefix xs ys in
    (x :: common', rest1, rest2)
  | xs, ys -> ([], xs, ys)

let mk (cs1 : Callstack.t) (cs2 : Callstack.t) =
  assert (Thread.equal cs1.thread cs2.thread);
  assert (Option.is_some cs1.event);
  assert (Option.is_some cs2.event);
  let common, rest1, rest2 = common_prefix cs1.calls cs2.calls in
  {
    thread = cs1.thread;
    common_prefix = common;
    first = rest1;
    first_event = Option.get cs1.event;
    second = rest2;
    second_event = Option.get cs2.event;
  }

let show_aux ~indent calls event =
  let indent_str = String.init (indent) (fun _ -> ' ') in
  match calls with
  | [] ->
    Format.asprintf "%sLock at %a" indent_str Print_utils.pretty_stmt_short event

  | _ ->
    indent_str
    ^ Callstack.show_call_list ~short:false ~indent:(indent+2) calls
    ^ (Format.asprintf "\n%s  Lock at %a" indent_str Print_utils.pretty_stmt_short event)

let show self =
  (* No-common call prefix *)
  if List.is_empty self.common_prefix then
    Format.asprintf "%s\n%s"
      (show_aux ~indent:2 self.first self.first_event)
      (show_aux ~indent:2 self.second self.second_event)
  else
    Format.asprintf "%s\n%s\n%s"
      (Callstack.show_call_list self.common_prefix)
      (show_aux ~indent:4 self.first self.first_event)
      (show_aux ~indent:4 self.second self.second_event)
