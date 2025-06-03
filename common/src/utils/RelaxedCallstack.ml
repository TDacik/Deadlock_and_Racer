(* Callstack with relaxed equality used for context-sensitive analyses.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_datatype

include Callstack

(** Parameters set during plugin initialization *)

let depth = ref 1
let entry_point = ref true

(** {2 Override of comparison function} *)

let extract cs = {cs with calls = BatList.take !depth cs.calls}

let compare_thread cs1 cs2 =
  if !entry_point then Thread.compare cs1.thread cs2.thread
  else 0

let compare_call_list calls1 calls2 =
  let calls1 = BatList.take !depth calls1 in
  let calls2 = BatList.take !depth calls2 in
  List.compare Call.compare calls1 calls2

let equal cs1 cs2 = (compare cs1 cs2) = 0

let show_short cs =
  if not !entry_point && !depth == 0 then ""
  else if not !entry_point then show_call_list @@ BatList.take !depth cs.calls
  else Format.asprintf "%s : %s : %s"
    (Thread.show cs.thread) (show_call_list @@ BatList.take !depth cs.calls) (show_event cs)

let compare cs1 cs2 =
  let aux1 = Option.compare Stmt.compare cs1.event cs2.event in
  if aux1 != 0 then aux1
  else
    let aux2 = compare_thread cs1 cs2 in
    if aux2 != 0 then aux2
    else compare_call_list cs1.calls cs2.calls

let match_with_eva cs eva_cs =
  match compare_call_list cs.calls (convert_eva eva_cs) with
    | 0 -> true
    | _ -> false
