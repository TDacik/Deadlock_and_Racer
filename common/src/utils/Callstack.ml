(* Callstacks.
 *
 * Currently, we use our own definition of callstack which can be converted to
 * EVA representation, if necessary.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_datatype

module Call = struct

  type t = Kernel_function.t * Stmt.t [@@deriving compare, equal]

  let show (kf, stmt) =
    Format.asprintf "Call of %a at %a"
      Kernel_function.pretty kf
      Print_utils.pretty_stmt_short stmt

  let show_short (kf, stmt) =
    Format.asprintf "%a(%a)"
      Kernel_function.pretty kf
      Print_utils.pretty_stmt_short stmt

end


type t = {
  thread : Thread.t;
  calls : Call.t List.t;
  event : Stmt.t Option.t;
}

let empty thread = {
  thread = thread;
  calls = [];
  event = None;
}

(** Modifications *)

let push stmt fn cs = {cs with calls = (fn, stmt) :: cs.calls}

let push_event stmt cs = {cs with event = Some stmt}

let pop_call cs = {cs with calls = List.tl cs.calls}

let mem_call call cs = List.exists (fun (kf, _) -> Kernel_function.equal kf call) cs.calls

(** Accessors *)

let get_thread cs = cs.thread

let get_event cs = Option.get cs.event

let top cs = match cs.calls with
  | [] -> Thread.get_entry_point cs.thread
  | x :: _ -> fst x

let top_call cs = List.hd cs.calls

let last_call cs =
  try fst @@ List.hd (List.rev cs.calls)
  with _ -> Thread.get_entry_point cs.thread

(** Show *)

let show_call_list ?(short=true) calls =
  let separator = if short then "->" else "\n        " in
  let show_call = if short then Call.show_short else Call.show in
  String.concat separator @@ List.map show_call @@ List.rev calls

let show ?event cs =
  let event_str = match event with
    | None -> "<no_event>"
    | Some event ->
      Format.asprintf "%s at %a" event Print_utils.pretty_stmt_short @@ Option.get cs.event
  in
  Format.asprintf "In thread %s:\n        %s\n            %s"
    (Thread.show cs.thread)
    (show_call_list ~short:false cs.calls)
    event_str

let show_short n cs = show_call_list @@ BatList.take n cs.calls

(** {2 Conversion of callstacks as seen by EVA} *)

let convert_eva eva_cs =
  let open Eva.Callstack in
  eva_cs.stack
