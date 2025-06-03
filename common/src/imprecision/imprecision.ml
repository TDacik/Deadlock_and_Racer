(* Imperative tracking of potential sources of incorrect analysis.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype

type t =
  | MemAccess of Stmt.t
  | Lock of Stmt.t * Exp.t
  | Unlock of Stmt.t * Exp.t
  | FunctionPointer of Varinfo.t
  | ThreadParamArray of Stmt.t
  | Join of Stmt.t
  | JoinInLoop of Stmt.t
  | ActiveWaiting of Stmt.t
  | TryLock of string
  | Malloc

  | Backend of t

let imprecisions = ref ([] : t list)

let add (source : t) =
  imprecisions := source :: !imprecisions

let add_backend (source : t) = () (*add (Backend source)*)

let get_imprecisions () = !imprecisions

let is_active_waiting stmt =
  let rec check_instructions stmt = match stmt.skind with
    | If (_, block_else, block_then, _) ->
      List.for_all check_instructions block_else.bstmts
      && List.for_all check_instructions block_then.bstmts
    | Break _ -> true
    | Block b -> List.for_all check_instructions b.bstmts
    | _ -> false
  in
  match stmt.skind with
  | Loop (_, block, _, _, _) -> begin match block.bstmts with
    | stmts -> List.for_all check_instructions stmts
    end
  | _ -> false

let check () =
  let empty_while_loops = CFG_utils.filter_stmts is_active_waiting in
  List.iter (fun s -> add (ActiveWaiting s)) empty_while_loops

let rec show = function
  | MemAccess (stmt) ->
    Format.asprintf "Memory access at %a" Print_utils.pretty_stmt_short stmt
  | Lock (stmt, _) ->
    Format.asprintf "No lock at %a" Print_utils.pretty_stmt_short stmt
  | Unlock (stmt, _) ->
    Format.asprintf "No unlock at %a" Print_utils.pretty_stmt_short stmt
  | FunctionPointer var ->
    Format.asprintf "Function pointer %a" Cil_datatype.Varinfo.pretty var
  | ThreadParamArray stmt ->
    Format.asprintf "Thread created at %a has array parameter" Print_utils.pretty_stmt_short stmt
  | Join stmt ->
    Format.asprintf "Thread join at %a" Print_utils.pretty_stmt_short stmt
  | JoinInLoop stmt ->
    Format.asprintf "Thread join in loop at %a" Print_utils.pretty_stmt_short stmt
  | ActiveWaiting stmt ->
    Format.asprintf "Active waiting at %a" Print_utils.pretty_stmt_short stmt
  | TryLock (str) ->
    Format.asprintf "Trylock status cannot be checked: %s" str
  | Malloc ->
    Format.asprintf "Dynamic allocation"

  | Backend imprecision ->
    Format.asprintf "%s (not supported by the backend)" (show imprecision)

let report filter =
  let imprecisions = List.filter filter !imprecisions in
  if imprecisions != [] then
    let _ = Core0.result "The result may be imprecise, reasons:" in
    List.iter (fun i -> Core0.result "  - %s" (show i)) imprecisions
