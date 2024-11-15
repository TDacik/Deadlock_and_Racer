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

let imprecisions = ref ([] : t list)

let add (source : t) =
  imprecisions := source :: !imprecisions

let get_imprecisions () = !imprecisions

let is_active_waiting stmt =
  let rec check_instructions stmt = match stmt.skind with
    | If (_, block_else, block_then, _) ->
      List.for_all check_instructions block_else.bstmts
      && List.for_all check_instructions block_then.bstmts
    | Break _ -> true
    | _ -> false
  in
  match stmt.skind with
  | Loop (_, block, _, _, _) -> begin match block.bstmts with
    | [s] -> check_instructions s
    | _ -> false
    end
  | _ -> false

let check () =
  (*Globals.Vars.iter (fun v _ ->
    if Cil.isFunPtrType v.vtype
    then add (FunctionPointer v)
  );*)
  let empty_while_loops = CFG_utils.filter_stmts is_active_waiting in
  List.iter (fun s -> add (ActiveWaiting s)) empty_while_loops

let show = function
  | MemAccess (stmt) ->
    Core0.result "  - Memory access at %a" Print_utils.pretty_stmt_short stmt
  | Lock (stmt, _) ->
    Core0.result "  - No lock at %a" Print_utils.pretty_stmt_short stmt
  | Unlock (stmt, _) ->
    Core0.result "  - No unlock at %a" Print_utils.pretty_stmt_short stmt
  | FunctionPointer var ->
    Core0.result "  - Function pointer %a" Cil_datatype.Varinfo.pretty var
  | ThreadParamArray stmt ->
    Core0.result "  - Thread created at %a has array parameter" Print_utils.pretty_stmt_short stmt
  | Join stmt ->
    Core0.result "  - Thread join at %a" Print_utils.pretty_stmt_short stmt
  | JoinInLoop stmt ->
    Core0.result "  - Thread join in loop at %a" Print_utils.pretty_stmt_short stmt
  | ActiveWaiting stmt ->
    Core0.result "  - Active waiting at %a" Print_utils.pretty_stmt_short stmt
  | TryLock (str) ->
    Core0.result "  - Trylock status cannot be checked: %s" str

let report filter =
  let imprecisions = List.filter filter !imprecisions in
  if imprecisions != [] then
    let _ = Core0.result "The result may be imprecise, reasons:" in
    List.iter show imprecisions
