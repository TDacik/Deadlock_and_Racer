(* CIL and globals utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

open Cil_types
open Cil_datatype

(** Find all statements matching given predicate. *)
let filter_stmts predicate =
  Globals.Functions.fold (fun kf acc ->
    let stmts =
      try (Kernel_function.get_definition kf).sallstmts
      with Kernel_function.No_Definition -> []
    in
    acc @ List.fold_left (fun acc stmt -> if predicate stmt then stmt :: acc else acc) [] stmts
  ) []

let filter_kfs predicate =
  Globals.Functions.fold (fun kf acc ->
    if predicate kf then kf :: acc
    else acc
  ) []

let rec is_unique_stmt stmt =
  let kf = Kernel_function.find_englobing_kf stmt in
  if Kernel_function.stmt_in_loop kf stmt then false
  else is_unique_kf kf

and is_unique_kf kf =
  let _, callsites = List.split @@ Kernel_function.find_syntactic_callsites kf in
  List.for_all is_unique_stmt callsites

(* TODO: probably not very efficient... *)
let rec transitive_callers stmt =
  let kf = Kernel_function.find_englobing_kf stmt in
  let kfs, callsites = List.split @@ Kernel_function.find_syntactic_callsites kf in
  kf :: (kfs @ List.concat_map transitive_callers callsites)

(** Get the number of line on which a dynamically allocated base was created.
    This is very ad-hoc, but Frama-C seems to have no suitable function. *)
let get_malloc_line base =
  let name = Format.asprintf "%a" Base.pretty base in
  let regex = Str.regexp ".*_l\\([0-9]+\\)" in
  let res = Str.string_match regex name 0 in
  if not res then failwith ("Internal error: unexpected name of allocated base " ^ name)
  else int_of_string @@ Str.matched_group 1 name

let find_allocation_stmt base =
  let line = get_malloc_line base in
  let stmts = filter_stmts (fun stmt ->
    let line' = (fst @@ Stmt.loc stmt).pos_lnum in
    Int.equal line line'
  )
  in
  List.hd stmts

let is_exit stmt = match stmt.skind with
  | Instr (Call (_, e, _, _)) ->
    begin match e.enode with
      | Lval (Var v, NoOffset) ->
        let kf = Globals.Functions.get v in
        Kernel_function.has_noreturn_attr kf
      | _ -> false
    end
  | _ -> false

(** Find a pointer variable for which the given base was allocated. *)
let find_allocation_target base =
  let line = get_malloc_line base in
  let stmts = filter_stmts (fun stmt ->
    let line' = (fst @@ Stmt.loc stmt).pos_lnum in
    Int.equal line line'
  )
  in
  List.fold_left (fun acc stmt -> match acc with
    | Some var -> Some var
    | None -> begin match stmt.skind with
      | Instr (Local_init (var, _, _)) -> Some var
      | Instr (Call (Some lval, _, _, _)) -> begin match lval with
        | (Var (var), _) -> Some var
        | _ -> None
      end
    | _ -> None
    end
  ) None stmts

let rec extract_atomic_expressions expr = match expr.enode with
  | SizeOfE e | AlignOfE e | CastE (_, e) | UnOp (_, e, _) -> extract_atomic_expressions e
  | BinOp (_, e1, e2, _) -> extract_atomic_expressions e1 @ extract_atomic_expressions e2
  | _ -> [expr]
