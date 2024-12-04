(* Syntactic backend answers all queries using just syntactical introspection of expressions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype

module Self = struct

  module Logger = Core0.Logger(struct let dkey = "syntactic" end)

  let name = "syntactic"

  include ValueAnalysis_Base.Make()

  let trim str =
    BatString.replace_chars (function '&' -> "" | c -> Format.asprintf "%c" c) str
    |> BatString.trim

  (** {2 Queries} *)

  (** Syntactic reachability *)
  let is_reachable stmt =
    let entry_point = Thread.get_entry_point @@ get_active_thread () in
    BatList.mem_cmp Kernel_function.compare entry_point @@ CFG_utils.transitive_callers stmt

  (** Extract all non-numerical bases *)
  let eval_expr_concretised ?callstack _ expr =
    Cil.extract_varinfos_from_exp expr
    |> Varinfo.Set.filter (fun var -> not @@ Cil.isArithmeticType var.vtype)
    |> Varinfo.Set.elements
    |> List.map (fun var -> (Base.of_varinfo var, Integer.zero))

  let eval_expr ?callstack stmt expr = Cvalue.V.top

  let stmt_state ?(after=false) ?callstack _ = Cvalue.Model.top

  let eval_fn_pointer _ expr =
    let expr' = Cil.stripCasts expr in
    let name = trim @@ Format.asprintf "%a" Printer.pp_exp expr' in
    let res =
      try [Globals.Functions.find_def_by_name name]
      with Not_found -> ValueAnalysis_utils.all_possible_threads ()
    in
    Logger.debug "[| %a |] = {%s}"
      Exp.pretty expr
      (String.concat ", " @@ List.map (fun f -> Format.asprintf "%a" Kernel_function.pretty f) res);
    res

  let eval_call _ expr =
    let name = trim @@ Format.asprintf "%a" Printer.pp_exp expr in
    try [Globals.Functions.find_def_by_name name]
    with Not_found -> []

  (** {2 Access extraction} *)

  (** We assume that every varinfo is read. To do this, we first need to
    deconstruct statement into expressions. *)
  let compute_reads local stmt =
    let expressions = match stmt.skind with
      | Instr instr -> begin match instr with
        | Set (_, expr, _) -> [expr]
        | Call (_, _, expr_list, _) -> expr_list
        | Local_init (_, local_init, _) -> begin match local_init with
          | AssignInit init -> begin match init with
            | SingleInit expr -> [expr]
            | CompoundInit _ -> [] (* TODO *)
          end
        | ConsInit (_, expr_list, _) -> expr_list
        end
      | _ -> [] (*TODO *)
      end
      | If (expr, _, _, _) | Switch (expr, _, _, _) -> [expr]
      | Return (expr, _) -> begin match expr with
        | Some expr -> [expr]
        | None -> []
        end
      | _ -> []
   in
   let thread = get_active_thread () in
   List.map Cil.extract_varinfos_from_exp expressions
   |> List.concat_map Varinfo.Set.elements
   |> List.map Base.of_varinfo
   |> List.filter (fun base -> local || BaseUtils.keep_for_racer thread base)
   |> List.map (fun b -> (b, Int_Intervals.top))

  (*  TODO: handle more precisely *)
  let extract_offset lval = match snd lval with
    | Field (field, NoOffset) ->
      let start, width = Cil.fieldBitsOffset field in
      let start, stop = Integer.of_int start, Integer.of_int (start + width - 1) in
      Int_Intervals.inject_bounds start stop
    | NoOffset | Index _ | Field (_, _) -> Int_Intervals.top

  (** Extraction of write variables *)

  let extract_writed_vars_lval lval = match fst lval with
    | Var v -> Varinfo.Set.singleton v
    | Mem e -> Cil.extract_varinfos_from_exp e (* Over-approximation by everything *)

  let compute_writes local stmt =
    let lvals = match stmt.skind with
      | Instr instr -> begin match instr with
        | Set (lval, _, _) -> [lval]
        | Call (lval_opt, _, _, _) -> Option.to_list lval_opt
        | Local_init (var, _, _) -> [Cil.var var]
        | _ -> [] (* TODO *)
      end
      | _ -> []
    in
    let offset = List.map extract_offset lvals in
    let thread = get_active_thread () in
    let bases =
      List.map extract_writed_vars_lval lvals
      |> List.concat_map Varinfo.Set.elements
      |> List.map Base.of_varinfo
      |> List.filter (fun base -> local || BaseUtils.keep_for_racer thread base)
    in
    match offset with
      | [] -> []
      | [o] -> List.map (fun b -> (b, o)) bases

  let concretise_zone zone =
    Locations.Zone.fold_i (fun base offsets acc ->
      if Int_Intervals.is_top offsets then acc
      else (base, offsets) :: acc
    ) zone []

  let get_accesses local zone =
    let thread = get_active_thread () in
    concretise_zone zone
    |> List.filter (fun (base, _) -> (BaseUtils.keep_for_racer thread base))

  let memory_accesses ?(local=false) stmt =
    (*try
      Logger.debug "R: %a : %a" Stmt.pretty stmt Locations.Zone.pretty (Inout.stmt_inputs stmt);
      Logger.debug "W: %a : %a" Stmt.pretty stmt Locations.Zone.pretty (Inout.stmt_outputs stmt);
      let reads = get_accesses local @@ Inout.stmt_inputs stmt in
      let writes = get_accesses local @@ Inout.stmt_outputs stmt in
      (reads, writes)
    with _ ->*)
      let reads = compute_reads local stmt in
      let writes = compute_writes local stmt in
      (reads, writes)

  let expr_reads ?(local=false) stmt expr =
    let thread = get_active_thread () in
    Cil.extract_varinfos_from_exp expr
    |> Varinfo.Set.elements
    |> List.map Base.of_varinfo
    |> List.filter (fun base -> local || BaseUtils.keep_for_racer thread base)
    |> List.map (fun b -> (b, Int_Intervals.top))

  let check_imprecision () = ()

end

include ValueAnalysis_builder.Make(Self)
