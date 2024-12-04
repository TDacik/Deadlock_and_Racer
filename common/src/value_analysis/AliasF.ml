(* Value analysis based on Frama-C's Alias plugin
 *
 *   Pointer-related queries are resolved using results of Alias analysis. Other
 *   queries ae using the Syntactic backend.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype

module Self = struct

  let name = "alias"

  module Logger = Core0.Logger(struct let dkey = "alias" end)

  (** Include references to active thread and initial states. *)
  include ValueAnalysis_Base.Make ()

  let compute () =
    Alias.Analysis.compute ();
    let print_kf_alias_sets kf =
      if Kernel_function.has_definition kf then
        let alias_sets = Alias.API.Function.alias_sets_lvals ~kf in
        List.iter (fun lset ->
          Logger.debug "%a: %a" Kernel_function.pretty kf Alias.API.LSet.pretty lset
        ) alias_sets
      else ()
    in
    Globals.Functions.iter print_kf_alias_sets

  let init main =
    init main;
    Syntactic.init main;
    compute ()

  let set_active_thread thread =
    set_active_thread thread;
    if Thread.is_main thread then begin
      Globals.set_entry_point "main" false;
    end else
      let entry_point_name = Thread.show thread in
      Globals.set_entry_point entry_point_name false

  (** Functions derived from syntactic backend *)

  let is_reachable = Syntactic.Self.is_reachable
  let eval_fn_pointer = Syntactic.Self.eval_fn_pointer
  let eval_call = Syntactic.Self.eval_call
  let eval_expr = Syntactic.Self.eval_expr
  let stmt_state = Syntactic.Self.stmt_state

  let lval_to_base lval = match lval with
    | Var var, NoOffset -> Base.of_varinfo var
    | _, _ -> failwith "TODO"

  (** TODO: this is a quick fix for issue with thread arguments *)
  let process_thread_arg thread base =
    if BaseUtils.is_thread_arg thread base then base
    else try
      Thread.Map.find thread !thread_args
      |> Exp.Set.elements
      |> List.map Cil.extract_varinfos_from_exp
      |> List.map Varinfo.Set.elements
      |> List.concat
      |> List.hd
      |> Base.of_varinfo
    with _ -> base

  let choose_representant keep_local lval base varset =
    let thread = get_active_thread () in
    let aliases =
      LvalStructEq.Set.elements varset
      |> List.map lval_to_base
      |> List.filter (fun base -> keep_local || BaseUtils.keep_for_racer thread base)
    in
    let representant =
      if List.is_empty aliases then base
      else List.hd aliases
    in
    process_thread_arg thread representant


  let normalise_address keep_local stmt (base, offset) =
    let lval = Cil.var @@ Base.to_varinfo base in
    let kf = Kernel_function.find_englobing_kf stmt in
    let state = Option.get @@ Alias.API.get_state_before_stmt kf stmt in
    let aliases = Alias.API.Abstract_state.find_synonyms lval state in
    Core0.debug "Aliases of %a (as lval: %a) at %a: %a"
      Base.pretty base
      LvalStructEq.pretty lval
      Print_utils.pretty_stmt_short stmt
      LvalStructEq.Set.pretty aliases;
    choose_representant keep_local lval base aliases, offset

  (** Apply aliases:
      - we have even local accesses on the input
      - we saturate the set with all aliases
      - TODO: we saturate the set by adding all aliases of thread argument
      - we filter out local accesses *)
  let normalise stmt ?(keep_local=false) locs =
    let thread = get_active_thread () in
    List.map (normalise_address keep_local stmt) locs
    |> List.filter (fun (base, _) -> keep_local || BaseUtils.keep_for_racer thread base)

  let eval_expr_concretised ?callstack stmt expr =
    Syntactic.eval_expr_concretised ?callstack stmt expr
    |> normalise ~keep_local:true stmt

  (* {2 Memory accesses} *)

  let memory_accesses ?(local=false) stmt =
    let reads, writes = Syntactic.memory_accesses ~local:true stmt in

    Format.printf "Memory accesses at %a:\n" Print_utils.pretty_stmt_short stmt;
    List.iter (fun m -> Format.printf " - r: %s\n" (MemoryAddress.show m)) reads;
    List.iter (fun m -> Format.printf " - w: %s\n" (MemoryAddress.show m)) writes;

    (normalise stmt reads, normalise stmt writes)

  let expr_reads ?(local=false) stmt expr =
    Syntactic.expr_reads ~local:true stmt expr
    |> normalise stmt

end

include ValueAnalysis_builder.Make(Self)
