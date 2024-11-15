(* Value analysis based on Frama-C's Alias plugin
 *
 *   Pointer-related queries are resolved using results of Alias analysis. Other
 *   queries ae using the Syntactic backend.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)


open Cil_datatype

module Self = struct

  let name = "alias"

  (** Include references to active thread and initial states. *)
  include ValueAnalysis_Base.Make ()

  let init main =
    init main;
    Syntactic.init main;
    Alias.Analysis.compute ()

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

  let choose_representant keep_local lval base varset =
    let thread = get_active_thread () in
    try
      Varinfo.Set.elements varset
      |> List.map Base.of_varinfo
      |> List.filter (fun base -> keep_local || BaseUtils.keep_for_racer thread base)
      |> List.hd
    with _ -> base

  let normalise_address keep_local stmt (base, offset) =
    let lval = Cil.var @@ Base.to_varinfo base in
    let aliases = Alias.API.Statement.points_to_vars ~stmt lval in
    Core0.debug "Aliases of %a (as lval: %a) at %a: %a"
      Base.pretty base
      LvalStructEq.pretty lval
      Print_utils.pretty_stmt_short stmt
      Varinfo.Set.pretty aliases;
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
    (normalise stmt reads, normalise stmt writes)

  let expr_reads ?(local=false) stmt expr =
    Syntactic.expr_reads ~local stmt expr
    |> normalise stmt

end

include ValueAnalysis_builder.Make(Self)
