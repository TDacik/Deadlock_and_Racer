(* May-run-in-parallel analysis.
 *
 * Computation of sets of sets of threads which may run in parallel at given stmt.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types

module Logger = Core0.Logger(struct let dkey = "pr" end)
module Callstack = RelaxedCallstack

module M = Map.Make(Callstack)
module C = Set.Make(struct
  type t = Callstack.t * Thread.Powerset.t [@@deriving equal, compare]
end)

module Context = struct

  let cache = ref C.empty

  type t = {
    active_threads : Thread.Powerset.t;
    is_imprecise : bool;

    callstack : Callstack.t;
    results : Thread.Powerset.t M.t;
    imprecise : bool M.t;
  }

  let empty thread = {
    active_threads = Thread.Powerset.singleton @@ Thread.Set.singleton thread;
    is_imprecise = false;

    callstack = Callstack.empty thread;
    results = M.empty;
    imprecise = M.empty;
  }

  let join ctx1 ctx2 = {
    active_threads = Thread.Powerset.union ctx1.active_threads ctx2.active_threads;
    is_imprecise = ctx1.is_imprecise || ctx2.is_imprecise;

    callstack = ctx1.callstack;
    results =
      M.union (fun _ ps1 ps2 -> Some (Thread.Powerset.union ps1 ps2)) ctx1.results ctx2.results;
    imprecise = M.union (fun _ c1 c2 -> Some (c1 || c2)) ctx1.imprecise ctx2.imprecise;
  }

  let set_imprecise ctx = {ctx with is_imprecise = true}

  let compute_initial ctx parent thread =
    M.fold (fun cs threads acc ->
      let parent' = Callstack.get_thread cs in
      if not @@ Thread.equal parent parent' then acc
      else
        let xs = Thread.Powerset.filter (fun set -> Thread.Set.mem thread set) threads in
        Thread.Set.union acc (Thread.Powerset.flatten_union xs)
    ) ctx.results Thread.Set.empty

  (** Callstack updates *)

  let push_call ctx stmt fn = {ctx with callstack = Callstack.push stmt fn ctx.callstack}

  let pop_call ctx = {ctx with callstack = Callstack.pop_call ctx.callstack}


  let update fn ctx = {ctx with active_threads = fn ctx.active_threads}

  let reset ctx thread active =
    {ctx with
      active_threads = active;
      is_imprecise = false;
      callstack = Callstack.empty thread
    }

  (** Updates of summaries *)

  let mk_stmt_summary ctx stmt =
    let callstack = Callstack.push_event stmt ctx.callstack in
    {ctx with
      results = M.add callstack ctx.active_threads ctx.results;
      imprecise = M.add callstack ctx.is_imprecise ctx.imprecise
    }

  let show_stmt_summaries ctx =
    M.fold (fun cs threads acc ->
      Format.asprintf "%s\n\t (%s) ↦  %a (precise: %b)"
        acc
        (Callstack.show_short cs)
        Thread.Powerset.pp threads
        (M.find cs ctx.imprecise)
    ) ctx.results ""

  let cache_find ctx stmt =
    let callstack = Callstack.push_event stmt ctx.callstack in
    if C.mem (callstack, ctx.active_threads) !cache then ctx
    else raise Not_found

  (** Function summaries

  let show_fn_summaries ctx =
    FunctionSummaries.fold (fun (cs, fn, active) post acc ->
      Format.asprintf "%s\n\t (%s, %a, %a) ↦  %a"
        acc
        (Callstack.show_short cs)
        Kernel_function.pretty fn
        Thread.Powerset.pp active
        Thread.Powerset.pp post
    ) !fn_cache "=========================================\n\nSummary:\n"

  let add_fn_summary ctx fn res =
    fn_cache := FunctionSummaries.add (ctx.callstack, fn, ctx.active_threads) res !fn_cache

  let find_fn_summary ctx fn =
    Core0.feedback "%s" (show_fn_summaries ctx);
    {ctx with
      active_threads = FunctionSummaries.find (ctx.callstack, fn, ctx.active_threads) !fn_cache}

  *)

end

open Context

module Make (ValueAnalysis : ValueAnalysis_sig.VALUE_ANALYSIS) = struct

  let thread_res = ref (None : ThreadAnalysis.Result.t option)

  (** Compute initial powerset for a thread different than main. *)
  let compute_initial ctx thread_graph thread =
    if Thread.is_main thread then
      Thread.Powerset.singleton @@ Thread.Set.singleton thread
    else
      let parents = ThreadGraph.pred thread_graph thread in
      List.fold_left (fun acc parent ->
        let active = Context.compute_initial ctx parent thread in
        Thread.Powerset.add active acc
      ) Thread.Powerset.empty parents

  let update_on_create ctx stmt id children arg =
    let children = ValueAnalysis.eval_fn_pointer stmt children in
    let children = List.map Thread.mk children in
    Context.update (fun active ->
      Thread.Powerset.concat_map (fun set ->
        Thread.Powerset.of_list @@ List.map (fun t ->
          Thread.Set.add t set
        ) children
      ) active
    ) ctx

  let update_on_join ctx stmt id =
    let kf = Kernel_function.find_englobing_kf stmt in
    if Kernel_function.stmt_in_loop kf stmt then Context.set_imprecise ctx
    else
      let id' = match id.enode with
        | Lval lval -> Cil.mkAddrOf ~loc:id.eloc lval
        | _ -> failwith "TODO: join id"
      in
      match ThreadAnalysis.Result.find_by_id_opt (Option.get !thread_res) id' with
        | None -> Context.set_imprecise ctx
        | Some thread ->
          Context.update (fun active ->
            Thread.Powerset.map (fun set -> Thread.Set.remove thread set) active
          ) ctx

  let rec update_on_call ctx callsite expr =
    let callees = ValueAnalysis.eval_call callsite expr in
    List.fold_left (fun acc callee ->
      let ctx' = compute_function ctx callee callsite in
      Context.join acc ctx'
    ) ctx callees

  and compute_stmt ctx stmt =
    try Context.cache_find ctx stmt
    with Not_found ->
      let callstack = Callstack.push_event stmt ctx.callstack in
      let _ = Context.cache := C.add (callstack, ctx.active_threads) !Context.cache in
      let ctx' = match ConcurrencyModel.classify_stmt stmt with
        | Thread_create (id, children, arg) -> update_on_create ctx stmt id children arg
        | Thread_join id -> update_on_join ctx stmt id
        | Call (_, _, fn, _) -> update_on_call ctx stmt fn
        | _ -> ctx
      in
      let ctx'' = Context.mk_stmt_summary ctx' stmt in
      List.fold_left (fun acc succ ->
        let ctx' = compute_stmt ctx'' succ in
        Context.join acc ctx'
      ) ctx'' stmt.succs

  and compute_function ctx fn stmt =
    if not @@ Kernel_function.has_definition fn then ctx
    else(* try
      let ctx = Context.find_fn_summary ctx fn in
      Core0.feedback "Cache hit";
      ctx
    with Not_found ->*)
      let _ = Core0.debug "Computing fn %a with %d"
        Kernel_function.pretty fn
        (Thread.Powerset.cardinal ctx.active_threads)
      in
      let ctx' = Context.push_call ctx stmt fn in
      let stmt = Kernel_function.find_first_stmt fn in
      let ctx'' = compute_stmt ctx' stmt in
      (*Context.add_fn_summary ctx' fn ctx''.active_threads;*)
      Context.pop_call ctx''

  let compute_thread ctx thread_graph thread =
    let entry_point = Thread.get_entry_point thread in
    let active = compute_initial ctx thread_graph thread in
    let ctx' = Context.reset ctx thread active in
    let stmt = Kernel_function.find_first_stmt entry_point in
    Logger.debug "Computing for thread %s with %s"
      (Thread.show thread) (Thread.Powerset.show ctx'.active_threads);
    compute_stmt ctx' stmt

  let compute threads =
    Logger.feedback "Starting may-run-in-parallel analysis";
    thread_res := Some threads;
    let thread_graph = ThreadAnalysis.Result.get_threads threads in
    let ctx0 = Context.empty @@ ThreadGraph.get_main thread_graph in

    let res = ThreadGraph.fold_vertex (fun thread ctx ->
      Logger.debug "> Starting analysis of thread: %a" Thread.pp thread;
      ValueAnalysis.set_active_thread thread;
      compute_thread ctx thread_graph thread
    ) thread_graph ctx0
    in

    Logger.feedback "May-run-in-parallel analysis finished";
    (*Logger.debug "%s" (Context.show_stmt_summaries res);*)

    res

end

(** Access to compute results *)

module Result = struct
  include Context

  let active_threads res cs = M.find cs res.results

  let parallel_aux flatten res cs1 cs2 =
    let thread1 = Callstack.get_thread cs1 in
    let thread2 = Callstack.get_thread cs2 in
    let active1 = flatten @@ active_threads res cs1 in
    let active2 = flatten @@ active_threads res cs2 in
    Thread.Set.mem thread1 active2 && Thread.Set.mem thread2 active1

  let must_run_in_parallel =
    parallel_aux Thread.Powerset.flatten_inter

  let may_run_in_parallel =
    parallel_aux Thread.Powerset.flatten_union

  let show = show_stmt_summaries

end
