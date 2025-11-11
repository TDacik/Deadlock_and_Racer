(* Lockset analysis
 *
 * TODO: simplify summaries
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz, 2020 *)

open Cil_types
open Cil_datatype

open ValueAnalysis_sig

module Logger = Core0.Logger(struct let dkey = "locksets" end)

module Callstack = RelaxedCallstack

module StatementSummaries = struct
  include Map.Make(struct
    type t = Callstack.t * Stmt.t * Lock.Set.t [@@deriving compare]
  end)
end

module FunctionSummaries = struct
  include Map.Make(struct
    type t = Callstack.t * Kernel_function.t * Lock.Set.t [@@deriving compare]
  end)
end

(** {2 Abstract state of the analysis} *)

module Context = struct

  type t = {
    lockset : Lock.Set.t;
    callstack : Callstack.t;
  }

  let mk thread = {
    lockset = Lock.Set.empty;
    callstack = Callstack.empty thread;
  }

  let push_call ctx fn stmt = {ctx with callstack = Callstack.push stmt fn ctx.callstack}

end

module Result = struct

  type t = {
    fn_summaries : Lock.PowerSet.t FunctionSummaries.t;
    stmt_summaries : Lock.PowerSet.t StatementSummaries.t;
    stmt_cache : Lock.PowerSet.t StatementSummaries.t;
    lock_graph : Lockgraph.t;
  }

  let empty = {
    fn_summaries = FunctionSummaries.empty;
    stmt_cache = StatementSummaries.empty;
    stmt_summaries = StatementSummaries.empty;
    lock_graph = Lockgraph.empty;
  }

  let get_lockgraph res = res.lock_graph

  let show self = Lockgraph.show self.lock_graph

  (** Summaries update *)

  let add_stmt_summary res ctx stmt exit_lss =
    let cs = ctx.Context.callstack in
    {res with stmt_summaries =
      StatementSummaries.add (cs, stmt, ctx.lockset) exit_lss res.stmt_summaries
    }

  let add_stmt_cache res ctx stmt exit_lss =
    let cs = ctx.Context.callstack in
    {res with stmt_cache =
      StatementSummaries.add (cs, stmt, ctx.lockset) exit_lss res.stmt_cache
    }

  let add_fn res ctx exit_lss =
    let cs = ctx.Context.callstack in
    let fn = Callstack.top ctx.callstack in
    {res with fn_summaries =
      FunctionSummaries.add (cs, fn, ctx.lockset) exit_lss res.fn_summaries
    }

  let find_stmt ctx res stmt =
    let cs = ctx.Context.callstack in
    StatementSummaries.find (cs, stmt, ctx.Context.lockset) res.stmt_summaries

  let find_fn ctx res fn =
    let cs = ctx.Context.callstack in
    FunctionSummaries.find (cs, fn, ctx.Context.lockset) res.fn_summaries

  let filter_stmt res stmt callstack =
    StatementSummaries.filter (fun (cs', stmt', _) _ ->
      match callstack with
      | None -> Stmt.equal stmt stmt'
      | Some cs -> Stmt.equal stmt stmt' && Callstack.equal cs cs'
    ) res.stmt_summaries

  let filter_fn res fn =
    FunctionSummaries.filter (fun (_, fn', _) _ ->
      Kernel_function.equal fn fn'
    ) res.fn_summaries

  let filter_trylocks state lss =
    let open Lock in
    let check_status lock = match lock.status, lock.kind.blocking with
      |  _, true -> true
      | None, _ -> true
      | Some status, false ->
        Eva.Results.in_cvalue_state state
        |> Eva.Results.eval_lval status
        |> Eva.Results.as_ival
        |> (function
             | Ok x when Ival.is_zero x -> true
             | Ok x -> false
             | Error error ->
               Imprecision.add (Imprecision.TryLock (Eva.Results.string_of_error error));
               Core0.debug "State: %a\n" Cvalue.Model.pretty state;
               true
             )
       in
       Lock.PowerSet.map (Lock.Set.filter check_status) lss

  let stmt_locksets res ?callstack ?state stmt =
    let lss =
      filter_stmt res stmt callstack
      |> StatementSummaries.bindings
      |> List.map snd
      |> Lock.PowerSet.union_list
    in
    (* Filter non-blocking locks based on their status in the state. *)
    match state with
      | None -> lss
      | Some state -> filter_trylocks state lss

  let stmt_must_lockset res stmt =
    Lock.PowerSet.flatten_inter @@ stmt_locksets res stmt

  (** Debug info *)

  let show_stmt res stmt =
    let filtered = filter_stmt res stmt None in
    StatementSummaries.fold (fun (cs, _, ls) lss acc ->
      Format.asprintf "%s\n\t (%s, %a) ↦  %a"
        acc
        (Callstack.show_short cs)
        Lock.Set.pp ls
        Lock.PowerSet.pp lss
    ) filtered ""

  let show_fn res fn =
    let filtered = filter_fn res fn in
    FunctionSummaries.fold (fun (cs, _, ls) lss acc ->
      Format.asprintf "%s\n\t (%s, %a) ↦  %a"
        acc
        (Callstack.show_short cs)
        Lock.Set.pp ls
        Lock.PowerSet.pp lss
    ) filtered ""

  let show_stmt_summaries res =
    StatementSummaries.fold (fun (cs, stmt, ls) lss acc ->
      Format.asprintf "%s\n\t (%s, %a, %a) ↦  %a"
        acc
        (Callstack.show_short cs)
        Core0.pretty_stmt stmt
        Lock.Set.pp ls
        Lock.PowerSet.pp lss
    ) res.stmt_summaries ""

  let show_fn_summaries res =
    FunctionSummaries.fold (fun (cs, f, ls) lss acc ->
      Format.asprintf "%s\n\t (%s, %a, %a) ↦  %a"
        acc
        (Callstack.show_short cs)
        Kernel_function.pretty f
        Lock.Set.pp ls
        Lock.PowerSet.pp lss
    ) res.fn_summaries ""

end

module Make (ValueAnalysis : VALUE_ANALYSIS) = struct

  module Result = Result
  open Result
  open Context

  let rec update_on_lock ctx res stmt expr kind status =
    let lock_values = ValueAnalysis.eval_expr_concretised ~callstack:ctx.callstack stmt expr in

    (if lock_values = [] then
      Imprecision.add (Lock (stmt, expr));
    );

    let callstack = Callstack.push_event stmt ctx.callstack in

    let locked = List.map (fun (base, offset) -> match status with
      | None -> Lock.mk ~callstack ~kind base offset
      | Some status -> Lock.mk ~callstack ~kind ~status base offset
    ) lock_values
    in
    Logger.debug ">       [|%a|] = %s" Exp.pretty expr (Lock.show_list locked);
    let lock_graph = Lockgraph.update_on_lock res.lock_graph ctx.lockset locked callstack in
    let lss = Lock.PowerSet.add_each ctx.lockset locked in
    lss, {res with lock_graph = lock_graph}

  and update_on_unlock ctx res stmt expr =
    let lock_values = ValueAnalysis.eval_expr_concretised ~callstack:ctx.callstack stmt expr in

    (*let inter = Lockset.intersection ctx.lockset (Lockset.of_list lockvalues)
    (if Lockset.is_empty inter then
      Imprecision.add (Unlock (stmt, expr));
    );*)

    let lss = match lock_values with
    (* TODO: is this covered? *)
    | [] when Lock.Set.is_empty ctx.lockset -> Lock.PowerSet.singleton ctx.lockset
    | [] -> Lock.Set.fold (fun lock acc ->
               Lock.PowerSet.add (Lock.Set.remove lock ctx.lockset) acc
            ) ctx.lockset Lock.PowerSet.empty
    | _ ->
      let unlocked = List.map (fun (base, offset) -> Lock.mk base offset) lock_values in
      Logger.debug ">       [|%a|] = %s" Exp.pretty expr (Lock.show_list unlocked);
      Lock.PowerSet.remove_each ctx.lockset unlocked
    in

    let lss =
      if List.length lock_values > 1 then Lock.PowerSet.add Lock.Set.empty lss
      else lss
    in
    lss, res

  (** Fork analysis for each possible value of called function. We propagate
      result and join abstract context. Not that for recursive call, we need
      to use ctx, not ctx_acc! *)
  and update_on_call ?(atomic=false) ctx res callsite callee =
    let callee_values = ValueAnalysis.eval_call callsite callee in
    match callee_values with
      | [] -> Lock.PowerSet.singleton ctx.lockset, res
      | _ ->
        let lss_final, res_final =
          List.fold_left (fun (lss_acc, res_acc) callee ->
            let lss, res' = analyse_function ctx res_acc callee callsite in
            Lock.PowerSet.union lss lss_acc, res'
          ) (Lock.PowerSet.empty, res) callee_values
        in
        if atomic then
          let lss_final = Lock.PowerSet.map (Lock.Set.remove (Lock.global_lock ())) lss_final in
          lss_final, res_final
        else lss_final, res_final

  and analyse_stmt ?(visited=[]) ctx res stmt =
    if BatList.mem_cmp Stmt.compare stmt visited then
      let _ = Logger.debug "> Breaking loop at %a" Print_utils.pretty_stmt_loc stmt in
      Lock.PowerSet.empty, res
    else try
      let lss = Result.find_stmt ctx res stmt in
      Logger.debug ">     Cutting off already visited path at %a"
        Print_utils.pretty_stmt_loc stmt;
      lss, res (* We have already analysed this path *)
    with Not_found ->
      Logger.debug ">     Analysing statement %a with %a"
        Print_utils.pretty_stmt_loc stmt Lock.Set.pp ctx.lockset;

     (* Evaluate effect of statement on lockset *)
     let lss, res' = match ConcurrencyModel.classify_stmt stmt with
        | Lock (lock, kind, status) -> update_on_lock ctx res stmt lock kind status
        | Unlock lock -> update_on_unlock ctx res stmt lock
        | Call (Direct kf, _, callee, _)
          when ConcurrencyModel.is_atomic_fn @@ Kernel_function.get_vi kf ->
            let lockset = Lock.Set.add (Lock.global_lock ()) ctx.lockset in
            update_on_call ~atomic:true {ctx with lockset} res stmt callee
        | Call (_, _, callee, _) -> update_on_call ctx res stmt callee
        | Atomic_seq_start -> Lock.PowerSet.add_each ctx.lockset [Lock.global_lock ()], res
        | Atomic_seq_end -> Lock.PowerSet.remove_each ctx.lockset [Lock.global_lock ()], res
        | _ -> Lock.PowerSet.singleton ctx.lockset, res
      in
      Logger.debug ">     ~> %a" Lock.PowerSet.pp lss;

      let res' = Result.add_stmt_summary res' ctx stmt lss in

      match stmt.succs with
        | [] -> lss, res'
        | _ ->

      (* Fork analysis for each successor and each possible lockset. After each
         branch, we progate the result and join abstract contexts. Note that for
         recursive call, we need to use ctx' ! *)
      let visited = stmt :: visited in
      let succs = match stmt.skind with
        (*| If (e, then_b, else_b, _) ->
          let v = ValueAnalysis.eval_expr ~callstack:ctx.callstack stmt e in
          if not @@ Cvalue.V.contains_zero v then [List.nth stmt.succs 0]
          else if not @@ Cvalue.V.contains_non_zero v then [List.nth stmt.succs 1]
          else stmt.succs*)
        | _ -> stmt.succs
      in
      let lss_final, res_final = BatList.cartesian_product succs (Lock.PowerSet.elements lss)
      |> List.fold_left (fun (lss_acc, res_acc) (stmt, ls) ->
        let ctx' = {ctx with lockset = ls} in
        let lss', res'' = analyse_stmt ~visited ctx' res_acc stmt in
        Lock.PowerSet.union lss_acc lss', res''
      ) (Lock.PowerSet.empty, res')
      in
      let res_final = Result.add_stmt_cache res_final ctx stmt lss_final in
      lss_final, res_final

  and analyse_function ctx res fn callsite =
    let ctx' = Context.push_call ctx fn callsite in
    if Callstack.mem_call fn ctx.callstack then
      Lock.PowerSet.singleton ctx.lockset, res
    else if Kernel_function.has_noreturn_attr fn then
      let _ = Logger.debug ">   Stopping at noreturn function %a" Kernel_function.pretty fn in
      Lock.PowerSet.empty, res
    else if not @@ Kernel_function.has_definition fn then
      let _ = Logger.debug ">   Skipping external function %a" Kernel_function.pretty fn in
      Lock.PowerSet.singleton ctx.lockset, res
    else try
      let exit_lss = Result.find_fn ctx' res fn in
      Logger.debug ">   Using chached entry for function %a" Kernel_function.pretty fn;
      exit_lss, res
    with Not_found ->
      Logger.debug ">   Entering function %a" Kernel_function.pretty fn;
      let stmt = Kernel_function.find_first_stmt fn in
      let lss, res = analyse_stmt ctx' res stmt in
      Logger.debug ">   Exit function %a: %a ~> %a"
        Kernel_function.pretty fn
        Lock.Set.pp ctx.lockset
        Lock.PowerSet.pp lss;
      let res' = Result.add_fn res ctx' lss in
      lss, res'

  and analyse_thread ctx res fn =
    let stmt = Kernel_function.find_first_stmt fn in
    let lss, res = analyse_stmt ctx res stmt in
    let res' = Result.add_fn res ctx lss in

    (** TODO: inter/union *)
    let unreleased_locks = Lock.PowerSet.flatten_union lss in
    (if not @@ Lock.Set.is_empty unreleased_locks then
      Imprecision.add (UnreleasedLock (fn, Lock.Set.choose unreleased_locks))
    );

    lss, res'

  let compute threads =
    Logger.feedback "Starting lockset analysis";
    let init_res = Result.empty in
    let thread_graph = ThreadAnalysis.Result.get_threads threads in

    let res = ThreadGraph.fold_vertex (fun thread acc ->
      Logger.debug "> Starting analysis of thread: %a" Thread.pp thread;
      let entry_point = Thread.get_entry_point thread in
      ValueAnalysis.set_active_thread thread;
      let ctx = Context.mk thread in
      snd @@ analyse_thread ctx acc entry_point
    ) thread_graph init_res
    in

    (* TODO: investigate why this slow down the analysis

    Logger.debug "Function summaries:%s"
      (Result.show_fn_summaries res);
    Logger.debug "Statement summaries:%s"
      (Result.show_stmt_summaries res);


    Logger.feedback "Lockgraph: %s"
      (Result.show res);
    *)
    Logger.feedback "Lockset analysis finished";
    res

end
