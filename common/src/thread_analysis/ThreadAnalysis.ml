open Cil_datatype

open ValueAnalysis_sig
open ThreadAnalysis0

module Logger = Core0.Logger(struct let dkey = "threads" end)
module Callstack = RelaxedCallstack

module Result = struct
  include ThreadAnalysis0
  let get_threads res = res.thread_graph
end

module Context = struct

  type t = {
    callstack : Callstack.t;
    active_threads : Thread.Set.t;
  }

  let initial thread active_threads = {
    callstack = Callstack.empty thread;
    active_threads = active_threads;
  }

  let current_thread ctx = Callstack.get_thread @@ ctx.callstack

  let add_threads ctx threads =
    {ctx with active_threads = Thread.Set.union ctx.active_threads @@ Thread.Set.of_list threads}

  let remove_thread ctx thread =
    {ctx with active_threads = Thread.Set.remove thread ctx.active_threads}

  let join c1 c2 = {c1 with active_threads = Thread.Set.union c1.active_threads c2.active_threads}

end

(** Imperative caching of already visited statements. *)
(** TODO: reset in fixpoint? *)
module Cache = struct

  module S = Set.Make(Stmt)

  let cache = ref S.empty

  let add stmt = cache := S.add stmt !cache

  let mem stmt = S.mem stmt !cache

end

(** The function is responsible for:
    - adding new threads to thread-graph
    - updating initial states of created threads
    - updating computed sets of active threads
    - update parallel threads *)
let update_on_create ctx stmt init all_active bases tid_bases self child =
  let parent = Context.current_thread ctx in
  let g' = ThreadGraph.update_edge self.thread_graph parent child stmt in
  let g'' = ThreadGraph.update_edge' self.thread_graph parent child stmt in
  let update = function None -> Some init | Some x -> Some (Thread.InitialState.join x init) in
  let init' = M.update child update self.initial_states in
  let active' = StatementMap.add (stmt) all_active self.active_threads in
  let create' = M.add_to_list child stmt self.create_stmts in (* TODO: cmp... *)

  let update_id = function None -> Some tid_bases | Some ids -> Some (ExpStructEq.Set.union tid_bases ids) in
  let ids = M.update child update_id self.thread_ids in

  (* For all active threads, update their parallel sets by active threads *)
  let update = function None -> Some all_active | Some a -> Some (Thread.Set.union a all_active) in
  let parallel' = Thread.Set.fold (fun t acc -> M.update t update acc) all_active self.parallel_threads in
  {self with
    thread_graph = g';
    thread_graph' = g'';
    initial_states = init';
    active_threads = active';
    create_stmts = create';
    parallel_threads = parallel';
    escaped_bases = bases;
    thread_ids = ids;
  }


(** Update with multiple created threads *)
(** TODO: join old *)
let update_on_creates (self : Result.t) stmt (ctx : Context.t) created_threads initial_state bases tid =
  let all_active = Thread.Set.union ctx.active_threads @@ Thread.Set.of_list created_threads in
  let all_active = Thread.Set.union all_active @@ StatementMap.find_or_empty stmt self.active_threads in
  List.fold_left (update_on_create ctx stmt initial_state all_active bases tid) self created_threads

let update_on_noop self stmt (ctx : Context.t) =
  let threads = StatementMap.find_or_empty stmt self.active_threads in
  let threads' = Thread.Set.union threads ctx.active_threads in
  {self with active_threads = StatementMap.add stmt threads' self.active_threads}


module Make (ValueAnalysis : VALUE_ANALYSIS) = struct

  let update_on_create ctx res stmt tid entry_point arg =
    let escaped_bases = List.map fst @@ ValueAnalysis.eval_expr_concretised stmt arg in
    let children = ValueAnalysis.eval_fn_pointer stmt entry_point in
    let children = List.map Thread.mk children in

    (if not @@ Cil.isScalarType @@ Cil.typeOf arg then
      Imprecision.add (ThreadParamArray (stmt))
    );

    let tid = ExpStructEq.Set.singleton tid in

    (* Initial state will be computed in fixpoint loop *)
    let initial_state = Thread.InitialState.bottom in
    Context.add_threads ctx children, update_on_creates res stmt ctx children initial_state escaped_bases tid

  let update_on_join ctx res stmt tid =
    let open Cil_types in
    Logger.debug "Joining %a in %s" Exp.pretty tid (Result.show_threads res);

    (*
    let kf = Kernel_function.find_englobing_kf stmt in
    (if not @@ Result.is_thread res kf || Kernel_function.stmt_in_loop kf stmt then
      ValueAnalysis.add_imprecision (JoinInLoop stmt)
    );
    *)

    try
      let tid' = match tid.enode with
        | Lval lval -> Cil.mkAddrOf ~loc:tid.eloc lval
        | _ -> raise Exit
      in
      let pred = fun tids ->
        ExpStructEq.Set.cardinal tids = 1
        && ExpStructEq.Set.mem tid' tids
      in
      let joined_thread = M.filter (fun _ tids -> pred tids) res.thread_ids in
      let joined_thread = fst @@ M.choose joined_thread in (* TODO: uniquenes *)

      let ctx' = Context.remove_thread ctx joined_thread in
      ctx', update_on_noop res stmt ctx'

    (* Do not know what to join *)
    with _ -> ctx, update_on_noop res stmt ctx

  (* TODO: context join! diff with LSA *)
  let rec update_on_call (ctx : Context.t) res callsite callee =
    let callee_values = ValueAnalysis.eval_call callsite callee in
    let ctx', res' = List.fold_left (fun (ctx_acc, res_acc) callee ->
      let ctx', res' = compute_function ctx res_acc callee in
      Context.join ctx' ctx_acc, res'
    ) (ctx, res) callee_values
    in
    let threads = StatementMap.find_or_empty callsite res'.active_threads in
    let threads' = Thread.Set.union threads ctx'.active_threads in
    let res'' = {res' with active_threads = StatementMap.add callsite threads' res'.active_threads} in
    ctx', res''


  and compute_stmt ctx (res : Result.t) stmt =
    if Cache.mem stmt then (ctx, res)
    else
      let _ = Cache.add stmt in
      let () = Logger.debug ~level:4 ">   Entering stmt %a" Print_utils.pretty_stmt_loc stmt in
      let ctx', res' = match ConcurrencyModel.classify_stmt stmt with
        | Thread_create (tid, entry_point, arg) -> update_on_create ctx res stmt tid entry_point arg
        | Call (_, _, callee, _) -> update_on_call ctx res stmt callee
        | Thread_join tid -> update_on_join ctx res stmt tid
        | _ -> ctx, update_on_noop res stmt ctx
      in
      let _ = Logger.debug ~level:4 ">     ~> %s" (Thread.Set.show ctx'.active_threads) in

      (* Compute all succesors *)
      List.fold_left (fun (ctx_acc, res_acc) callee ->
        let ctx'', res'' = compute_stmt ctx' res_acc callee in
        Context.join ctx'' ctx_acc, res''
      ) (ctx', res') stmt.succs

  and compute_function ctx res fn =
    if not @@ Kernel_function.has_definition fn then ctx, res
    else
      let _ = Logger.debug ~level:3 ">   Entering function %a" Kernel_function.pretty fn in
      let stmt = Kernel_function.find_first_stmt fn in
      compute_stmt ctx res stmt

  let extract_state stmt = match ConcurrencyModel.classify_stmt stmt with
    | Thread_create (_, _, arg) ->
      let globals = ValueAnalysis.stmt_state stmt in
      let arg = ValueAnalysis.eval_expr stmt arg in
      let state = Thread.InitialState.mk globals arg in
      Logger.debug "Extracted at %a: %s" Print_utils.pretty_stmt_short stmt (Thread.InitialState.show state);
      state
    | _ -> (* TODO: arg *)
      let globals1 = ValueAnalysis.stmt_state ~after:true stmt in
      let globals2 = ValueAnalysis.stmt_state ~after:false stmt in
      let globals = Cvalue.Model.join globals1 globals2 in
      let arg = Cvalue.V.bottom in
      Thread.InitialState.mk globals arg

  (** Fixpoint computation *)

  let graph = ref ThreadGraph.empty

  let join_all acc thread =
    ThreadGraph.fold_vertex (fun thread acc ->
      try ValueAnalysis.set_active_thread thread;
      let state = CFG_utils.filter_stmts (fun _ -> true)
      |> List.fold_left (fun acc stmt ->
          let state = extract_state stmt in
          Thread.InitialState.join state acc
         ) acc
      in
      Thread.InitialState.join state acc
      with _ -> acc
    ) !graph acc

  let fixpoint_step (parent, stmts, child) initial_state =
    (* Join current initial state with states on all incoming create edges. *)
    let initial' = match Core0.ThreadApproximation.get () with
      | `Under ->
        Stmt.Set.elements stmts
        |> List.map extract_state
        |> List.fold_left Thread.InitialState.join initial_state
      | `Over -> join_all initial_state parent
    in
    Logger.debug ~level:2 "  > fixpoint step for %a -> %a (%d) with:\n %a"
      Thread.pp parent
      Thread.pp child
      (Stmt.Set.cardinal stmts)
      Core0.pp_globals (fst initial');

    let args =
      Stmt.Set.elements stmts
      |> List.map ConcurrencyModel.get_thread_arg
      |> Exp.Set.of_list
    in
    ValueAnalysis.set_active_thread parent;
    ValueAnalysis.update_thread child args initial';
    initial'

  module WTO = Graph.WeakTopological.Make(ThreadGraph)

  module Fixpoint = Graph.ChaoticIteration.Make
    (ThreadGraph)
    (struct
      include Thread.InitialState
      let widening = widen

      type edge = ThreadGraph.E.t
      let analyze = fixpoint_step
    end)

  let changed_states original (computed : Thread.InitialState.t Fixpoint.M.t) =
    Fixpoint.M.fold (fun thread state acc ->
      let state' = M.find thread original in
      if Thread.InitialState.equal state state' then acc
      else Thread.Set.add thread acc
    ) computed Thread.Set.empty

  (** Recompute initial states of all discovered threads based on the current
      thread-create graph. *)
  let compute_initial_states res =
    Logger.debug ~level:1 "Computing fixpoint of initial states";
    let wto = WTO.recursive_scc res.thread_graph res.main in
    let init thread =
      try M.find thread res.initial_states
      with Not_found -> Thread.InitialState.bottom (*failwith @@ Thread.show thread*)
    in
    let initial' = Fixpoint.recurse res.thread_graph' wto init FromWto 2 in
    let changed = changed_states res.initial_states initial' in
    let initial' = M.of_seq @@ Fixpoint.M.to_seq initial' in
    changed, {res with initial_states = initial'}

  let compute_vertex changed  thread res =
    (*if not @@ Thread.Set.mem thread changed then res
    else*)
      let _ = Logger.feedback "Computing thread %s" (Thread.show thread) in
      ValueAnalysis.set_active_thread thread;

      let active_threads = M.find thread res.parallel_threads in
      let ctx = Context.initial thread active_threads in
      let _, res = compute_function ctx res (Thread.get_entry_point thread) in
      res

  let compute_main main =
    ValueAnalysis.set_active_thread main;
    initial main

  let iteration = ref 1

  let cnt = ref 2

(** Compute a new thread graph based on thread's initial states. *)
  let rec compute_fixpoint res =
    let changed, res' = compute_initial_states res in

    Logger.feedback "Build iteration %d, changed: %s" !iteration (Thread.Set.show changed);

    iteration := !iteration + 1;

    if (!cnt = 0 &&
        ThreadGraph.nb_vertex res'.thread_graph = ThreadGraph.nb_vertex res.thread_graph)
    then res'
    else (* begin
    if Thread.Set.is_empty changed then res' *)
    begin
      let _ = cnt := !cnt - 1 in
      let _ = graph := res'.thread_graph' in
      compute_fixpoint @@
      ThreadGraph.fold_vertex (compute_vertex changed) res'.thread_graph res'
    end

  let compute () =
    Logger.feedback "Thread analysis started";

    let entry_point, _ = Globals.entry_point () in
    let entry_thread = Thread.mk ~is_main:true entry_point in
    ValueAnalysis.init entry_point;

    let g0 = compute_main entry_thread in
    let res = compute_vertex (Thread.Set.singleton entry_thread) entry_thread g0 in

    graph := res.thread_graph;
    let res = compute_fixpoint res in

    Logger.feedback "%a" ThreadAnalysis0.pp_short res;
    Logger.debug ~level:3 "Thread analysis: %a" pp res;
    Logger.debug ~level:3 "Active threads: %s" (ThreadAnalysis0.show_stmt_summaries res);
    res
end
