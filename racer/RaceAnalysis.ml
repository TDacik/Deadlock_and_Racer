(* Data race detection.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype

open ConcurrencyModel
open ValueAnalysis_sig

module Callstack = RelaxedCallstack

(** Imperative caching of already visited statements. *)
module Cache = struct

  module S = Set.Make(struct
    type t = Callstack.t * Stmt.t [@@deriving compare]
  end)

  let cache = ref S.empty

  let add callstack stmt =
    cache := S.add (callstack, stmt) !cache

  let mem callstack stmt =
    S.mem (callstack, stmt) !cache

end

module Context = struct

  type t = {
    locksets : LocksetAnalysis.Result.t;
    callstack : Callstack.t;
  }

  let mk locksets thread = {
    locksets = locksets;
    callstack = Callstack.empty thread;
  }

  let modify_cs fn {locksets; callstack} =
    {locksets = locksets; callstack = fn callstack}

  let push_call fn stmt = modify_cs (Callstack.push stmt fn)
  let pop_call = modify_cs Callstack.pop_call


end

module Result = struct

  module M = Base.Map
  module AS = MemoryAccess.Set

  type base_info = State.t * AS.t

  type t = {
    states : base_info M.t;
    races : Race.t list;
  }

  let empty = {
    states = M.empty;
    races = [];
  }

  let all_accesses self =
    M.fold (fun _ (_, xs) acc -> AS.elements xs @ acc) self.states []

  let update_one res access =
    let base = MemoryAccess.get_base access in
    let states' =
      try
        let (current, accs) = M.find base res.states in
        M.add base (State.update current access, AS.add access accs) res.states
      with Not_found ->
        M.add base (State.initial access, AS.singleton access) res.states
    in
    {res with states = states'}

  let is_precise_update stmt accesses =
    let is_ptr (t : Cil_types.typ) = match t with TPtr _ -> true | _ -> false in
    let bases = List.map MemoryAddress.base accesses in
    List.length bases <= 1
    || List.for_all (fun (b1, b2) ->
       let v1 = Base.to_varinfo b1 in
       let v2 = Base.to_varinfo b2 in
       Base.equal b1 b2
       || (not @@ Typ.equal v1.vtype v2.vtype)
     ) (BatList.cartesian_product bases bases)

  let update_reads stmt callstack lockset res accesses =
    List.map (fun (a, precise) -> MemoryAccess.mk_read stmt a lockset callstack precise) accesses
    |> List.fold_left update_one res

  let update_writes stmt callstack lockset res accesses =
    let precise = is_precise_update stmt accesses in
    List.map (fun a -> MemoryAccess.mk_write stmt a lockset callstack precise) accesses
    |> List.fold_left update_one res

  (** Whenever possible, return write-write races *)
  let ranking = function
    | [] -> []
    | races ->
      try [List.find (fun race -> race.Race.kind = Race.Must) races ]
      with Not_found -> [List.hd races]

  let has_must_race res = List.exists (fun race -> race.Race.kind = Race.Must) res.races

  let is_race_free res = List.is_empty res.races

  let choose res =
    try List.find (fun race -> race.Race.kind = Race.Must) res.races
    with Not_found -> List.hd res.races

  let check_races threads parallel base accesses =
    let accesses = MemoryAccess.Set.elements accesses in
    BatList.fold_lefti (fun acc i x ->
      BatList.fold_lefti (fun acc j y ->
        if i <= j then match Race.check threads parallel x y with
          | Some race -> race :: acc
          | None -> acc
        else acc
      ) acc accesses
    ) [] accesses
    |> ranking

  let find_races res parallel_res threads =
    let races = M.fold (fun base (state, accesses) acc -> match state with
      | State.Shared_modified _ ->
        check_races threads parallel_res base accesses @ acc
      | State.Exclusive owner when not @@ ThreadAnalysis.Result.is_unique threads owner ->
        check_races threads parallel_res base accesses @ acc
      | _ -> acc
    ) res.states []
    in
    {res with races = races}


  let report res =
    Racer.debug "Memory accesses:";
    List.iter (fun a -> Racer.debug "\t%s" (MemoryAccess.show a)) (all_accesses res);
    let str = "Race analysis results:" in
    M.bindings res.states
    |> List.fold_left (fun acc (var, (s, _)) ->
        Format.asprintf "%s\n\    %a (weak: %b) : %s"
          acc Base.pretty var (Base.is_weak var) (State.show s)
       ) str
    |> Racer.result "%s";
    match res.races with
    | [] -> Racer.result ~level:0 "No data races found"
    | races -> List.iter (fun race -> Racer.result ~level:0 "%s" (Race.report race)) races

  let to_json_access access =
    let open MemoryAccess in
    `Assoc [
      "kind",     `String (MemoryAccess.show_kind access.kind);
      "offset",   `String (Format.asprintf "%a" Int_Intervals.pretty @@ get_offset access);
      "thread",   `String (Format.asprintf "%a" Thread.pp @@ MemoryAccess.get_thread access);
      "stmt",     `String (Format.asprintf "%a" Print_utils.pretty_stmt_short @@ MemoryAccess.get_stmt access);
      "locksets", `String (Format.asprintf "%a" Lock.PowerSet.pp access.locksets);
    ]

  let to_json_race race =
    let open Race in
    let fst, snd = race.accesses in
    `Assoc [
      "location", `String (Format.asprintf "%a" Base.pretty race.base);
      "offset",   `String (Format.asprintf "%a" Int_Intervals.pretty race.offset);
      "access 1", (to_json_access @@ fst);
      "access 2", (to_json_access @@ snd);
    ]


  let to_json res =
    `Assoc [
      "races", `List (List.map to_json_race res.races);
    ]

  let out_json res filepath =
    let file = Format.asprintf "%a" Frama_c_kernel.Filepath.Normalized.pp_abs filepath in
    let channel = open_out_gen [Open_creat; Open_wronly] 0o666 file in
    Yojson.Basic.(pretty_to_channel channel (to_json res));
    close_out channel

  let filter_shared threads res =
    M.filter (fun _ (state, _) -> match state with
      | State.Shared_modified _ -> true
      | State.Exclusive owner -> not @@ ThreadAnalysis.Result.is_unique threads owner
      | _ -> false
    ) res

  let find_max states =
    M.fold (fun base (_, accesses) (max_base, max_n) ->
      let n = AS.cardinal accesses in
      if n > max_n then (Some base, n)
      else (max_base, max_n)
    ) states (None, -1)

  let show_stats threads res =
    let states = filter_shared threads res.states in
    match find_max states with
      | (None, _) ->
        Format.asprintf "      - # shared bases: 0\n" (* Needed for scripts *)
      | (Some base, max) ->
        Format.asprintf "      - # shared bases: %d\n" (M.cardinal states) ^
        Format.asprintf "      - # max accesses: %d (%a)" max Base.pretty base

end

module Make (ValueAnalysis : VALUE_ANALYSIS) = struct

  open Context

  module Result = Result

  (** Get set of locksets for given stmt, possible refined by Cvalue state. *)
  let stmt_lockset ctx stmt =
    let callstack = ctx.callstack in
    let state = ValueAnalysis.stmt_state stmt ~callstack in
    LocksetAnalysis.Result.stmt_locksets ~callstack ~state ctx.locksets stmt

  let update_on_access ctx res stmt =
    let reads, writes = match stmt.skind with
      | Instr _ | Return _ -> ValueAnalysis.memory_accesses stmt
      | If (e, _, _, _) | Switch (e, _, _, _) -> ValueAnalysis.expr_reads stmt e, []
      | _ -> [], []
    in
    Racer.debug ">     Accesses:";
    Racer.debug ">       - Reads: %a" MemoryAddress.pp_list reads;
    Racer.debug ">       - Writes: %a" MemoryAddress.pp_list writes;

    let lss = stmt_lockset ctx stmt in

    let res' = Result.update_reads stmt ctx.callstack lss res reads in
    Result.update_writes stmt ctx.callstack lss res' writes

  let rec update_on_call ctx res stmt typ lval fn args =
    let reads1 = match typ with
      | Direct kf -> []
      | Extern _ -> []
      | Pointer ptr -> ValueAnalysis.expr_reads stmt ptr
    in
    let writes1 = match lval with
      | None -> []
      | Some lval ->
        let expr = Cil.new_exp ~loc:(Stmt.loc stmt) (Cil_types.Lval lval) in
        ValueAnalysis.expr_reads stmt expr
    in

    let reads2 = List.fold_left (fun acc exp ->
      if Cil.isPointerType @@ Cil.typeOf exp then acc
      else acc @ ValueAnalysis.expr_reads stmt exp
    ) [] args in

    let callees = ValueAnalysis.eval_call stmt fn in

    Racer.debug ">     Accesses on call:";
    Racer.debug ">       - Reads (fn pointer call): %a" MemoryAddress.pp_list reads1;
    Racer.debug ">       - Reads (pass-by-value): %a" MemoryAddress.pp_list reads2;
    Racer.debug ">       - Writes (lval init): %a" MemoryAddress.pp_list writes1;
    Racer.debug ">       - Callees:\n";

    let lss = stmt_lockset ctx stmt in
    let res' = Result.update_writes stmt ctx.callstack lss res writes1 in
    let res' = Result.update_reads stmt ctx.callstack lss res' (reads1 @ reads2) in

    List.fold_left (fun acc callee ->
      if Kernel_function.has_definition callee then compute_function ctx acc callee stmt
      else update_on_access ctx res stmt (* We drop previously computed! *)
    ) res' callees

  and compute_stmt ctx res stmt =
    if Cache.mem ctx.callstack stmt then res
    else
      let () = Cache.add ctx.callstack stmt in
      Racer.debug ">   Computing stmt %a" Print_utils.pretty_stmt_loc stmt;
      let res' = match ConcurrencyModel.classify_stmt stmt with
        | Call (typ, lval, fn, args) -> update_on_call ctx res stmt typ lval fn args
        | Other | Return -> update_on_access ctx res stmt
        | _ -> res (* Ignore accesses by lock(.) and unlock(.) *)
      in
      List.fold_left (compute_stmt ctx) res' stmt.succs

  and compute_function ctx res fn callsite =
    if Kernel_function.has_noreturn_attr fn then res (* TODO: do not continue *)
    else
      let _ = Racer.debug ">   Computing function %a" Kernel_function.pretty fn in
      let stmt = Kernel_function.find_first_stmt fn in
      let ctx' = Context.push_call fn callsite ctx in
      compute_stmt ctx' res stmt

  and compute_thread ctx res fn =
    let stmt = Kernel_function.find_first_stmt fn in
    compute_stmt ctx res stmt

  let compute threads parallel_res locksets =
    Racer.feedback "Starting memory access analysis";
    let init_res = Result.empty in
    let thread_graph = ThreadAnalysis.Result.get_threads threads in

    let res = ThreadGraph.fold_vertex (fun thread acc ->
      Racer.debug "> Starting analysis of thread: %a" Thread.pp thread;
      let entry_point = Thread.get_entry_point thread in
      ValueAnalysis.set_active_thread thread;
      let ctx = Context.mk locksets thread in
      compute_thread ctx acc entry_point
    ) thread_graph init_res
    in
    Racer.feedback "Checking resulting memory accesses for races:\n%s"
      (Result.show_stats threads res);
    let races = Result.find_races res parallel_res threads in
    Racer.feedback "Race analysis finished";
    races

end
