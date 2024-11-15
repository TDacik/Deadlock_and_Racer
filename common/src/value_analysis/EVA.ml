(* Value analysis backend using Frama-C's EVA plugin.
 *
 * TODO: how to do caching also for main thread?
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype
open Locations

module Analysis = Eva.Analysis
module Old = Eva.Eva_results
module Eva = Eva.Results

module Callstack = RelaxedCallstack

module Self = struct

  module Logger = Core0.Logger(struct let dkey = "backend:eva" end)

  let name = "eva"

  (** Include references to active thread and initial states. *)
  include ValueAnalysis_Base.Make ()

  module Cache = struct
      include Stdlib.Map.Make(struct
        type t = Thread.t * Thread.InitialState.t [@@deriving compare]
      end)

      let show self =
        Core0.debug "Cache:";
        iter (fun (k, s) _ ->
        Core0.debug " -  (%s, %s)"
          (Thread.show k)
          (Thread.InitialState.show s)
        ) self

  end

  let init kf =
    init kf

  let cache = ref Cache.empty

  let set_args kf arg =
    if List.length @@ Kernel_function.get_formals kf == 1 then
      Old.set_main_args [arg]
    else ()

  let cache_or_compute thread =
    let initial_state = get_initial_state thread in
    Cache.show !cache;
    try
      let res = Cache.find (thread, initial_state) !cache in
      Core0.debug "Using cache for (%s, %s)"
        (Thread.show thread)
        (Thread.InitialState.show initial_state);
      Old.set_results res
    with Not_found ->
      let _ = Core0.debug "Not using cache..." in
      let _ = Analysis.compute () in
      let res = Old.get_results () in
      cache := Cache.add (thread, initial_state) res !cache

  (** Run the analysis when a thread is set as active *)
  let set_active_thread =
    let very_first = ref true in
    (fun thread ->
      set_active_thread thread;
      if !very_first then begin
        very_first := false;
        assert(Thread.is_main thread);
        Globals.set_entry_point "main" false;
        Old.use_default_initial_state ();
        Old.use_default_main_args ();
        cache_or_compute thread
      end
      else
        let entry_point = Thread.get_entry_point thread in
        let entry_point_name = Thread.show thread in
        let globals, arg = get_initial_state thread in
        Globals.set_entry_point entry_point_name false;
        Old.set_initial_state globals;
        set_args entry_point arg;
        cache_or_compute thread
    )

  let is_reachable = Eva.is_reachable

  let concretise loc =
    Logger.debug "Concretizing locations %a" Location_Bytes.pretty loc;
    match Location_Bytes.cardinal loc with
      | None -> []
      | Some n when n > Integer.of_int 12 -> [] (* TODO *)
      | _ ->
        Location_Bytes.fold_i (fun base offsets acc ->
          let ints = Ival.fold_int List.cons offsets [] in
          let xs = List.map (fun i -> (base, i)) ints in
          xs @ acc
        ) loc []

  let concretise_zone zone =
    try Locations.Zone.fold_i (fun base offsets acc ->
      if Int_Intervals.is_top offsets then acc
      else (base, offsets) :: acc
    ) zone []
    with Abstract_interp.Error_Top -> []

  let cs_pred cs eva_cs = match cs with
    | None -> true
    | Some cs -> Callstack.match_with_eva cs eva_cs

  let eval_expr ?callstack stmt expr =
    Eva.before stmt
    |> Eva.filter_callstack (cs_pred callstack)
    |> Eva.eval_exp expr
    |> Eva.as_cvalue

  let eval_expr_concretised ?callstack stmt expr =
    let cvalue = match callstack with
      | None -> eval_expr stmt expr
      | Some callstack -> eval_expr ~callstack stmt expr
    in
    concretise cvalue

  let stmt_state ?(after=false) ?callstack stmt =
    let where = if after then Eva.after else Eva.before in
    where stmt
    |> Eva.filter_callstack (cs_pred callstack)
    |> Eva.get_cvalue_model

  let eval_call stmt expr =
    Eva.before stmt
    |> Eva.eval_exp expr
    |> Eva.as_cvalue
    |> concretise
    |> List.map (fun (b, _) -> Format.asprintf "%a" Base.pretty b)
    |> List.map Globals.Functions.find_by_name

  let eval_fn_pointer stmt expr = match (Cil.stripCasts expr).enode with
    (* For some reason, EVA returns bottom for '*fn'. Thus, we strip the star. *)
    | Lval (Mem exp, NoOffset) -> eval_call stmt exp
    | _ -> eval_call stmt expr (* Should ever happen? *)

  let get_accesses with_locals zone =
    let thread = get_active_thread () in
    concretise_zone zone
    |> List.filter (fun (base, _) -> BaseUtils.keep_for_racer thread base)

  let memory_accesses ?(local=false) stmt =
    let open Locations in
    let read_zone = Inout.stmt_inputs stmt in
    let write_zone = Inout.stmt_outputs stmt in
    Logger.debug " > Read: %a" Zone.pretty read_zone;
    Logger.debug " > Write: %a" Zone.pretty write_zone;

    let reads = get_accesses local read_zone in
    let writes = get_accesses local write_zone in
    (reads, writes)

  let expr_reads ?(local=false) stmt expr =
    let open Locations in
    let read_zone = Inout.expr_inputs stmt expr in
    Logger.debug "> Accesses of %a" Exp.pretty expr;
    Logger.debug ">   Read: %a" Zone.pretty read_zone;
    get_accesses local read_zone

end

include ValueAnalysis_builder.Make(Self)
