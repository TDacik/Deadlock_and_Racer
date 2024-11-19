(* Utilities for manipulation with Frama-C's memory bases.
 *
 * The functions are mostly used for filtering of memory accesses relevant
 * to data detection.
 *
 * TODO: more precise handling of escaping for dynamic bases
 * TODO: make 'thread_local' parametric as part of the concurrency model
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype
open Base

module Logger = Core0.Logger(struct let dkey = "base-utils" end)

let is_ghost = function
  | Var (var, _) ->
    (var.vghost || not var.vsource) || String.starts_with ~prefix:"__fc" var.vname
  | _ -> false

let is_allocated = function
  | Allocated _ -> true
  | _ -> false

(** Ignore constant strings *)
let is_interesting = function
  | Var _ | Allocated _ -> true
  | _ -> false

let is_thread_arg thread = function
  | Var (var, _) -> Kernel_function.is_formal var @@ Thread.get_entry_point thread
  | _ -> false

(** We cannot use the _Atomic attribute which is erased during preprocessing. We need to
    rely on encoding in the type name, e.g., atomic_int. *)
let is_atomic = function
  | Var (var, _) ->
    let typ = match var.vtype with TPtr (t, _) -> t | t -> t in
    let type_name = Format.asprintf "%a" Typ.pretty typ in
    String.starts_with ~prefix:"atomic_" type_name
  | _ -> false

let is_thread_local base =
  let is_thread_local_attr = function Attr ("thread", []) -> true | _ -> false in
  match base with
    | Var (var, _) -> List.exists is_thread_local_attr var.vattr
    | Allocated (var, _, _) ->
      begin match CFG_utils.find_allocation_target base with
      | Some var ->  List.exists is_thread_local_attr var.vattr
      | None -> false
      end
    | _ -> false

let cache = ref (None : Varinfo.Set.t option)

class escapes_expr = object
  inherit Visitor.frama_c_inplace

  method! vexpr e = match (Cil.stripCasts e).enode with
    | AddrOf (Var v, _) -> begin match !cache with
      | None -> failwith "Internal error"
      | Some c ->
        Logger.debug ~level:5 "Adding escaping variable: %a" Varinfo.pretty v;
        cache := Some (Varinfo.Set.add v c); DoChildren

      end
    | _ -> DoChildren

 end

class is_referenced_visitor = object
  inherit Visitor.frama_c_inplace

  method! vstmt stmt =
    let exprs = match stmt.skind with
      | Instr (Set (lval, expr, _)) -> [expr]
      | Instr (Call (_, {enode = Lval (Var fn, NoOffset); _}, exprs, _)) ->
        if ConcurrencyModel.is_atomic_fn fn then []
        else exprs
      | Instr (Call (_, _, exprs, _)) -> exprs
      | Instr (Local_init (_, ConsInit (fn, exprs, _), _)) ->
        if ConcurrencyModel.is_atomic_fn fn then []
        else exprs
      | _ -> []
    in
    let check_expr = (fun e -> ignore @@ (new escapes_expr)#vexpr e) in
    List.iter check_expr exprs;
    DoChildren

end

let is_referenced var =
  match !cache with
  | None ->
    let _ =  cache := Some Varinfo.Set.empty in
    let file = Ast.get () in
    let _  = Visitor.visitFramacFileSameGlobals (new is_referenced_visitor) file in
    Varinfo.Set.mem var (Option.get !cache)
  | Some _ -> Varinfo.Set.mem var (Option.get !cache)

(** Simple  escape analysis *)
let may_escape = function
  | Var (var, _) -> is_referenced var
  | Allocated (var, _, _) -> true
    (*
    begin match Cil_utils.find_allocation_target var with
    | Some v -> v.vglob || v.vaddrof
    | None -> true
    end
    *)
  | _ -> false

let keep_for_racer thread base =
  let not_ghost = not @@ is_ghost base in
  let not_atomic = not @@ is_atomic base in
  let not_thread_local = not @@ is_thread_local base in
  let may_escape = may_escape base in
  let global = Frama_c_kernel.Base.is_global base in
  let thread_arg = is_thread_arg thread base in
  let res =
    (is_interesting base)
    && not_ghost
    && not_atomic
    && (not_thread_local || may_escape)
    && (global || thread_arg || may_escape)
  in
  Logger.debug ~level:5 " Base: %a" Base.pretty base;
  Logger.debug ~level:5 " >    Global: %b" global;
  Logger.debug ~level:5 " >    Thread arg: %b" thread_arg;
  Logger.debug ~level:5 " >    May escape: %b" may_escape;
  Logger.debug ~level:5 " >    Not ghost: %b" not_ghost;
  Logger.debug ~level:5 " >    Not atomic: %b" not_atomic;
  Logger.debug ~level:5 " >    Not thread local: %b" not_thread_local;
  Logger.debug ~level:5 " >>>> Verdict: %b\n" res;
  res
