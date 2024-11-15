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

(*
class is_referenced = object
  inherit Visitor.frama_in_place

  module M = Base.Map

  val mutable cache = M.empty
  val mutable res = (None : bool option)

  method! vexpr e = match e.enode with
    | Lval (Var var', NoOffset) ->
      if Varinfo.equal var var' then ChangeTo {e with enode = Const const}
      else DoChildren
    | _ -> DoChildren

let is_referenced =
  let cache := ref M.empty in
  fun base ->
    try M.find base !cache
    with Not_found ->


class substitution = object
  inherit Visitor.frama_c_inplace
*)
(** Very simple approximation of escape analysis:
    - All referenced variables can escape
    - All dynamically allocated bases can escape *)
let may_escape = function
  | Var (var, _) -> var.vaddrof
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
