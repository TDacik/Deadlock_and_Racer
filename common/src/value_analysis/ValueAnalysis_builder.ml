(* Builder of backends.
 *
 * TODO: move memory accesses here.
 * TODO: move logging from instances to this builder.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype

open ValueAnalysis_sig
open ValueAnalysis_utils

module Make (B : VALUE_ANALYSIS) = struct

  module Logger = Core0.Logger(struct let dkey = "backend:" ^ B.name end)

  include B

  let eval_expr_concretised ?callstack stmt expr = match expr.enode with
    (* Simple access to non-pointer variable *)
    | AddrOf (Var var, NoOffset) when not @@ Cil.isPointerType var.vtype ->
      [Base.of_varinfo var , Integer.of_int 0]
    | _ -> begin match callstack with
      | None -> B.eval_expr_concretised stmt expr
      | Some callstack -> B.eval_expr_concretised ~callstack stmt expr
    end

  let eval_call stmt expr =
    Logger.debug "Evaluating call: %a" Exp.pretty expr;
    let res = match ConcurrencyModel.classify_stmt stmt with
      | Call (Direct kf, _, _, _ ) -> [kf]
      | Call (Pointer ptr, _, _, _) ->
        if Core0.OverApproxFunctionPointers.get () then all_referenced_fns ()
        else B.eval_fn_pointer stmt expr
      | _ -> B.eval_call stmt expr
    in
    List.iter (fun f -> Logger.debug ">  %a" Kernel_function.pretty f) res;
    res

 let eval_fn_pointer stmt expr =
    Logger.debug "Evaluating function pointer: %a" Exp.pretty expr;
    match expr.enode with
    (* Simple reference to global function *)
    | AddrOf (Var var, NoOffset) -> [Globals.Functions.get var]
    | _ ->
      if Core0.OverApproxFunctionPointers.get () then all_referenced_fns ()
      else B.eval_fn_pointer stmt expr

end
