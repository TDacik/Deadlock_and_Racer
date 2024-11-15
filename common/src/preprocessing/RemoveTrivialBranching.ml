(* Removal of trivial branching.
 *
 * The goal of this pass is to simplify the following pattern introduced by loop unrolling:
 *
 *   type i = const;
 *   { if expr(i) then C1 else C2; ... }
 *
 * TODO: the implementation with visitors crash on pthread-driver-races benchmarks.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype

let is_instr stmt = match stmt.skind with Instr _ -> true | _ -> false
let is_block stmt = match stmt.skind with Block _ -> true | _ -> false
let is_block stmt = match stmt.skind with Block _ -> true | _ -> false

let exp_as_const expr = match (Cil.stripCasts expr).enode with
  | Const c -> c
  | _ -> raise Exit

class substitution var const = object
  inherit Visitor.frama_c_copy (Project.create "dummy")

  method! vexpr e = match e.enode with
    | Lval (Var var', NoOffset) ->
      if Varinfo.equal var var' then ChangeTo {e with enode = Const const}
      else DoChildren
    | _ -> DoChildren

end

let inline fst snd =
  match fst.skind, snd.skind with
    | Instr (Local_init (v, AssignInit (SingleInit e), loc)), Block b ->
      let const = exp_as_const e in
      let head, tail = List.hd b.bstmts, List.tl b.bstmts in
      begin match head.skind with
      | If (cond, b_then, b_else, loc) ->
        let cond' = Visitor.visitFramacExpr (new substitution v const) cond in
        let cond'' = Cil.constFoldToInt ~machdep:true cond' in
        let res = match cond'' with
          | Some i when Integer.is_one i -> b_then
          | Some i when Integer.is_zero i -> b_else
          | None -> raise Exit
        in

        let res = Cil.mkStmtCfgBlock (fst :: res.bstmts @ tail) in
        res
      | _ -> raise Exit
      end
    | _ -> raise Exit

class visitor proj = object
  inherit Visitor.frama_c_refresh proj

  method! vstmt_aux stmt = match stmt.skind with
    | Block block when List.length block.bstmts > 1 ->
      begin try
        let fst, snd, tail = match block.bstmts with x :: y :: tl -> (x, y, tl) in
        let res = Cil.mkStmtCfgBlock (inline fst snd :: tail) in
        ChangeDoChildrenPost (res, fun x -> x)
      with Exit | _ -> DoChildren
      end
    | _ -> DoChildren

end

let run () =
  Core0.debug "Removing trivial IFs";
  let proj' = Frama_c_kernel.File.create_project_from_visitor "remove-ifs" (new visitor) in
  Project.set_current proj';
  Ast.mark_as_changed ();
  let file = Ast.get () in
  Cfg.clearFileCFG ~clear_id:true file;
  Cfg.computeFileCFG file
