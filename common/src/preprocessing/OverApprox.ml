
module F = File

open Cil_types
open Cil_datatype

let mk_stmt kind =
  Cil.mkStmt ~valid_sid:true ~sattr:[] kind

let mk_nondet_bool loc fn (var : Varinfo.t) =
  let stmt = mk_stmt (Instr (Call (Some (Var var, NoOffset), (Var fn.svar), [], loc))) in
  let expr = Cil.evar var in
  (stmt, expr)

let is_not_local lval = match fst lval with
  | Mem _ -> true
  | Var var -> var.vglob || var.vformal

class visitor (nondet_fn : Fundec.t) var = object (self)
  inherit Cil.nopCilVisitor

  method! vstmt stmt = match stmt.skind with
    | Instr (Set (lval, exp, loc)) when is_not_local lval ->
      Core0.debug "Adding line at %d" (Print_utils.stmt_line stmt); 
      let init, cond = mk_nondet_bool loc nondet_fn var in
      let b_then = Cil.mkBlock [Cil.mkStmt ~valid_sid:true ~sattr:[] stmt.skind] in
      let b_else = Cil.mkBlock [] in
      let kind = If (cond, b_then, b_else, loc) in
      let res = Cil.mkStmt ~valid_sid:true ~sattr:[] kind in
      let res = {(Cil.mkStmtCfgBlock [init; res]) with labels=stmt.labels} in
      F.must_recompute_cfg (Option.get self#current_func);
      ChangeTo res

    | Instr (Call (Some lval, fn, args, loc)) when is_not_local lval ->
      Core0.debug "Adding line at %d" (Print_utils.stmt_line stmt);
      let init, cond = mk_nondet_bool loc nondet_fn var in
      let b_then = Cil.mkBlock [Cil.mkStmt ~valid_sid:true ~sattr:[] stmt.skind] in
      let b_else = Cil.mkBlock [] in
      let kind = If (cond, b_then, b_else, loc) in
      let res = Cil.mkStmt ~valid_sid:true ~sattr:[] kind in
      let res = {(Cil.mkStmtCfgBlock [init; res]) with labels=stmt.labels} in
      F.must_recompute_cfg (Option.get self#current_func);
      ChangeTo res

    | _ -> DoChildren

end

let do_transform file =
  Core0.debug "Overapproximation transformation";

  (** Global variable used to store non-deterministic values *)
  let var = Cil.makeGlobalVar ~temp:true ~ghost:true "nondet" @@ Cil.int16_t () in
  var.vdefined <- true;
  let initinfo = {init = Some (CInit (SingleInit (Cil.zero ~loc:Fileloc.unknown)))} in
  file.globals <- GVar (var, initinfo, Fileloc.unknown) :: file.globals;

  (** Function implementing non-determinism *)
  let fn = Cil.emptyFunction "get_nondet" in
  let spec = Cil.empty_funspec () in
  Cil.setReturnType fn @@ Cil.int16_t ();
  file.globals <- GFunDecl (spec, fn.svar, Fileloc.unknown) :: file.globals;

  Cil.visitCilFileSameGlobals (new visitor fn var) file

let transform file =
  if Core0.NondetAssignments.get () then do_transform file
  else ()

let () =
  let category = F.register_code_transformation_category "over-approx" in
  F.add_code_transformation_before_cleanup ~deps:[] category transform
