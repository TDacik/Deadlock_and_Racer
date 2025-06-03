open Cil_types
open Cil_datatype

let mk_stmt kind =
  Cil.mkStmt ~valid_sid:true ~sattr:[] kind

let find_fundec stmt =
  try
    let kf = Kernel_function.find_englobing_kf stmt in
    kf.fundec
  with Not_found ->
    Core0.result "No function for stmt: %a" Cil_printer.pp_stmt stmt;
    exit 1

let mk_nondet_bool loc (var : Varinfo.t) =
  let fn = Cil.emptyFunction "get_nondet" in
  let stmt = mk_stmt (Instr (Call (Some (Cil.var var), Cil.evar fn.svar, [], loc))) in
  let expr = Cil.evar var in
  (stmt, expr)

let is_not_local lval = match fst lval with
  | Mem _ -> true
  | Var var -> var.vglob || var.vformal

class visitor var proj = object
  inherit Visitor.frama_c_refresh proj

  method! vstmt_aux stmt = match stmt.skind with
    | Instr (Set (lval, exp, loc)) when is_not_local lval ->
      let init, cond = mk_nondet_bool loc var in
      let b_then = Cil.mkBlock [Cil.mkStmt ~valid_sid:true ~sattr:[] stmt.skind] in
      let b_else = Cil.mkBlock [] in
      let kind = If (cond, b_then, b_else, loc) in
      let res = Cil.mkStmt ~valid_sid:true ~sattr:[] kind in
      let res = {(Cil.mkStmtCfgBlock [init; res]) with labels=stmt.labels} in
      ChangeTo res

    | Instr (Call (Some lval, fn, args, loc)) when is_not_local lval ->
      let init, cond = mk_nondet_bool loc var in
      let b_then = Cil.mkBlock [Cil.mkStmt ~valid_sid:true ~sattr:[] stmt.skind] in
      let b_else = Cil.mkBlock [] in
      let kind = If (cond, b_then, b_else, loc) in
      let res = Cil.mkStmt ~valid_sid:true ~sattr:[] kind in
      let res = {(Cil.mkStmtCfgBlock [init; res]) with labels=stmt.labels} in
      ChangeTo res

    | _ -> DoChildren

end

let run () =
  Core0.debug "Overapproximation transformation";
  let var = Cil.makeGlobalVar ~source:false ~temp:true "nondet" @@ Cil.int16_t () in
  Globals.Vars.add var {init = Some (SingleInit (Cil.zero ~loc:Location.unknown))};

  let proj' = Frama_c_kernel.File.create_project_from_visitor "overapprox" (new visitor var) in
  Project.set_current proj';
  Ast.mark_as_changed ();
  let file = Ast.get () in
  Cfg.clearFileCFG ~clear_id:true file;
  Cfg.computeFileCFG file
