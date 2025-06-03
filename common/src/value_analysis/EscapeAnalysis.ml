(* Escape analysis.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Base
open Cil_types
open Cil_datatype

module Logger = Core0.Logger(struct let dkey = "escape" end)

(** Cache of escaped variables *)
let cache = ref (None : Base.Set.t option)

let cache_add base = match !cache with
  | None -> assert false
  | Some c -> cache := Some (Base.Set.add base c)

module Make (Backend : ValueAnalysis_sig.VALUE_ANALYSIS) = struct

  let get_bases = function
    | Cvalue.V.Top _ -> [] (* TODO: default + garbled mix *)
    | Cvalue.V.Map map -> Cvalue.V.M.fold (fun b _ acc -> b :: acc) map []

  let escapes_expr stmt expr =
    Core0.debug "Checking escape of %a at %a" Exp.pretty expr Stmt.pretty stmt;
    Backend.eval_expr stmt expr
    |> get_bases
    |> List.iter (fun b -> Core0.debug " - escape: %a\n" Base.pretty b; cache_add b)

  let escapes_stmt stmt = match ConcurrencyModel.classify_stmt stmt with
    | Thread_create (_, _, arg) ->
      escapes_expr stmt arg
    | _ -> begin match stmt.skind with
      | Instr (Set (lval, expr, _)) -> escapes_expr stmt expr
      | _ -> ()
    end

  class is_referenced_visitor = object
    inherit Visitor.frama_c_inplace

    method! vstmt stmt =
      escapes_stmt stmt;
      DoChildren
    end

  let compute threads = match !cache with
    | Some _ -> ()
    | None ->
      let _ = cache := Some Base.Set.empty in
      let file = Ast.get () in
      let g = ThreadAnalysis.Result.get_threads threads in
      ThreadGraph.fold_vertex (fun thread () ->
        Backend.set_active_thread thread;
        Visitor.visitFramacFileSameGlobals (new is_referenced_visitor) file
      ) g ()

end

let may_escape (b : Base.t) = match b with
  | Allocated (var, _, _) -> true (* TODO: improve *)
  | _ -> Base.Set.mem b (Option.get !cache)
