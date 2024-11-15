(* Utilities for value analyses.
 *
 * TODO: more precise handling of escaping for dynamic bases
 * TODO: make 'thread_local' parametric as part of the concurrency model
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let all_referenced_fns () =
  CFG_utils.filter_kfs (fun kf -> (Kernel_function.get_vi kf).vaddrof)

let all_possible_threads () =
  all_referenced_fns ()
  |> List.filter (fun kf ->
      let return_type = Kernel_function.get_return_type kf in
      let formals = Kernel_function.get_formals kf in
      Cil.isVoidPtrType return_type
      && (List.length formals) = 1
      && Cil.isVoidPtrType (List.hd formals).vtype
    )
