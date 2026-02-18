(* Postprocessing of deadlocks based on detected imprecisions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Imprecision
open DeadlockAnalysis


(** TODO: backend imprecisions *)
let run deadlocks =
  let filter =
    if List.is_empty deadlocks then
      (function (FunctionPointer _ | Lock _ | Backend _) -> true | _ -> false)
    else if List.exists Deadlock.is_must deadlocks then
      (function (FunctionPointer _ | Lock _ | ActiveWaiting _ | Backend (Lock _)) -> false
       | _ -> true)
    else (fun _ -> true)
   in
   Imprecision.report filter
