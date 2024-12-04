(* Imperative state generator used by all backends.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_datatype

module Make () = struct

  let main_thread : Thread.t option ref = ref None
  let active_thread : Thread.t option ref = ref None
  let thread_states = ref (Thread.Map.empty : Thread.InitialState.t Thread.Map.t)
  let thread_args = ref (Thread.Map.empty : Exp.Set.t Thread.Map.t)

  let init main =
    let main = Thread.mk main in
    main_thread := Some main;
    active_thread := Some main;
    thread_states := Thread.Map.singleton main Thread.InitialState.bottom;
    thread_args := Thread.Map.singleton main Exp.Set.empty

  let update_thread thread args state =
    thread_args := Thread.Map.add thread args !thread_args;
    try
      let old = Thread.Map.find thread !thread_states in
      let join = Thread.InitialState.join state old in
      thread_states := Thread.Map.add thread join !thread_states
    with Not_found ->
      thread_states := Thread.Map.add thread state !thread_states

  let set_active_thread t =
    Globals.set_entry_point (Thread.show t) false;
    active_thread := Some t

  let get_active_thread () =
    Option.get !active_thread

  let get_initial_state thread =
    try Thread.Map.find thread !thread_states
    with Not_found -> Thread.InitialState.bottom

end
