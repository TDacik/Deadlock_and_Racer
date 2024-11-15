(* State automaton for memory location states.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryAccess

type t =
  | Read_only of Thread.t
  | Exclusive of Thread.t
  | Shared of Thread.Set.t
  | Shared_modified of Thread.Set.t * Thread.Set.t

let show = function
  | Read_only owner -> Format.asprintf "Read only by %s" (Thread.show owner)
  | Exclusive owner -> Format.asprintf "Exclusive for %s" (Thread.show owner)
  | Shared readers -> Format.asprintf "Shared by %s" (Thread.show_list @@ Thread.Set.elements readers)
  | Shared_modified (readers, writers) ->
    Format.asprintf "Shared by %s, modified by %s"
      (Thread.show_list @@ Thread.Set.elements (Thread.Set.union readers writers))
      (Thread.show_list @@ Thread.Set.elements writers)

let initial access =
  let thread = MemoryAccess.get_thread access in
  match access.kind with
    | Read -> Read_only thread
    | Write -> Exclusive thread

(* Transition function of automaton *)
let update state access : t =
  let thread = MemoryAccess.get_thread access in
  match state with
  | Read_only owner when Thread.equal thread owner && access.kind = Read -> Read_only owner
  | Read_only owner when Thread.equal thread owner && access.kind = Write -> Exclusive owner
  | Read_only owner when access.kind = Read -> Shared (Thread.Set.of_list [owner; thread])
  | Read_only owner when access.kind = Write ->
    Shared_modified (Thread.Set.singleton owner, Thread.Set.singleton thread)

  | Exclusive owner when Thread.equal thread owner -> Exclusive owner
  | Exclusive owner when access.kind = Read ->
    Shared_modified (Thread.Set.singleton owner, Thread.Set.singleton thread)
  | Exclusive owner when access.kind = Write ->
    Shared_modified (Thread.Set.empty, Thread.Set.of_list [owner; thread])

  | Shared readers when access.kind = Read -> Shared (Thread.Set.add thread readers)
  | Shared readers when access.kind = Write ->
    Shared_modified (readers, Thread.Set.singleton thread)

  | Shared_modified (readers, writers) when access.kind = Read ->
    Shared_modified (Thread.Set.add thread readers, writers)
  | Shared_modified (readers, writers) when access.kind = Write ->
    Shared_modified (readers, Thread.Set.add thread writers)
