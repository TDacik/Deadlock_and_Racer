(* Representation of locks, locksets and sets of locksets.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open ConcurrencyModel_types
open ConcurrencyModel_types.LockKind

type t = {
  base: Base.t;
  kind: LockKind.t;
  offset: Integer.t;
  status: Cil_types.lval Option.t;
  callstack: Callstack.t Option.t;
}

let global_lock =
  let cache = ref None in
  fun () -> match !cache with
    | Some lock -> lock
    | None ->
      let base = Base.of_varinfo @@ Cil.makeGlobalVar "__racerf_global_lock" Cil.voidType in
      let lock = {
        base = base;
        kind = {blocking = true; read_lock = false; reentrant = true};
        offset = Integer.zero;
        callstack = None;
        status = None;
      }
      in
      cache := Some lock;
      lock

let is_read lock = lock.kind.read_lock
let is_write lock = not @@ is_read lock
let is_blocking lock = lock.kind.blocking
let is_reentrant lock = lock.kind.reentrant

let get_callstack lock = Option.get lock.callstack

let compare lock1 lock2 =
  let aux = Base.compare lock1.base lock2.base in
  if aux <> 0 then aux else Integer.compare lock1.offset lock2.offset

let equal lhs rhs = (compare lhs rhs) == 0

let show lock =
  if Integer.is_zero lock.offset then Format.asprintf "%a" Base.pretty lock.base
  else Format.asprintf "%a[%a]" Base.pretty lock.base Integer.pretty lock.offset

let mk ?callstack ?(kind={blocking=true; read_lock=true; reentrant=true}) ?status base offset = {
  base = base;
  kind = kind;
  offset = offset;
  callstack = callstack;
  status = status;
}

let hash = Hashtbl.hash

module Lock = struct
  type nonrec t = t
  let show = show
  let compare = compare
  let equal = equal
end

include Datatype.Printable(Lock)

module Set = struct
  include BatSet.Make(Lock)

  let show xs =
    elements xs
    |> List.map show
    |> String.concat ", "
    |> (fun s -> "{" ^ s ^ "}")

  include Datatype.Printable(struct
    type nonrec t = t
    let show = show
  end)

  let rw_disjoint ls1 ls2 =
    not @@ exists (fun l1 ->
      exists (fun l2 ->
        Lock.equal l1 l2
        && (is_write l1 || is_write l2)
      ) ls2
    ) ls1

end

module PowerSet = struct
  include BatSet.Make(Set)

  let union_list = List.fold_left union empty

  let add_each ls update =
    if List.is_empty update then singleton ls
    else List.fold_left (fun acc lock -> add (Set.add lock ls) acc) empty update

  let remove_each ls update =
    if List.is_empty update then singleton ls
    else List.fold_left (fun acc lock -> add (Set.remove lock ls) acc) empty update

  let flatten_union lss = fold Set.union lss Set.empty

  let flatten_inter lss =
    if is_empty lss then Set.empty (* Unreachable statement *)
    else fold Set.inter lss (choose lss)

  let show xs =
    elements xs
    |> List.map Set.show
    |> String.concat ", "
    |> (fun s -> "{" ^ s ^ "}")

  include Datatype.Printable(struct
    type nonrec t = t
    let show = show
  end)

end
