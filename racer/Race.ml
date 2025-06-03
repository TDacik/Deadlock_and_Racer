(* Representation of data races.
 *
 * TODO: add reason to May race
 * TODO: clean up race checking
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryAccess

type race_kind = Must | May

type t = {
  kind : race_kind;
  base : Base.t;
  offset : Int_Intervals.t;
  accesses : MemoryAccess.t * MemoryAccess.t;
}

(** Check whether a pair of accesses is a race *)
let is_race_base threads parallel combine_lss self other =
  let open ThreadAnalysis in
  let ls1 = combine_lss self.locksets in
  let ls2 = combine_lss other.locksets in
  let thread1 = get_thread self in
  let thread2 = get_thread other in
  let stmt1 = get_stmt self in
  let stmt2 = get_stmt other in
  let offset1 = get_offset self in
  let offset2 = get_offset other in

  let different =
    not @@ Thread.equal thread1 thread2 || not @@ Result.is_unique threads thread1
  in
  let disjoint = Lock.Set.rw_disjoint ls1 ls2 in
  let parallel =
    ParallelAnalysis.Result.may_run_in_parallel parallel self.callstack other.callstack
  in
  let one_write = self.kind = Write || other.kind = Write in
  let intersects = Int_Intervals.intersects offset1 offset2 in

  Racer.debug "Checking race:\n  %s\n   %s\n"
    (Callstack.show ~event:(MemoryAccess.show_kind self.kind) self.callstack)
    (Callstack.show ~event:(MemoryAccess.show_kind other.kind) other.callstack);
  Racer.debug ">  Lockset: %s x %s -> %b" (Lock.Set.show ls1) (Lock.Set.show ls2)
                                          (Lock.Set.rw_disjoint ls1 ls2);

  let res = different && disjoint && parallel && one_write && intersects in
  Racer.debug ">  different threads: %b\n" different;
  Racer.debug ">  disjoint locksets: %b\n" disjoint;
  Racer.debug ">  may-run-in-parallel: %b\n" parallel;
  Racer.debug ">  one is write: %b\n" one_write;
  Racer.debug ">  addresses intersects: %b\n" intersects;
  Racer.debug ">>> %b\n" res;
  res

let is_local_malloc base =
  try
    let var = Option.get @@ CFG_utils.find_allocation_target base in
    Cil.isPointerType var.vtype && not @@ var.vglob
  with _ -> false


let is_unique_malloc access threads base = match base with
  | Base.Allocated _ ->
    let thread = MemoryAccess.get_thread access in
    ThreadAnalysis.Result.is_unique threads thread
  | _ -> true


let is_alloced_array base = match base with
  | Base.Allocated (var, _, validity) -> Cil.isArrayType var.vtype
  | _ -> false

let is_may_race threads parallel self other =
  is_race_base threads parallel Lock.PowerSet.flatten_inter self other

let is_must_race threads parallel self other =
  let base = MemoryAccess.get_base self in
  let is_precise = MemoryAccess.is_precise self in
  let is_allocated_array = is_alloced_array base in
  let are_parallel =
    ParallelAnalysis.Result.must_run_in_parallel parallel self.callstack other.callstack
  in
  Racer.debug ">  surely parallel: %b\n" are_parallel;
  Racer.debug ">  precise access: %b\n" is_precise;
  Racer.debug ">  is allocted array: %b\n" is_allocated_array;

  is_race_base threads parallel Lock.PowerSet.flatten_union self other
  && is_precise
  && not (Base.is_weak base)
  && are_parallel
  && is_unique_malloc self threads base

let of_accesses kind access1 access2 =
  let base = MemoryAccess.get_base access1 in
  let offset1 = MemoryAccess.get_offset access1 in
  let offset2 = MemoryAccess.get_offset access2 in
  let inter = Int_Intervals.narrow offset1 offset2 in (* TODO: use meet in must mode *)
  {kind = kind; base = base; offset = inter; accesses = (access1, access2)}

let check threads parallel access1 access2 =
  if is_must_race threads parallel access1 access2 then
    Some (of_accesses Must access1 access2)
  else if is_may_race threads parallel access1 access2 then
    Some (of_accesses May access1 access2)
  else None

let is_write_write {accesses = (a1, a2); _} =
  a1.kind = Write && a2.kind = Write

let show_kind = function
  | Must -> "must"
  | May -> "may"

let show race =
  if Int_Intervals.is_top race.offset then
    Format.asprintf "Data race (%s) on %a (weak: %b)"
      (show_kind race.kind)
      Base.pretty race.base
      (Base.is_weak race.base)
  else
    Format.asprintf "Data race (%s) on %a (weak: %b) at offset %a"
      (show_kind race.kind)
      Base.pretty race.base
      (Base.is_weak race.base)
      Int_Intervals.pretty race.offset

let report race =
  let a1, a2 = race.accesses in
  Format.asprintf "%s:\n    %s\n\n    %s"
    (show race)
    (Callstack.show ~event:(MemoryAccess.show_kind a1.kind) a1.callstack)
    (Callstack.show ~event:(MemoryAccess.show_kind a2.kind) a2.callstack)
