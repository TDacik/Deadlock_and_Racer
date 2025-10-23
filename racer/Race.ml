(* Representation of data races.
 *
 * TODO: clean up race checking
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open MemoryAccess

type race_kind = Must | May of string

type t = {
  kind : race_kind;
  base : Base.t;
  offset : Int_Intervals.t;
  accesses : MemoryAccess.t * MemoryAccess.t;
}

(** Check whether a pair of accesses is a race *)
let is_may_race threads parallel self other =
  let open ThreadAnalysis in
  let ls1 = Lock.PowerSet.flatten_inter self.locksets in
  let ls2 = Lock.PowerSet.flatten_inter other.locksets in
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

let of_accesses kind access1 access2 =
  let base = MemoryAccess.get_base access1 in
  let offset1 = MemoryAccess.get_offset access1 in
  let offset2 = MemoryAccess.get_offset access2 in
  let inter = Int_Intervals.narrow offset1 offset2 in (* TODO: use meet in must mode *)
  {kind = kind; base = base; offset = inter; accesses = (access1, access2)}

let check_must_race threads parallel self other =
  let base = MemoryAccess.get_base self in
  let is_precise = MemoryAccess.is_precise self in
  let is_allocated_array = is_alloced_array base in
  let are_parallel =
    ParallelAnalysis.Result.must_run_in_parallel parallel self.callstack other.callstack
  in
  let locksets_disjoint = 
    Lock.Set.rw_disjoint (Lock.PowerSet.flatten_union self.locksets) (Lock.PowerSet.flatten_union other.locksets) 
  in
  let not_weak = not @@ Base.is_weak base in
  let unique_malloc = is_unique_malloc self threads base in

  Racer.debug ">  surely parallel: %b\n" are_parallel;
  Racer.debug ">  precise access: %b\n" is_precise;
  Racer.debug ">  is allocted array: %b\n" is_allocated_array;

  if (is_precise && not_weak && are_parallel && unique_malloc && locksets_disjoint) then
    Option.some @@ of_accesses Must self other
  else
    let reason1 = if is_precise then None else Some "not precise" in
    let reason2 = if not_weak then None else Some "weak" in
    let reason3 = if are_parallel then None else Some "not parallel" in
    let reason4 = if unique_malloc then None else Some "not unique malloc" in
    let reason5 = if locksets_disjoint then None else Some "locksets are not must-disjoint" in
    let reason = String.concat " & " @@ List.filter_map Fun.id [reason1; reason2; reason3; reason4; reason5] in
    Option.some @@ of_accesses (May reason) self other

let check threads parallel a1 a2 =
  if not @@ is_may_race threads parallel a1 a2 then None
  else check_must_race threads parallel a1 a2

let is_write_write {accesses = (a1, a2); _} =
  a1.kind = Write && a2.kind = Write

let show_kind = function
  | Must -> "must"
  | May _ -> "may"

let show_reason race = match race.kind with
  | Must -> ""
  | May reason -> "Reason: " ^ reason

let show race =
  if Int_Intervals.is_top race.offset then
    Format.asprintf "Data race (%s) on %a\n%s"
      (show_kind race.kind)
      Base.pretty race.base
      (show_reason race)
  else
    Format.asprintf "Data race (%s) on %a at offset %a\n%s"
      (show_kind race.kind)
      Base.pretty race.base
      Int_Intervals.pretty race.offset
      (show_reason race)

let report race =
  let a1, a2 = race.accesses in
  Format.asprintf "%s:\n    %s\n\n    %s"
    (show race)
    (Callstack.show ~event:(MemoryAccess.show_kind a1.kind) a1.callstack)
    (Callstack.show ~event:(MemoryAccess.show_kind a2.kind) a2.callstack)
