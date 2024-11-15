(* Representation of memory accesses.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

type access_kind = Read | Write [@@deriving compare, equal]

type t = {
  address : MemoryAddress.t;
  kind : access_kind;
  locksets: Lock.PowerSet.t;
  callstack : RelaxedCallstack.t;
} [@@ deriving compare, equal]

let get_base access = MemoryAddress.base access.address
let get_offset access = MemoryAddress.offset access.address
let get_thread access = Callstack.get_thread access.callstack
let get_stmt access = Callstack.get_event access.callstack

let show_kind = function Read -> "read" | Write -> "write"

let show self = Format.asprintf "thread %s %s %s at %s under %s"
  (Thread.show @@ get_thread self)
  (match self.kind with Write -> "writes" | Read -> "reads")
  (MemoryAddress.show self.address)
  (RelaxedCallstack.show_short self.callstack)
  (Lock.PowerSet.show self.locksets)

module Self = struct
  type nonrec t = t
  let show = show
  let compare = compare
end

include Datatype.Collections(Self)

let is_precise access =
  let offset = get_offset access in
  match Int_Intervals.project_singleton offset with
    | Some _ -> true
    | None -> false

let mk kind stmt address locksets callstack =
  let callstack = Callstack.push_event stmt callstack in
  {address = address; kind = kind; locksets = locksets; callstack = callstack}

let mk_read stmt address locksets callstack =
  mk Read stmt address locksets callstack

let mk_write stmt address locksets callstack =
  mk Write stmt address locksets callstack
