(* Representation of memory accesses.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

type access_kind = Read | Write [@@deriving compare, equal]

type t = {
  address : MemoryAddress.t;
  kind : access_kind;
  locksets: Lock.PowerSet.t;
  callstack : RelaxedCallstack.t;
  precise : Bool.t;
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
  if not access.precise then false
  else
  let base = get_base access in
  let offset = get_offset access in
  match Int_Intervals.project_singleton offset with
    | Some (x, y) ->
      begin match Base.typeof base with
        | Some typ when Cil.isArrayType typ ->
          let size = Integer.of_int @@ Cil.bitsSizeOf @@ Cil.typeOf_array_elem typ in
          let diff = Integer.sub y x in
          Racer.debug "Base %a : size: %s, diff: %s" Base.pretty base (Integer.to_string size) (Integer.to_string diff);
          Integer.ge size diff && access.precise
        | _ -> true
      end
    | None -> false

let mk kind stmt address locksets callstack precise =
  let callstack = Callstack.push_event stmt callstack in
  {address = address; kind = kind; locksets = locksets; callstack = callstack; precise = precise}

let mk_read stmt address locksets callstack precise =
  mk Read stmt address locksets callstack precise

let mk_write stmt address locksets callstack precise =
  mk Write stmt address locksets callstack precise
