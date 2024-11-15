(* Imperative representation of Deadlock's concurrency model.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open ConcurrencyModel_types

module type KEY = sig
  type t
  include Map.OrderedType with type t := t
  val show : t -> string
end

module type DATA = sig
  type t
  val show : t -> string
end

(** Imperative set with minimal functionality needed for the model *)
module Imperative_set (Key : KEY) = struct

  module S = Set.Make(Key)

  let self = ref S.empty

  let add x = self := S.add x !self
  let mem x = S.mem x !self
  let iter fn = S.iter fn !self
  let exists pred = S.exists pred !self
  let cardinal () = S.cardinal !self

  let add_list xs = List.iter add xs

  let print prefix =
    S.elements !self
    |> List.map Key.show
    |> String.concat ", "
    |> (fun s -> prefix ^ ": {" ^ s ^ "}")
    |> Format.printf "%s\n"

end

(** Imperative map with minimal functionality needed for the model *)
module Imperative_map (Key : KEY) (Data : DATA) = struct

  module M = Map.Make(Key)

  let self = ref M.empty

  let add (key, (x : Data.t)) = self := M.add key x !self
  let mem key = M.mem key !self
  let find key = M.find key !self
  let iter fn = M.iter fn !self
  let cardinal () = M.cardinal !self

  let add_list xs = List.iter add xs

  let print prefix =
    M.bindings !self
    |> List.map (fun (k, d) -> Format.asprintf "%s: %s" (Key.show k) (Data.show d))
    |> String.concat ",\n  "
    |> (fun s -> Format.asprintf "%s: {\n  %s\n}" prefix s)
    |> Format.printf "%s\n"

end

module String = struct
  include String
  let show = Fun.id
end

module Int = struct
  type t = int
  let show x = Format.asprintf "[%d]" x
end

module Int2 = struct
  type t = int * int
  let show (x, y) = Format.asprintf "[%d, %d]" x y
end
module Int3 = struct
  type t = int * int * int
  let show (x, y, z) = Format.asprintf "[%d, %d, %d]" x y z
end
module Lock = struct
  type t = int * LockKind.t
  let show (i, kind) = Format.asprintf "[%d] (%s)" i (LockKind.show kind)
end

(* Locks *)

module Lock_types                 = Imperative_set(String)
module Lock_functions             = Imperative_map(String)(Lock)
module Unlock_functions           = Imperative_map(String)(Int)
module Lock_init_functions        = Imperative_map(String)(Int)
module Lock_destroy_functions     = Imperative_map(String)(Int)

(* Conditions *)

module Condition_types            = Imperative_set(String)
module Condition_init_functions   = Imperative_map(String)(Int)
module Condition_wait_functions   = Imperative_map(String)(Int2)
module Condition_signal_functions = Imperative_map(String)(Int)

(* Threads *)

module Thread_types            = Imperative_set(String)
module Thread_create_functions = Imperative_map(String)(Int3)
module Thread_join_functions   = Imperative_map(String)(Int)

(* Atomic *)

module AtomicFunctions     = Imperative_set(String)
module AtomicSequenceStart = Imperative_set(String)
module AtomicSequenceEnd   = Imperative_set(String)

let print () =
  Lock_types.print "Lock types";
  Lock_init_functions.print "Lock init functions";
  Lock_destroy_functions.print "Lock destroy functions";
  Lock_functions.print "Lock functions";
  Unlock_functions.print "Unlock functions";
  Format.printf "\n";

  Thread_types.print "Thread types";
  Thread_create_functions.print "Thread create functions";
  Thread_join_functions.print "Thread join functions";
  Format.printf "\n";

  Condition_types.print "Condition types";
  Condition_wait_functions.print "Condition wait functions";
  Format.printf "\n";

  AtomicFunctions.print "Atomic function prefixes";
  AtomicSequenceStart.print "Atomic sequence start";
  AtomicSequenceEnd.print "Atomic sequence end"
