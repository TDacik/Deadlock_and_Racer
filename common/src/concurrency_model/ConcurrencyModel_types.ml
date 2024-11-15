(* Specification of lock properties.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

module LockKind = struct

  type t = {
    blocking : bool;
    read_lock : bool;
    reentrant : bool;
  }

  let show self =
    Format.asprintf "blocking: %b, read: %b, reentrant: %b"
      self.blocking self.read_lock self.reentrant

end
