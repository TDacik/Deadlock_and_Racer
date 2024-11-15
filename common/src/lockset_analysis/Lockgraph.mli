(* Lockgraph.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

include Graph.Sig.P
  with type V.t = Lock.t
   and type E.t = Lock.t * Trace.t list * Lock.t

include Datatype_sig.PRINTABLE with type t := t

val get_traces : E.t -> Trace.t list

val update_on_lock : t -> Lock.Set.t -> Lock.t list -> Callstack.t -> t
