(* Representation of a thread - entry point & initial state
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2020 *)

module InitialState = struct

  type t = Cvalue.Model.t * Cvalue.V.t

  let is_global base =
    try
      let varinfo = Base.to_varinfo base in
      Base.is_global base &&
      not (Cil.hasAttribute "fc_stdlib" varinfo.vattr
           || Cil.hasAttribute "fc_stdlib_generated" varinfo.vattr)
      with Base.Not_a_C_variable -> false

  (* Comparison module generated bases. TODO: why is this needed for fixpoint termination? *)
  let compare (glob1, arg1) (glob2, arg2) =
    let glob1' = Cvalue.Model.filter_base is_global glob1 in
    let glob2' = Cvalue.Model.filter_base is_global glob2 in
    let aux = Cvalue.V.compare arg1 arg2 in
    if aux != 0 then aux
    else Cvalue.Model.compare glob1' glob2'

  let equal s1 s2 = compare s1 s2 = 0

  let mk globals arg = (globals, arg)

  let get (globals, arg) = (globals, arg)

  let show (globals, arg) =
    let globals = Cvalue.Model.filter_base is_global globals in
    Format.asprintf "\t\tGlobals: %s: %a%s \t\tArg: %s%a%s"
      (Core0.magenta ())
      Cvalue.Model.pretty globals
      (Core0.white ())
      (Core0.magenta ())
      Cvalue.V.pretty arg
      (Core0.white ())

  let bottom = (Cvalue.Model.bottom, Cvalue.V.bottom)
  let top = (Cvalue.Model.top, Cvalue.V.top)

  let join (globals1, arg1) (globals2, arg2) =
    (Cvalue.Model.join globals1 globals2, Cvalue.V.join arg1 arg2)

  let widen (globals1, arg1) (globals2, arg2) =
    (Cvalue.Model.widen globals1 globals2, Cvalue.V.widen arg1 arg2)

end

type t = {
  entry_point : Kernel_function.t;
  stmt : Cil_datatype.Stmt.t option;
  is_main : bool;
}

let show t = match t.stmt with
  | None -> Format.asprintf "%a" Kernel_function.pretty t.entry_point
  | Some s -> Format.asprintf "%a:%d" Kernel_function.pretty t.entry_point (Print_utils.stmt_line s)

let compare t1 t2 =
  let aux = Kernel_function.compare t1.entry_point t2.entry_point in
  if aux <> 0 then aux
  else Option.compare Cil_datatype.Stmt.compare t1.stmt t2.stmt

module Self = struct
  type nonrec t = t
  let compare = compare
  let show = show
end

include Datatype.Printable(Self)
include Datatype.Comparable(Self)
include Datatype.Collections(Self)

module Powerset = Powerset.Make(Set)

let mk ?(is_main=false) ?stmt entry_point = {
  entry_point = entry_point;
  stmt = if Core0.ThreadSensitive.get () then stmt else None;
  is_main = is_main;
}

let get_name t = Format.asprintf "%a" Kernel_function.pretty t.entry_point

let get_entry_point t = t.entry_point

let is_main t = t.is_main

let hash t = Kernel_function.hash t.entry_point
