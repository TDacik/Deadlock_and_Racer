(* Utilities for pretty-printing.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

open Cil_types
open Cil_datatype
open Filepath

let get_time () = Format.asprintf "(%.2f)" (Sys.time ())

(* True if base represents variable from original source code *)
let is_orig base =
  try
    let varinfo = Base.to_varinfo base in
    not (Cil.hasAttribute "fc_stdlib" varinfo.vattr
         || Cil.hasAttribute "fc_stdlib_generated" varinfo.vattr)
  with Base.Not_a_C_variable -> false

let stmt_line stmt =
  let loc = Stmt.loc stmt in
  (fst loc).pos_lnum

let pretty_stmt_loc fmt stmt =
  let stmt_short = match stmt.skind with
    | If _ -> "if"
    | Switch _ -> "switch"
    | Loop _ -> "loop"
    | Block _ -> "block"
    | UnspecifiedSequence _ -> "unspec-seq"
    | _ -> Format.asprintf "%a" Stmt.pretty stmt
  in
  Format.fprintf fmt "%d:%s" (stmt_line stmt) stmt_short

let aux_file_line stmt =
  Stmt.loc stmt
  |> Format.asprintf "%a" Printer.pp_location
  |> String.split_on_char '/'
  |> BatList.last

let pretty_stmt_short fmt stmt =
  Format.fprintf fmt "%s" (aux_file_line stmt)

let descr_stmt stmt =
  let loc = Stmt.loc stmt in
  let path = (fst loc).pos_path in
  (Format.asprintf "%a" Filepath.Normalized.pretty path, stmt_line stmt)


module Make(Plugin : Plugin.General_services) = struct

  let color code = if Plugin.has_tty () then code else ""

  let blue ()    = color "\x1b[34m"
  let magenta () = color "\x1b[35m"
  let cyan ()    = color "\x1b[36m"
  let white ()   = color "\x1b[0m"

  (* Print statement as a location in the source code *)
  let pretty_stmt fmt stmt =
    Format.fprintf fmt "%s:%s%a%s"
      (aux_file_line stmt)
      (cyan ())
      pretty_stmt_loc stmt
      (white ())

  let pp_globals fmt state =
    let filtered = Cvalue.Model.filter_base is_orig state in
      Format.fprintf fmt "%s%a%s"
        (cyan ())
        Cvalue.Model.pretty filtered
        (white ())

  module Logger(L : sig val dkey : string end) = struct

    let c = ref None

    let () =
      let category = Plugin.register_category L.dkey in
      c := Some category;
      Plugin.add_debug_keys category

    let feedback ?(level=1) =
      Plugin.feedback ~level ~dkey:(Option.get !c)

    let debug ?(level=1) =
      Plugin.debug ~level ~dkey:(Option.get !c)

  end

end
