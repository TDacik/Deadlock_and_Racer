(* Utilities for witness generation.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

type metadata = {
  producer : string;
  version : string;
  spec : string;
  language : string;
  files : (string * string) list; (* File, hash *)
  time: string;
}

(** For SV-COMP, we use a wrapper with preprocessing. The preprocessor stores the resulting
    file in the same directory, but with this prefix. The prefix needs to be removed in
    witness. *)
let process_name str =
  let prefix = "__preprocessed_" in
  let basename = BatList.last @@ BatString.split_on_char '/' str in
  if String.starts_with ~prefix basename then
    let basename' = BatString.lchop basename ~n:(String.length prefix) in
    snd @@ BatString.replace ~str ~sub:basename ~by:basename'
  else str

let get_files () =
  let prefix = "__pre_" in
  Ast.UntypedFiles.get ()
  |> List.map (fun (path, _) -> Format.asprintf "%a" Filepath.Normalized.pretty path)
  |> List.filter (fun s -> not @@ String.ends_with s ~suffix:".h")
  |> List.map process_name

let get_file_hashes () =
  get_files ()
  |> List.map (fun path -> (path, Sha256.to_hex @@ Sha256.string path))

let now () =
  let t = Unix.localtime @@ Unix.time () in
  Format.sprintf "%04u-%02u-%02uT%02u:%02u:%02uZ"
    (1900 + t.tm_year) (1 + t.tm_mon) t.tm_mday t.tm_hour t.tm_min t.tm_sec

let metadata () = {
  producer = "RacerF";
  version = "2.0";
  spec = "CHECK( init(main()), LTL(G ! data-race) )";
  language = "C";
  files = get_file_hashes ();
  time = now ();
}
