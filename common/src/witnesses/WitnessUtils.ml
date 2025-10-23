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

let get_files () =
  let orig_files = Core0.OriginalSources.get () in
  let files =
    if List.is_empty orig_files then List.map fst @@ Ast.UntypedFiles.get ()
    else orig_files
  in
  files
  |> List.map (fun path -> Format.asprintf "%a" Filepath.Normalized.pretty path)
  |> List.filter (fun s -> not @@ String.ends_with s ~suffix:".h")

let get_file_hashes () =
  get_files ()
  |> List.map (fun path -> path, In_channel.with_open_text path In_channel.input_all)
  |> List.map (fun (path, content) -> (path, Sha256.to_hex @@ Sha256.string content))

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
