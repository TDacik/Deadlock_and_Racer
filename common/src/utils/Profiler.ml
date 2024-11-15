(* Utility for profiling.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open Unix

let self : (string * float * float) list ref = ref []

let start = ref Float.zero

let total = ref Float.minus_one

let reset () =
  self := [];
  start := (Unix.times ()).tms_utime;
  total := Float.zero

let add name =
  let times = Unix.times () in
  self := ((name, times.tms_utime, times.tms_cutime) :: !self)

let finish () = total := (Unix.times ()).tms_utime

let compute_stats () =
  let _, _, stats = List.fold_left
    (fun (prev1, prev2, acc) (name, t1, t2) ->
      (t1, t2, (name, t1, t1 -. prev1, t2, t2 -. prev2) :: acc)
    ) (0.0, 0.0, []) (List.rev !self)
  in
  List.rev stats

let total_time () = !total -. !start

let json_repr () =
  let stats = compute_stats () in
  `Assoc [
    "Total time", `Float (!total -. !start);
    "Phases",     `Assoc (List.map (fun (name, _, _, self, childs) -> (name, `List [`Float self; `Float childs])) stats)
  ]

let report () =
  let stats = compute_stats () in
  Format.printf "[Profiler]\n\n";
  Format.print_flush ();
  Format.open_tbox ();
  Format.printf "  ";
  Format.set_tab (); Format.printf "Phase                ";
  Format.set_tab (); Format.printf "Total                ";
  Format.set_tab (); Format.printf "Diff                 ";
  Format.set_tab (); Format.printf "Total (childs)  ";
  Format.set_tab (); Format.printf "Diff (childs)";
  let delim = String.init 92 (fun _ -> '-') in
  Format.printf "\n  %s" delim;

  List.iter
    (fun (name, time, diff, subprocess_time, subprocess_diff) ->
      Format.print_tbreak 0 0;
      Format.printf "%s" name;
      Format.print_tbreak 0 0;
      Format.printf "%f" time;
      Format.print_tbreak 0 0;
      Format.printf "%f" diff;
      Format.print_tbreak 0 0;
      Format.printf "%f" subprocess_time;
      Format.print_tbreak 0 0;
      Format.printf "%f" subprocess_diff;
    ) stats;

  Format.close_tbox ();
  Format.printf "\n\n"
