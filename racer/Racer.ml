(* RacerF : command-line options and logging utilities.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let version = "2.2"

module Self = Plugin.Register
  (struct
    let name = "RacerF"
    let shortname = "racer"
    let help = "Data race detector"
  end)

include Self
include Print_utils.Make(Self)


module Enabled = False
  (struct
    let option_name = "-racer"
    let help = "Run data race analysis"
  end)

module Version = False
  (struct
    let option_name = "-racer-version"
    let help = "Show version and exit"
  end)

(*
module Mode = Enum
  (struct
    let option_name = "-racer-mode"
    let arg_name = "must | may"
    let help = "Report (must/may) races"

    type t = [`Must | `May]
    let default = `May
    let all_values = [`Must; `May]
    let to_string = function `Must -> "must" | `May -> "may"
   end)
*)

module ExternalFunctions = False
  (struct
    let option_name = "-racer-external-fns"
    let help = "Report races caused by external functions"
  end)

module JsonOutput = Self.Filepath
  (struct
    let option_name = "-racer-json-output"
    let help = "Output summary of results in json"
    let arg_name = "path"
    let existence = Frama_c_kernel.Filepath.Indifferent
    let file_kind = "json"
  end)

(** {2 SV-comp} *)

module SVWitnessPath = Self.Filepath
  (struct
    let option_name = "-racer-witness-path"
    let help = "Output software verification witness in format 1.0"
    let arg_name = "path"
    let existence = Frama_c_kernel.Filepath.Indifferent
    let file_kind = "graphml"
  end)
