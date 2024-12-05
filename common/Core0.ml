module Self = Plugin.Register
  (struct
    let name = "Concurrency core"
    let shortname = "cc"
    let help = "..."
  end)

include Self
include Print_utils.Make(Self)

module ConcurrencyModels = Self.Filepath_list
  (struct
    let option_name = "-cc-models"
    let help = "TODO (default: all available)"
    let arg_name = "model list"
    let existence = Frama_c_kernel.Filepath.Must_exist
    let file_kind = ".yaml model"
  end)

module JsonOutput = Self.Filepath
  (struct
    let option_name = "-cc-json-output"
    let help = ""
    let arg_name = ""
    let existence = Frama_c_kernel.Filepath.Must_not_exist
    let file_kind = "TODO"
  end)

module JustThreadAnalysis = Self.False
  (struct
    let option_name = "-cc-thread-analysis"
    let help = "Run thread analysis"
  end)

module ThreadApproximation = Self.Enum
  (struct
    let option_name = "-cc-thread-approx"
    let arg_name = "under|over"
    let help = "TODO"

    type t = [`Under | `Over]
    let default = `Under
    let all_values = [`Under; `Over]
    let to_string = function
      | `Under -> "under"
      | `Over -> "over"
   end)

module ValueAnalysisType = Self.Enum
  (struct
    let option_name = "-cc-value-analysis"
    let arg_name = "syntactic|alias|eva"
    let help = "TODO"

    type t = [`Syntactic | `Alias | `Eva]
    let default = `Eva
    let all_values = [`Syntactic; `Alias; `Eva]
    let to_string = function
      | `Syntactic -> "syntactic"
      | `Alias -> "alias"
      | `Eva -> "eva"
   end)

module NonDeterminization = Self.False
  (struct
    let option_name = "-cc-nondet"
    let help = "Make all branching nedeterministic"
  end)

module OverApproxFunctionPointers = Self.False
  (struct
    let option_name = "-cc-over-approx-fn-pointers"
    let help = "Over-approximate function pointer calls with all referenced functions"
  end)

module ContextDepth = Self.Int
  (struct
    let option_name = "-cc-context-depth"
    let arg_name = "depth"
    let help = "TODO"
    let default = 1
  end)
