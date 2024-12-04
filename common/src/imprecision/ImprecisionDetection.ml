open Imprecision

let is_called name =
  try
    let kf = Globals.Functions.find_by_name name in
    not @@ List.is_empty @@ Kernel_function.find_syntactic_callsites kf
  with Not_found -> false

let check_malloc () =
  if is_called "malloc" then Imprecision.add_backend Malloc
