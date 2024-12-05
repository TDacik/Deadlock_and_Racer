open Core0

let () = ContextDepth.add_set_hook (fun _ n -> RelaxedCallstack.depth := n)
