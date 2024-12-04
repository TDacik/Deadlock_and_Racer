(* Functions over conccurency model.
 *
 * TODO: handle theoretical situation when one of modeled functions is called via
 *       a function pointer and thus get wrongly classified.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2021 *)

open Cil_datatype

open ConcurrencyModel_data
open ConcurrencyModel_types

type call_type =
  | Direct of Kernel_function.t (* Function defined in the source code *)
  | Extern of Varinfo.t         (* Extern function without definition *)
  | Pointer of Exp.t    (* Call via a function pointer *)

type stmt =
  | Lock of Exp.t * LockKind.t * Lval.t option
  | Unlock of Exp.t
  | Lock_init of Exp.t
  | Lock_destroy of Exp.t
  | Condition_init of Exp.t
  | Condition_wait of Exp.t * Exp.t
  | Condition_signal of Exp.t
  | Thread_create of Exp.t * Exp.t * Exp.t
  | Thread_join of Exp.t

  | Atomic_seq_start
  | Atomic_seq_end
  | Atomic_call of Exp.t * Exp.t list

  | Call of call_type * Lval.t option * Exp.t * Exp.t list

  | Return
  | Exit
  | Other

let classify_call ?lval fn args =
  let fn_name = Format.asprintf "%a" Printer.pp_exp fn in

  if fn_name = "exit" then Exit

  else if Lock_functions.mem fn_name then
    let pos, kind = Lock_functions.find fn_name in
    Lock (List.nth args pos, kind, lval)

  else if Unlock_functions.mem fn_name then
    Unlock (List.nth args @@ Unlock_functions.find fn_name)

  else if Lock_init_functions.mem fn_name then
    Lock_init (List.nth args @@ Lock_init_functions.find fn_name)

  else if Lock_destroy_functions.mem fn_name then
    Lock_destroy (List.nth args @@ Lock_destroy_functions.find fn_name)

  else if Condition_init_functions.mem fn_name then
    Condition_init (List.nth args @@ Condition_init_functions.find fn_name)

  else if Condition_wait_functions.mem fn_name then
    let cond_pos, lock_pos = Condition_wait_functions.find fn_name in
    let cond = List.nth args cond_pos in
    let lock = List.nth args lock_pos in
    Condition_wait (cond, lock)

  else if Condition_signal_functions.mem fn_name then
    Condition_signal (List.nth args @@ Condition_signal_functions.find fn_name)

  else if Thread_create_functions.mem fn_name then
    let (id_pos, thread_pos, arg_pos) = Thread_create_functions.find fn_name in
    let id = List.nth args id_pos in
    let thread = List.nth args thread_pos in
    let arg = List.nth args arg_pos in
    Thread_create (id, thread, arg)

  else if Thread_join_functions.mem fn_name then
    Thread_join (List.nth args @@ Thread_join_functions.find fn_name)

  else if AtomicSequenceStart.mem fn_name then
    Atomic_seq_start

  else if AtomicSequenceEnd.mem fn_name then
    Atomic_seq_end

  (** Needs to be after sequences! *)
  else if AtomicFunctions.exists (fun a -> Str.string_match (Str.regexp a) fn_name 0) then
    Atomic_call (fn, args)

  else
    let call_type = match fn.enode with
      | Lval lval -> begin match fst lval with
        | Var var -> begin
            try let kf = Globals.Functions.get var in Direct kf
            with Not_found -> Extern var
          end
        | Mem expr -> Pointer (expr)
      end
    | _ -> assert false
    in
    Call (call_type, lval, fn, args)

open Cil_types

let classify_instr instr =
  match instr with
  | Call (Some lval, fn, args, _) -> classify_call ~lval fn args
  | Call (None, fn, args, _) -> classify_call fn args
  | Local_init (_, init, _) ->
    begin match init with
    | AssignInit _ -> Other
    | ConsInit (varinfo, args, _) ->
      let fn = Cil.evar varinfo in
      classify_call fn args
    end
  | _ -> Other

let classify_stmt stmt = match stmt.skind with
  | Instr instr -> classify_instr instr
  | Return _ -> Return
  | _ -> Other

let get_thread_arg stmt = match classify_stmt stmt with
  | Thread_create (_, _, arg) -> arg

let is_atomic_fn var =
  AtomicFunctions.exists (fun a -> Str.string_match (Str.regexp a) var.vname 0)
