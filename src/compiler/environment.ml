(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;

open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t * int;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty, 0);;

(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let allocate_named_variable (name : string) (env : environment) : environment =
  let (offset, dictionary, params) = env in
  (offset - 8, (Map.String.add name (ArgMemory(AddrByRegisterOffset(RBP,offset))) dictionary), params)
;;

let allocate_closure (name: string) (env : environment) : environment = 
  let (offset, dictionary, params) = env in
  (offset - 8, (Map.String.add (name) (ArgLabelOffset("closure_of_"^name, 1)) dictionary), params)
;;

let allocate_parameters (param_list : string list) (env : environment): environment =
  (* Arguments passed onto stack in reverse order (i.e. last argument is 
     the first one on the stack and first argument is the last one on the
     stack) *)
  let rec loop (param_list : string list) 
      (dictionary : argument Map.String.t) 
      (param_offset : int) : argument Map.String.t =
    match param_list with 
    | [] -> dictionary
    | param::rest -> 
      let new_dict = loop (rest) (dictionary) (param_offset + 8)in  
      Map.String.add param (ArgMemory(AddrByRegisterOffset(RBP,param_offset))) new_dict
  in 
  let (offset, dict, _) = env in 
  (offset, loop param_list dict 16, List.length param_list)
;;

(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
  let (_, dictionary, _) = env in
  try
    (Map.String.find name dictionary)
  with
    Not_found -> 
    (* let () = print_endline ("err: " ^ (string_of_environment env)) in *)
    raise (UnboundVariable (name, env))
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
  let (offset, dictionary, params) = env in
  (ArgMemory(AddrByRegisterOffset(RBP, offset)), (offset-8, dictionary, params))
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict, _) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;
