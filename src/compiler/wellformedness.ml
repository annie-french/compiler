open Batteries;;
open HatchLanguage;;

open Printf;;

open Asts;;

exception IllFormed of string list;;


(* takes a function call and checks if it is defined and has correct number of arguments 
   Not needed for Falcon *)

(* let rec check_function_appl (name : string) (decl_list : declaration list) : string list =
  match decl_list with 
  | [] -> ["Function "^name^" is not defined."]
  | decl :: rest -> 
    match decl with 
    | DFunction(fn_name,_,_) -> 
      if fn_name = name then
        []
      else 
        check_function_appl name rest
;; *)

let rec check_unbound_variable (bound_vars : string list) (e : expr) : string list =
  match e with
  | ELet (x, e1, e2) ->
    let new_list = bound_vars @ [x] in
    (check_unbound_variable new_list e1) @ (check_unbound_variable new_list e2)
  | EVar x -> 
    if List.mem x bound_vars then
      []
    else
      ["Unbound variable "^x]
  | EBinaryOp (_,e1,e2) -> 
    check_unbound_variable bound_vars e1 @ check_unbound_variable bound_vars e2
  | EUnaryOp(_,e) -> 
    check_unbound_variable bound_vars e
  | EIf (e1,e2,e3) ->
    check_unbound_variable bound_vars e1 @ 
    check_unbound_variable bound_vars e2 @
    check_unbound_variable bound_vars e3
  |ETuple (e_list) -> List.concat (List.map (check_unbound_variable bound_vars) e_list)
  | EAppl (e1, e2, _) -> 
    check_unbound_variable bound_vars e1 @ check_unbound_variable bound_vars e2
  |ESet (e1, e2,e3) ->
    check_unbound_variable bound_vars e1 @ 
    check_unbound_variable bound_vars e2 @
    check_unbound_variable bound_vars e3
  | _ -> []
;;

(* Loop that returns an empty list if there is no duplicate of x.
   Otherwise it returns a list of the element that appears more than once *)
let rec check_duplicate (x : 'a)(lst : 'a list) : 'a list = 
  match lst with
  | [] -> []
  | first :: rest -> 
    if x = first then
      [x]
    else
      check_duplicate x rest
;;

(* Checks declarations for unique parameter names *)
let rec check_parameter_duplication 
    (param_lst : string list)(fn_name : string) : string list =
  match param_lst with
  | [] -> []
  | parameter :: rest ->
    let check = check_duplicate parameter rest in
    (* check should only ever return an empty list or a list with one element *)
    match check with
    | [] -> check_parameter_duplication rest fn_name
    |duplicate :: _ ->  
      ["Function "^fn_name^" declares a duplicate parameter "^duplicate]
;;

let show_declaration_name (d : declaration) : string =
  match d with
  | DFunction (name,_,_) -> name
;;

(* Checks for duplicate funciton declarations *)
let rec check_declaration_duplication (d_lst : declaration list) : string list =
  match d_lst with
  | [] -> []
  | DFunction(name,_,_) :: rest -> 
    let fn_names = List.map show_declaration_name rest in
    let check = check_duplicate name fn_names in
    if List.is_empty check then
      check_declaration_duplication rest
    else
      ["Duplicate definition of function "^name] @ check_declaration_duplication rest
;;

(* two parameters the "global declaration" list and the declaration list 
   that we iterate through *)
let check_declaration_lst_for_errors (d_lst : declaration list) : string list =
  let rec loop (d_lst' : declaration list) : string list =
    match d_lst' with
    | [] -> []
    | DFunction(name, param_lst, body)::rest ->
      check_parameter_duplication param_lst name @
      check_unbound_variable ((List.map show_declaration_name d_lst) @ param_lst) body @
      loop rest
  in
  check_declaration_duplication d_lst @ loop d_lst

;;

(* This function produces a list of compile-time errors found in a program. *)
let check_program_for_errors (p : program) : string list =
  match p with
  |Program(d_lst, e) -> 
    check_declaration_lst_for_errors d_lst @
    check_unbound_variable (List.map show_declaration_name d_lst) e 
;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : unit =
  let errors = List.unique (check_program_for_errors p) in
  if List.is_empty errors then () else raise (IllFormed errors);
;;
