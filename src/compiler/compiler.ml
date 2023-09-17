(** This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;
open Wellformedness;;

let string_of_twice_int (n : int) : string =
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2));;

let hex_of_bool (boolean : bool) : string = 
  if boolean = true then
    "0xFFFFFFFFFFFFFFFF"
  else
    "0x7FFFFFFFFFFFFFFF"
;;

let compare_intstructions (instr : instruction) : instruction list =
  let true_lbl = fresh_name "true" in
  let end_lbl = fresh_name "end" in
  let jump = match instr with
    |AsmJe(_) -> AsmJe(true_lbl)
    |AsmJg(_) -> AsmJg(true_lbl)
    |AsmJl(_) -> AsmJl(true_lbl)
    | _ -> failwith "Not a valid comparison"
  in
  [AsmCmp(ArgRegister(RAX), ArgRegister(R10)); 
   jump; 
   AsmMov(ArgRegister(RAX), ArgConstant(hex_of_bool false));
   AsmJmp(end_lbl);
   AsmLabel(true_lbl);
   AsmMov(ArgRegister(RAX), ArgConstant(hex_of_bool true));
   AsmLabel(end_lbl);]
;;

let stack_memory_of_argument (arg : argument) : int = 
  match arg with
  | ArgConstant(_) -> 0
  | ArgRegister(_) -> 0
  | ArgLabelOffset (_,_) -> 0 
  | ArgMemory(addr) -> 
    match addr with 
    | AddrByRegister(_) -> 0
    | AddrByRegisterOffset(_,int) -> -int
    | AddrByLabel(_) -> 0
    | AddrByRegisterProductOffset(_,_,_) -> 0 (* currently only used to access heap *)

;;

let greater_offset (arg1 : argument) (arg2 : argument) : int =
  if stack_memory_of_argument arg1 > stack_memory_of_argument arg2 then
    stack_memory_of_argument arg1
  else
    stack_memory_of_argument arg2
;;

let stack_memory_of_instruction (instr : instruction) : int = 
  match instr with
  | AsmAdd(arg1,arg2) -> greater_offset arg1 arg2
  | AsmIMul(arg1,arg2) -> greater_offset arg1 arg2
  | AsmMov(arg1,arg2) -> greater_offset arg1 arg2
  | AsmSub(arg1,arg2) -> greater_offset arg1 arg2
  | _ -> 0
;;

let stack_memory_of_instruction_list (instr_list: instruction list) : int = 
  let rec loop (instr_list: instruction list) (max : int) : int =
    match instr_list with 
    | [] -> max
    | instr::rest -> if stack_memory_of_instruction instr > max then 
        loop rest (stack_memory_of_instruction instr)
      else 
        loop rest max
  in loop instr_list 0
;;

let check_int (reg : register) : instruction list = 
  let int_lbl = fresh_name "is_int" in 
  [AsmMov(ArgRegister(R11),ArgRegister(reg));
   AsmAnd(ArgRegister(reg),ArgConstant("1"));
   AsmCmp(ArgRegister(reg),ArgConstant("1"));
   AsmJne(int_lbl);
   AsmMov(ArgRegister(RDI),ArgConstant("1"));
   AsmCall("stopWithError");
   AsmLabel(int_lbl);
   AsmMov(ArgRegister(reg),ArgRegister(R11))
  ]
;;

let check_bool (reg : register) : instruction list = 
  let bool_lbl = fresh_name "is_bool" in 
  [AsmMov(ArgRegister(R11),ArgRegister(reg));
   AsmAnd(ArgRegister(reg),ArgConstant("3"));
   AsmCmp(ArgRegister(reg),ArgConstant("3"));
   AsmJe(bool_lbl);
   AsmMov(ArgRegister(RDI),ArgConstant("2"));
   AsmCall("stopWithError");
   AsmLabel(bool_lbl);
   AsmMov(ArgRegister(reg),ArgRegister(R11))
  ]
;;

let check_tuple (reg : register) : instruction list = 
  let end_lbl = fresh_name "end" in
  let error_lbl = fresh_name "error" in
  [AsmMov(ArgRegister(R9), ArgRegister(reg));
   AsmAnd(ArgRegister(R9), ArgConstant("3"));
   AsmCmp(ArgRegister(R9), ArgConstant("1")); (* if equal, ends in 01 *)
   AsmJne(error_lbl);
   AsmMov(ArgRegister(R9), ArgRegister(reg));
   AsmSub(ArgRegister(R9),ArgConstant("1"));
   AsmMov(ArgRegister(R11), ArgMemory(AddrByRegister(R9))); (* R10 has first element*)
   AsmShr(ArgRegister(R11), ArgConstant("63"));
   AsmCmp(ArgRegister(R11), ArgConstant("1")); (* if not equal, its a tuple *)
   AsmJne(end_lbl);
   AsmLabel(error_lbl);
   AsmMov(ArgRegister(RDI),ArgConstant("3"));
   AsmCall("stopWithError");
   AsmLabel(end_lbl)]
;;

let check_closure (reg : register) : instruction list = 
  let end_lbl = fresh_name "end" in
  let error_lbl = fresh_name "error" in
  [AsmMov(ArgRegister(R9), ArgRegister(reg));
   AsmAnd(ArgRegister(R9), ArgConstant("3"));
   AsmCmp(ArgRegister(R9), ArgConstant("1")); (* if equal, ends in 01 *)
   AsmJne(error_lbl);
   AsmMov(ArgRegister(R9), ArgRegister(reg));
   AsmSub(ArgRegister(R9),ArgConstant("1"));
   AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(R9))); (* R10 has first element*)
   AsmShr(ArgRegister(R10), ArgConstant("63"));
   AsmCmp(ArgRegister(R10), ArgConstant("1")); (* if equal equal, its a closure *)
   AsmJe(end_lbl);
   AsmLabel(error_lbl);
   AsmMov(ArgRegister(RDI),ArgConstant("5"));
   AsmCall("stopWithError");
   AsmLabel(end_lbl)]
;;

let check_index (index : register) (tuple_ptr : register) : instruction list = 
  let error_lbl = fresh_name "error" in
  let end_lbl = fresh_name "end" in
  [AsmMov(ArgRegister(R9), ArgRegister(index));
   AsmShr(ArgRegister(R9), ArgConstant("1"));
   AsmCmp(ArgRegister(R9),ArgMemory(AddrByRegister(tuple_ptr))); 
   AsmJge(error_lbl);
   AsmCmp(ArgRegister(R9),ArgConstant("0"));
   AsmJl(error_lbl);
   AsmJmp(end_lbl);
   AsmLabel(error_lbl);
   (* exit with error code 4 *)
   AsmMov(ArgRegister(RDI),ArgConstant("4"));
   AsmCall("stopWithError");
   AsmLabel(end_lbl)
  ]
;;

let copy_closure (new_arg : argument) 
    (old_closure : argument) 
    (new_closure : register) 
    (num_bytes : argument): instruction list =
  (* new_arg: bird value that is added to arguments list
     old_closure: bird pointer to old closure on heap 
     new_closure : machine pointer to new closure location 
     num_bytes : register containing size of new closure *)
  [AsmMov(ArgRegister(RSI), old_closure);
   AsmSub(ArgRegister(RSI), ArgConstant("1"));
   AsmMov(ArgRegister(RDI), ArgRegister(new_closure));
   AsmMov(ArgRegister(RCX), num_bytes);
   AsmRepMovsq;
   AsmMov(ArgRegister(RCX), num_bytes);
   AsmMov(ArgRegister(R9), new_arg);
   AsmMov((ArgRegister(RDI)), ArgRegister(new_closure));
   AsmMov(ArgMemory(AddrByRegisterProductOffset(RDI,RCX,8)), ArgRegister(R9));
   AsmMov(ArgRegister(R8), ArgMemory(AddrByRegister(new_closure)));
   AsmAdd(ArgRegister(R8), ArgConstant("1"));
   AsmMov(ArgMemory(AddrByRegister(new_closure)), ArgRegister(R8))
  ]
;;

let check_heap_space (needed_bytes : register) : instruction list =
  let heap_lbl = fresh_name "end_gc" in
  [AsmMov(ArgRegister(R11), ArgMemory(AddrByLabel("heap_cursor")));
   AsmAdd(ArgRegister(R11), ArgRegister(needed_bytes)); (* adds needed bytes to heap cursor *)
   AsmCmp(ArgMemory(AddrByLabel("end_of_heap")), ArgRegister(R11)); (* heap_cursor + bytes is stored in R11 *)
   AsmJge(heap_lbl);(* if the end_of_heap >= heap_cursor then we don't need to run gc so we will jump *)
   AsmMov(ArgMemory(AddrByLabel("end_of_stack")),ArgRegister(RSP)); (* save the top of stack before calling gc *)
   AsmMov(ArgRegister(RDI), ArgRegister(needed_bytes));
   AsmCall("gc");
   AsmLabel(heap_lbl)
  ] 
;;

let rec mark_tail_positions_expr (e : expr) : expr = 
  (* tail calls can occur in both statements of if expression or second expression of let *)
  (* if there is a fcn call in tail expression, mark that EAppl as true *)
  (* program = (decl list, expr), decl = (string, string list, expr)*)
  match e with 
  | EIf(e1, e2, e3) -> EIf(e1,
                           (match e2 with 
                            | EAppl(inner_e1, inner_e2, _) -> EAppl(inner_e1, inner_e2, true)
                            | _ -> mark_tail_positions_expr e2),
                           (match e3 with 
                            | EAppl(inner_e1, inner_e2, _) -> EAppl(inner_e1, inner_e2, true)
                            | _ -> mark_tail_positions_expr e2)
                          )
  | ELet(var, e1, e2) -> ELet(var, e1,
                              match e2 with 
                              | EAppl(inner_e1, inner_e2, _) -> EAppl(inner_e1, inner_e2, true)
                              | _ -> mark_tail_positions_expr e2
                             )
  | _ -> e
;;

let mark_tail_positions_decl (DFunction(str, str_list, e) : declaration) : declaration = 
  DFunction(str, str_list,
            match e with 
            | EAppl(e1,e2,_) -> EAppl(e1,e2,true)
            | _ -> mark_tail_positions_expr e
           )
;;

let mark_tail_positions (Program(decl_list, e) : program) : program = 
  Program(List.map mark_tail_positions_decl decl_list, e)
;;

let normal_fn_call (left_addr: argument)(right_addr: argument) : instruction list =
   (* code for fcn call *)
     (* allocate (a+1)*8 bytes of stack memory for all arguments, a+1 is in R8 *)
     [AsmIMul(ArgRegister(R8), ArgConstant("8")); (* R8 has a+1 *)
     AsmSub(ArgRegister(RSP), ArgRegister(R8)); (* R8 has 8*(a+1) *)
     (* copy args from closure to stack, use rep mov sq *)
     AsmAdd(ArgRegister(R9), ArgConstant("32")); (* R9 points to start of args in closure *)
     AsmMov(ArgRegister(RSI), ArgRegister(R9));
     AsmMov(ArgRegister(RDI), ArgRegister(RSP));
     AsmMov(ArgRegister(R9), ArgMemory(AddrByRegisterOffset(R9, -32)));
     AsmMov(ArgRegister(R10), ArgConstant(hex_of_bool false));
     AsmAnd(ArgRegister(R9), ArgRegister(R10));
     AsmMov(ArgRegister(RCX), ArgRegister(R9));
     AsmRepMovsq;
     (* copy last arg (in right_addr) to stack below other args *)
     AsmShr(ArgRegister(R8), ArgConstant("3")); (* R8 has a+1 *)
     AsmSub(ArgRegister(R8), ArgConstant("1")); 
     AsmMov(ArgRegister(R10), right_addr);
     AsmMov(ArgMemory(AddrByRegisterProductOffset(RSP, R8, 8)), ArgRegister(R10)); (* negative?*)
     (* call by moving fcn address to RAX then AsmCall("rax")*)
     AsmMov(ArgRegister(R9), left_addr);  (* puts the bird pointer in r9 *)
     AsmSub(ArgRegister(R9), ArgConstant("1"));
     AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(R9, 24))); (* moves the fn pointer in closure into rax *)
     AsmCall("rax");
     (* move rsp to remove args from stack *)
     AsmMov(ArgRegister(R8), left_addr); (* move closure pointer into r8 *)
     AsmSub(ArgRegister(R8), ArgConstant("1")); (* transform from bird pointer to machine pointer *)
     AsmMov(ArgRegister(R9), ArgMemory(AddrByRegisterOffset(R8,16))); (* pull the number of params from the closure*)
     AsmIMul(ArgRegister(R9), ArgConstant("8"));
     AsmAdd(ArgRegister(RSP), ArgRegister(R9))]
;;

let execute_tail_call 
    (tail : bool) 
    (left_addr : argument) (right_addr: argument)
    (env : environment): 
  (* left_addr - BIRD pointer to closure with one missing arg
     right_addr - our last arg
     OTHER:
     R8 has a+1 in it
     R9 has machine ptr to closure
     should check that function is ready to be called before calling this fun
  *)
  instruction list = 
  if tail then (* also need to check params of caller >= args of callee *)
    (let (_,_,params) = env in
     let no_tco_lbl = fresh_name "no_tco" in
     let end_lbl = fresh_name "end" in
     [AsmMov(ArgRegister(R10), ArgConstant(string_of_int params)); (* R10 has caller number of params *)
      AsmCmp(ArgRegister(R8), ArgRegister(R10));
      AsmJg(no_tco_lbl); (* if a + 1 > caller params *)
      (* copy args from closure into current arg*)
      AsmAdd(ArgRegister(R9), ArgConstant("32")); (* R9 points to start of args in closure, machine ptr *)
      AsmMov(ArgRegister(RSI), ArgRegister(R9));
      AsmMov(ArgRegister(RDI), ArgRegister(RBP)); (* DOES RBP NEED TO BE OFFSET?*)
      AsmAdd(ArgRegister(RDI), ArgConstant("16")); (* I THINK THIS IS THE OFFSET WE NEED *)
      AsmMov(ArgRegister(R9), ArgMemory(AddrByRegisterOffset(R9, -32))); 
      AsmMov(ArgRegister(R10), ArgConstant(hex_of_bool false));
      AsmAnd(ArgRegister(R9), ArgRegister(R10)); 
      AsmMov(ArgRegister(RCX), ArgRegister(R9)); (* R9 has number of args in closure *)
      AsmRepMovsq;
      (* copy last arg (in right_addr) to stack below other args *)
      (* AsmSub(ArgRegister(R8), ArgConstant("1")); R8 has # of args now *)
      AsmMov(ArgRegister(R10), right_addr);
      AsmMov(ArgMemory(AddrByRegister(RDI)), ArgRegister(R10));
      (* fill in remaining arg space *)
      AsmMov(ArgRegister(RAX), ArgConstant("0"));
      AsmAdd(ArgRegister(RDI), ArgConstant("8")); (* RDI has location of last added arg *)
      AsmMov(ArgRegister(RCX), ArgConstant(string_of_int params)); (* RCX has caller number of params *)
      AsmSub(ArgRegister(RCX), ArgRegister(R8));
      AsmStosq;
      (* puts the bird pointer in r9 *)
      AsmMov(ArgRegister(R9), left_addr);  
      AsmSub(ArgRegister(R9), ArgConstant("1"));
      (* tear down stack *)
      AsmMov(ArgRegister(RSP), ArgRegister(RBP));
      AsmPop(ArgRegister(RBP));
      (* jump to function code rather than call it *)
      AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(R9, 24))); (* moves the fn pointer in closure into rax *)
      AsmJmp("rax");
      AsmJmp(end_lbl);
      AsmLabel(no_tco_lbl);
     ] @ normal_fn_call left_addr right_addr @
     [AsmLabel(end_lbl)])
  else
    (normal_fn_call left_addr right_addr)
;;

let rec compile_expression (env : environment) (e : expr)
  : instruction list =
  match e with
  | EInt(i) -> [AsmMov(ArgRegister(RAX),ArgConstant(string_of_twice_int i))]
  | EBool(boolean) -> [AsmMov(ArgRegister(RAX),ArgConstant(hex_of_bool boolean))]
  | EVar(str) -> [AsmMov(ArgRegister(RAX), find_named_variable str env)]
  | EUnaryOp(unary_op,expression) ->
    (compile_expression env expression) @
    (match unary_op with
     | OpAfter -> check_int (RAX) @ [AsmAdd(ArgRegister(RAX),ArgConstant("2"))]
     | OpBefore -> check_int (RAX) @ [AsmSub(ArgRegister(RAX),ArgConstant("2"))]
     | OpIsInt -> [AsmShl(ArgRegister(RAX), ArgConstant("63"));
                   AsmMov(ArgRegister(R10),ArgConstant(hex_of_bool true));
                   AsmXor(ArgRegister(RAX),ArgRegister(R10))]
     | OpIsBool ->
       let bool_lbl = fresh_name "bool" in
       let end_lbl = fresh_name "end" in
       [AsmAnd(ArgRegister(RAX), ArgConstant("3"));
        AsmCmp(ArgRegister(RAX), ArgConstant("3")); (* if equal, it's a boolean *)
        AsmJe(bool_lbl);
        AsmMov(ArgRegister(RAX),ArgConstant(hex_of_bool false));
        AsmJmp(end_lbl);
        AsmLabel(bool_lbl);
        AsmMov(ArgRegister(RAX),ArgConstant(hex_of_bool true));
        AsmLabel(end_lbl)]
     | OpPrint -> [AsmPush(ArgRegister(RAX)); 
                   AsmMov(ArgRegister(RDI), ArgRegister(RAX));
                   AsmCall("printValue"); 
                   AsmPop(ArgRegister(RAX))]
     | OpIsTuple -> 
       let false_lbl = fresh_name "false" in
       let end_lbl = fresh_name "end" in
       [AsmMov(ArgRegister(R10), ArgRegister(RAX)); (* tuple ptr in R10 *)
        AsmAnd(ArgRegister(R10), ArgConstant("3"));
        AsmCmp(ArgRegister(R10), ArgConstant("1")); (* if equal, last 2 bits are 01 *)
        AsmJne(false_lbl);
        AsmSub(ArgRegister(RAX), ArgConstant("1"));
        AsmMov(ArgRegister(R9), ArgMemory(AddrByRegister(RAX)));
        AsmShr(ArgRegister(R9), ArgConstant("63"));
        AsmCmp(ArgRegister(R9), ArgConstant("1")); (* if equal, it's not a tuple *)
        AsmJe(false_lbl);
        AsmMov(ArgRegister(RAX),ArgConstant(hex_of_bool true));
        AsmJmp(end_lbl);
        AsmLabel(false_lbl);
        AsmMov(ArgRegister(RAX),ArgConstant(hex_of_bool false));
        AsmLabel(end_lbl)]
    )
  | EBinaryOp(binary_op, expr1, expr2) -> 
    let (left_addr,left_env) = allocate_temp_variable env in
    let (right_addr, _) = allocate_temp_variable left_env in
    compile_expression env expr1 @
    [AsmMov(left_addr, ArgRegister(RAX))] @
    compile_expression left_env expr2  @
    [AsmMov(right_addr, ArgRegister(RAX)); 
     AsmMov(ArgRegister(RAX), left_addr);
     AsmMov(ArgRegister(R10), right_addr)] @
    (match binary_op with
     | OpPlus -> check_int (RAX) @ check_int (R10) @ [AsmAdd(ArgRegister(RAX), ArgRegister(R10))]
     | OpMinus-> check_int (RAX) @ check_int (R10) @ [AsmSub(ArgRegister(RAX), ArgRegister(R10))]
     | OpTimes-> check_int (RAX) @ check_int (R10) @
                 [AsmShr(ArgRegister(RAX), ArgConstant("1")); AsmIMul(ArgRegister(RAX), ArgRegister(R10))]
     | OpLessThan -> check_int (RAX) @ check_int (R10) @ compare_intstructions(AsmJl("temp"))
     | OpGreaterThan -> check_int (RAX) @ check_int (R10) @ compare_intstructions(AsmJg("temp"))
     | OpEqualTo -> check_int (RAX) @ check_int (R10) @ compare_intstructions(AsmJe("temp"))
     | OpAnd -> check_bool (RAX) @ check_bool (R10) @ [AsmAnd(ArgRegister(RAX),ArgRegister(R10))]
     | OpOr -> check_bool (RAX) @ check_bool (R10) @ [AsmOr(ArgRegister(RAX),ArgRegister(R10))]
     | OpTupleIndex ->  check_tuple (RAX) @ check_int (R10) @ 
                        [AsmSub(ArgRegister(RAX),ArgConstant("1"))] @ (* clears the last bit to 0 *)
                        check_index R10 RAX @
                        [AsmAdd(ArgRegister(R10),ArgConstant("4"));
                         AsmShr(ArgRegister(R10),ArgConstant("1"));
                         AsmMov(ArgRegister(RAX),ArgMemory(AddrByRegisterProductOffset(RAX,R10,8)))]
    ) @
    [AsmMov(ArgRegister(R10), ArgConstant("0"));
     AsmMov(left_addr, ArgRegister(R10));
     AsmMov(right_addr, ArgRegister(R10))]
  | ETuple(lst) -> 
    let size = List.length lst in 
    let (tuple_addr, tuple_env) = allocate_temp_variable env in 
    let rec loop (n : int) (lst : expr list) : instruction list = 
      match lst with 
      | [] -> []
      | expr::rest -> compile_expression tuple_env expr @ 
                      [AsmMov(ArgRegister(R8),tuple_addr);
                       AsmMov(ArgMemory(AddrByRegisterOffset(R8,n)),
                              ArgRegister(RAX))] @
                      loop (n+8) rest
    in

    [AsmMov(ArgRegister(R10),ArgConstant(string_of_int (8*(size+2))));]@ (*move size into a register so that memory know we are working in 8 bytes*)
    check_heap_space (R10) @
    [AsmMov(ArgRegister(R11), ArgMemory(AddrByLabel("heap_cursor"))); (* pointer to start of tuple stored in r11 *)
     AsmMov(tuple_addr, ArgRegister(R11)); (* tuple_addr contains the machine pointer for our new tuple*)
     AsmMov(ArgRegister(R10),ArgConstant(string_of_int (8*(size+2)))); (* R10 has bytes that we need to move heap_cursor*)
     AsmAdd(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(R10)); (* moving heap_cursor*)
     AsmMov(ArgRegister(R10),ArgConstant(string_of_int size));
     (* AsmMov(ArgRegister(R11), tuple_addr); *)
     AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister(R10)); (* writing size of our new tuple *)
     AsmMov(ArgRegister(R10), ArgConstant("0"));
     AsmMov(ArgMemory(AddrByRegisterOffset(R11,8)), ArgRegister(R10))] @ (* writing 0 into GC word *)
    loop 16 lst @
    [AsmMov(ArgRegister(RAX), tuple_addr);
     AsmAdd(ArgRegister(RAX), ArgConstant("1"));
     AsmMov(ArgRegister(R10), ArgConstant("0"));
     AsmMov(tuple_addr, ArgRegister(R10))]
  | ELet(var,expr1,expr2) ->
    let new_env = allocate_named_variable var env in 
    compile_expression env expr1 @ 
    [AsmMov(find_named_variable var new_env, ArgRegister(RAX))] @
    compile_expression new_env expr2 @
    [AsmMov(ArgRegister(R10), ArgConstant("0"));
     AsmMov(find_named_variable var new_env, ArgRegister(R10))]
  | EIf(expr1, expr2, expr3) -> 
    let else_lbl = fresh_name "else" in
    let end_lbl = fresh_name "end" in
    compile_expression env expr1 @
    check_bool (RAX) @
    [AsmMov(ArgRegister(R10), ArgConstant(hex_of_bool true));
     AsmCmp(ArgRegister(RAX), ArgRegister(R10));
     AsmJne(else_lbl)] @
    compile_expression env expr2 @
    [AsmJmp(end_lbl);
     AsmLabel(else_lbl)] @
    compile_expression env expr3 @
    [AsmLabel(end_lbl)]
  | EAppl (expr1,expr2, tail) ->
    let continue = fresh_name "end" in
    let fn_call = fresh_name "fn_call" in
    let end_lbl = fresh_name "end" in 
    let (left_addr,left_env) = allocate_temp_variable env in
    let (right_addr, _) = allocate_temp_variable left_env in
    compile_expression env expr1 @ (* pointer to closure in RAX *)
    check_closure (RAX) @
    [AsmMov(left_addr, ArgRegister(RAX));
     AsmMov(ArgRegister(R9),ArgMemory(AddrByRegisterOffset(RAX, -1)));
     AsmShr(ArgRegister(R9), ArgConstant("63"));
     AsmCmp(ArgRegister(R9), ArgConstant("1")); (* if equal, it's a closure *)
     AsmJe(continue);
     AsmMov(ArgRegister(RDI),ArgConstant("5"));
     AsmCall("stopWithError");
     AsmLabel(continue);
     AsmMov(ArgRegister(R9),ArgMemory(AddrByRegisterOffset(RAX, -1)))] @
    compile_expression left_env expr2  @ (* argument in RAX *)
    [AsmMov(right_addr, ArgRegister(RAX));
     AsmMov(ArgRegister(R9),left_addr); 
     AsmSub(ArgRegister(R9), ArgConstant("1")); (* R9 has machine pointer to closure *)
     AsmMov(ArgRegister(R8), ArgMemory(AddrByRegister(R9))); (* puts number of args in R8 *)
     AsmMov(ArgRegister(R10), ArgConstant(hex_of_bool false));
     AsmAnd(ArgRegister(R8), ArgRegister(R10)); (* sets tuple/closure tag bit to 0 *)
     AsmAdd(ArgRegister(R8), ArgConstant("1")); (* R8 has a+1 now *)
     AsmCmp(ArgRegister(R8), ArgMemory(AddrByRegisterOffset(R9,16))); (* total parameters is 3rd element in closure*)
     AsmJge(fn_call); (* but if greater, should throw an error? *)
     AsmAdd(ArgRegister(R8), ArgConstant("4"));
     AsmMov(ArgRegister(R9), ArgRegister(R8)); 
     AsmSub(ArgRegister(R9), ArgConstant("1")); (* R9 has number of words to copy *)
     AsmIMul(ArgRegister(R8), ArgConstant("8")); (* R8 has number of bytes the heap cursor should move *)
     AsmPush(ArgRegister(R8));
     AsmPush(ArgRegister(R9))] @
    check_heap_space (R8) @
    [AsmMov(ArgRegister(R10),ArgMemory(AddrByLabel("heap_cursor"))); (* Our new closure pointer is in R10 *)
     AsmPop(ArgRegister(R9));
     AsmPop(ArgRegister(R8));
     AsmAdd(ArgMemory(AddrByLabel("heap_cursor")), ArgRegister(R8))]@
    copy_closure (right_addr) left_addr (R10) (ArgRegister(R9)) @
    [AsmMov(ArgRegister(RAX), ArgRegister(R10));
     AsmAdd(ArgRegister(RAX), ArgConstant("1")); (* RAX has bird pointer to new closure *)
     AsmJmp(end_lbl);
     AsmLabel(fn_call);] @
    (* doesn't matter which env we pass bc we only care about param number which shouldn't change *)
    execute_tail_call tail left_addr right_addr env @ 
    [AsmLabel(end_lbl);
     AsmMov(ArgRegister(R10), ArgConstant("0"));
     AsmMov(left_addr, ArgRegister(R10));
     AsmMov(right_addr, ArgRegister(R10));
    ]
  | ESet (e1, e2, e3) -> 
    let (tuple_addr, env1) = allocate_temp_variable env in 
    let (index_addr, env2) = allocate_temp_variable env1 in 
    compile_expression env e1 @
    check_tuple (RAX) @ 
    [AsmMov(tuple_addr, ArgRegister(RAX))] @ (* bird ptr on stack *)
    compile_expression env1 e2 @
    check_int (RAX) @
    [AsmMov(ArgRegister(R10), tuple_addr);
     AsmSub(ArgRegister(R10), ArgConstant("1"))] @
    check_index RAX R10 @ (* index in RAX, tuple machine ptr in R10 *)
    [AsmMov(index_addr, ArgRegister(RAX))] @
    compile_expression env2 e3 @
    [AsmMov(ArgRegister(R8), index_addr); (* index in R8 *)
     AsmAdd(ArgRegister(R8),ArgConstant("4")); (* adding bird 2 to account for size and GC word *)
     AsmShr(ArgRegister(R8),ArgConstant("1")); (* shifting by 2 to get machine index *)
     AsmMov(ArgRegister(R10), tuple_addr);
     AsmSub(ArgRegister(R10), ArgConstant("1"));
     AsmMov(ArgMemory(AddrByRegisterProductOffset(R10,R8,8)), ArgRegister(RAX));
     AsmMov(ArgRegister(R10), ArgConstant("0"));
     AsmMov(tuple_addr, ArgRegister(R10));
     AsmMov(index_addr, ArgRegister(R10))]
;;

(* assume rsp points to top of local vars, rbp is at base, call will push return address, callee then sets up stack 
   frame by pushing old rbp, moving up rbp and rsp, first arg is at [rbp+16]
*)

let compile_declaration (env : environment) (d : declaration) : instruction list =
  match d with
  | DFunction (name, param_list, body) ->
    let func_env = allocate_parameters param_list env in
    let func_lbl = "fn_"^name in
    let instructions = compile_expression func_env body in
    let allocation = stack_memory_of_instruction_list instructions in
    [AsmLabel(func_lbl)] @
    [AsmPush(ArgRegister(RBP));
     AsmMov(ArgRegister(RBP), ArgRegister(RSP));
     AsmSub(ArgRegister(RSP), ArgConstant(string_of_int allocation));
     AsmMov(ArgRegister(RAX), ArgConstant("0"));
     AsmMov(ArgRegister(RDI), ArgRegister(RSP));
     AsmMov(ArgRegister(RCX), ArgConstant(string_of_int allocation));
     AsmShr(ArgRegister(RCX), ArgConstant("3"));
     AsmStosq
    ] @ (* push callee-saved registers? *)
    instructions @ 
    [AsmMov(ArgRegister(RSP), ArgRegister(RBP));
     AsmPop(ArgRegister(RBP));
     AsmRet]
;;

let create_fn_data_section (d : declaration) : instruction list = 
  match d with 
  | DFunction(name,param_list,_) -> 
    let len = List.length param_list in
    [AsmAlign(8);
     AsmLabel("closure_of_"^name);
     AsmDq(["0x8000000000000000"; "0"; string_of_int len; "fn_"^name])]
;;

let rec create_data_section (d_list : declaration list): instruction list = 
  [AsmSection(".data");
   AsmAlign(8);
   AsmLabel("heap_cursor");
   AsmDq(["0"])]
  @ List.concat (List.map create_fn_data_section d_list) @
  [AsmLabel("start_of_stack");
   AsmDq(["0"]);
   AsmLabel("end_of_stack");
   AsmDq(["0"]);
   AsmLabel("start_of_heap");
   AsmDq(["0"]);
   AsmLabel("end_of_heap");
   AsmDq(["0"])]
;;

let rec create_program_env (d_list : declaration list) (env : environment) : environment = 
  match d_list with
  | [] -> env
  | DFunction(name,_,_) :: rest -> 
    let temp_env = allocate_closure name env in
    create_program_env (rest) (temp_env)
;;

let rec compile_program (p : program) : instruction list =
  let () = check_well_formed p in
  let p' = mark_tail_positions p in
  match p' with 
  | Program(decl_lst,e) -> 
    let program_env = create_program_env decl_lst empty_environment in
    let instructions = compile_expression program_env e in
    let allocation = stack_memory_of_instruction_list instructions in
    [AsmLabel("bird_main");
     AsmMov(ArgMemory(AddrByLabel("heap_cursor")),ArgRegister(RDI));
     AsmMov(ArgMemory(AddrByLabel("start_of_heap")), ArgRegister(RDI));
     AsmMov(ArgMemory(AddrByLabel("end_of_heap")), ArgRegister(RSI)); (* second parameter passed to bird main is end of heap *)
     AsmPush(ArgRegister(RBP));
     AsmMov(ArgRegister(RBP), ArgRegister(RSP));
     AsmSub(ArgRegister(RSP), ArgConstant(string_of_int allocation));
     AsmMov(ArgMemory(AddrByLabel("start_of_stack")), ArgRegister(RBP)); 
     AsmMov(ArgRegister(RAX), ArgConstant("0")); (* start of steps to clear stack to 0's*)
     AsmMov(ArgRegister(RDI), ArgRegister(RSP));
     AsmMov(ArgRegister(RCX), ArgConstant(string_of_int allocation));
     AsmShr(ArgRegister(RCX), ArgConstant("3"));
     AsmStosq] @ (* actually clears stack to all 0's *)
    instructions @ 
    [AsmMov(ArgRegister(RSP), ArgRegister(RBP));
     AsmPop(ArgRegister(RBP));
     AsmRet] @
    List.concat (List.map (compile_declaration program_env) decl_lst )
;;

let compile_to_assembly_code (p : program) : string =
  let Program(d_list,_) = p in
  let instructions = compile_program p in
  let instruction_code = code_of_instruction_list instructions in
  code_of_instruction_list (create_data_section d_list) ^
  "section .text\n" ^
  "global bird_main\n" ^
  "global start_of_stack\n" ^
  "global end_of_stack\n" ^
  "global start_of_heap\n" ^
  "global end_of_heap\n" ^
  "global heap_cursor\n" ^
  "extern printValue\n" ^
  "extern stopWithError\n" ^
  "extern gc\n" ^
  "extern dump_heap\n" ^
  instruction_code ^
  "\n"
;;
