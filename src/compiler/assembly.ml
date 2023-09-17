(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | RBP
  | RDI
  | RSI 
  | RDX 
  | RCX 
  | R8 
  | R9 
  | R10
  | R11
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddrByLabel of string 
  | AddrByRegisterProductOffset of register * register * int 
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgLabelOffset of string * int
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmRet
  | AsmShl of argument * argument
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmXor of argument * argument
  | AsmLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string
  | AsmJge of string
  | AsmJg of string
  | AsmJl of string
  | AsmPush of argument
  | AsmPop of argument
  | AsmCall of string
  | AsmSection of string
  | AsmAlign of int
  | AsmDq of string list
  | AsmRepMovsq
  | AsmStosq
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  match register with
  | RAX -> "rax"
  | RSP -> "rsp"
  | RBP -> "rbp"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
;;

(** A function which transforms an x86 address expression into a string suitable
    for writing into an assembly language file. *)
let code_of_address (address : address) : string =
  match address with
  | AddrByRegister register -> "["^(code_of_register register)^"]"
  | AddrByRegisterOffset(register,n) -> "[" ^ (code_of_register register) ^ "+" ^ string_of_int n ^ "]"
  | AddrByLabel(lbl) -> "["^lbl^"]"
  | AddrByRegisterProductOffset(reg1, reg2, n) -> "["^code_of_register reg1^"+"^
                                                  code_of_register reg2^"*"^string_of_int n^"]"
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  match argument with
  | ArgConstant const -> const
  | ArgRegister register -> (code_of_register register)
  | ArgMemory address -> (code_of_address address)
  | ArgLabelOffset (name, i) -> (name ^" + "^string_of_int i)
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
*)
let code_of_instruction (instruction : instruction) : string =
  match instruction with
  | AsmAdd(arg1, arg2) -> "  add " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmIMul(arg1, arg2) -> "  imul " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmMov(arg1, arg2) -> "  mov " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmSub(arg1, arg2) -> "  sub " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmRet -> "  ret\n"
  | AsmShl(arg1, arg2) -> "  shl " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmShr(arg1, arg2) -> "  shr " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmSal(arg1, arg2) -> "  sal " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmSar(arg1, arg2) -> "  sar " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmAnd(arg1, arg2) -> "  and " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmOr(arg1, arg2) -> "  or " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmXor(arg1, arg2) -> "  xor " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmLabel(str) -> "  " ^ str ^ ": \n"
  | AsmCmp(arg1, arg2) -> "  cmp " ^ (code_of_argument arg1) ^ ", " ^ (code_of_argument arg2) ^ "\n"
  | AsmJmp(str) -> "  jmp " ^ str ^ "\n"
  | AsmJe(str) -> "  je " ^ str ^ "\n"
  | AsmJne(str) -> "  jne " ^ str ^ "\n"
  | AsmJge(str) -> "  jge " ^ str ^ "\n"
  | AsmJg(str) -> "  jg " ^ str ^ "\n"
  | AsmJl(str) -> "  jl " ^ str ^ "\n"
  | AsmPush(arg) -> "  push " ^ (code_of_argument arg) ^ "\n"
  | AsmPop(arg) -> "  pop " ^ (code_of_argument arg) ^ "\n"
  | AsmCall(str) -> "  call " ^ str ^ "\n"
  | AsmSection(str) -> "  section " ^ str ^ "\n"
  | AsmAlign(num)-> " align " ^ string_of_int num ^ "\n"
  | AsmDq(lst) -> " dq " ^ String.join "," lst ^ "\n"
  | AsmRepMovsq -> "  rep movsq\n"
  | AsmStosq -> " rep stosq\n"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec code_of_instruction_list (instruction_list : instruction list) : string =
  match instruction_list with
  | [] -> ""
  | instruction::rest -> (code_of_instruction instruction) ^ (code_of_instruction_list rest)
;;
