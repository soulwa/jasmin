open Bool
open Arch_decl
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Ssrfun
open Strings
open Type
open Utils0
open Var0
open Wsize
open Xseq

val of_string : stype -> 'a1 coq_ToString -> char list -> 'a1 option

val to_var : stype -> 'a1 coq_ToString -> 'a1 -> Var.var

val of_var : stype -> 'a1 coq_ToString -> Var.var -> 'a1 option

val sopn_implicit_arg :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  Arch_decl.implicit_arg -> implicit_arg

val sopn_arg_desc :
  ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
  Arch_decl.arg_desc -> arg_desc

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) asm_extra = { 
_asm : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm;
_extra : 'extra_op asmOp;
to_asm : (instr_info -> 'extra_op -> lval list -> pexpr list -> ((('reg,
         'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t * lval
         list) * pexpr list) cexec) }

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t =
  'extra_op

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op =
| BaseOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t
| ExtOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t

val extended_op_beq :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op ->
  bool

val extended_op_eq_axiom :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op Equality.axiom

val get_instr_desc :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op -> instruction_desc

val sopn_prim_constructor :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (wsize option -> 'a6 ->
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op) -> 'a6
  Arch_decl.prim_constructor -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op prim_constructor

val sopn_prim_string_base :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (char list * 'a6
  Arch_decl.prim_constructor) list -> (char list * ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op prim_constructor) list

val sopn_prim_string_extra :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (char list * 'a7
  prim_constructor) list -> (char list * ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op prim_constructor) list

val get_prime_op :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (char list * ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op prim_constructor) list

val eqTC_extended_op :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op eqTypeC

val asm_opI :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op asmOp
