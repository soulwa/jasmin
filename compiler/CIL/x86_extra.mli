open Bool
open Datatypes
open Arch_decl
open Arch_extra
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Ssralg
open Ssrbool
open Type
open Utils0
open Word0
open Wsize
open X86
open X86_decl
open X86_instr_decl

type x86_extra_op =
| Oset0 of wsize
| Oconcat128
| Ox86MOVZX32

val x86_extra_op_beq : x86_extra_op -> x86_extra_op -> bool

val x86_extra_op_eq_dec : x86_extra_op -> x86_extra_op -> bool

val x86_extra_op_eq_axiom : x86_extra_op Equality.axiom

val x86_extra_op_eqMixin : x86_extra_op Equality.mixin_of

val x86_extra_op_eqType : Equality.coq_type

val coq_Oset0_instr : wsize -> instruction_desc

val coq_Oconcat128_instr : instruction_desc

val coq_Ox86MOVZX32_instr : instruction_desc

val get_instr_desc : x86_extra_op -> instruction_desc

val prim_string : (char list * x86_extra_op prim_constructor) list

module E :
 sig
  val pass_name : char list

  val error : instr_info -> char list -> pp_error_loc
 end

val assemble_extra :
  instr_info -> x86_extra_op -> lval list -> pexpr list -> (((register,
  register_ext, xmm_register, rflag, condt, x86_op) asm_op_msb_t * lval
  list) * pexpr list) cexec

val eqC_x86_extra_op : x86_extra_op eqTypeC

val x86_extra_op_decl : x86_extra_op asmOp

val x86_extra :
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  asm_extra

type x86_extended_op =
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op

val coq_Ox86 : x86_op -> x86_extended_op sopn
