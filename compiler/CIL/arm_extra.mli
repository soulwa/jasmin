open Bool
open Arch_decl
open Arch_extra
open Arm
open Arm_decl
open Arm_instr_decl
open Compiler_util
open Eqtype
open Expr
open Sopn
open Ssrbool
open Utils0

type __ = Obj.t

val arm_extra_op_beq : __ -> bool

val arm_extra_op_eq_dec : __ -> bool

val arm_extra_op_eq_axiom : __ -> reflect

val arm_extra_op_eqMixin : __ Equality.mixin_of

val arm_extra_op_eqType : Equality.coq_type

val eqTC_arm_extra_op : __ eqTypeC

val get_instr_desc : __ -> instruction_desc

val arm_extra_op_decl : __ asmOp

val assemble_extra :
  instr_info -> lval list -> pexpr list -> (((register, __, __, rflag, condt,
  arm_op) asm_op_msb_t * lval list) * pexpr list) cexec

val arm_extra : (register, __, __, rflag, condt, arm_op, __) asm_extra

type arm_extended_op =
  (register, __, __, rflag, condt, arm_op, __) extended_op

val coq_Oarm : arm_op -> arm_extended_op sopn
