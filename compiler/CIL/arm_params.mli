open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_params
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Arm_lowering
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Linearization
open Lowering
open Sopn
open Ssrbool
open Stack_alloc
open Type
open Utils0
open Word0
open Wsize

type __ = Obj.t

val addi : lval -> assgn_tag -> pexpr -> coq_Z -> arm_extended_op instr_r

val arm_mov_ofs :
  lval -> assgn_tag -> vptr_kind -> pexpr -> coq_Z -> (register, __, __,
  rflag, condt, arm_op, __) extended_op instr_r option

val arm_saparams :
  (register, __, __, rflag, condt, arm_op, __) extended_op stack_alloc_params

val arm_allocate_stack_frame :
  var_i -> coq_Z -> (lval list * arm_extended_op sopn) * pexpr list

val arm_free_stack_frame :
  var_i -> coq_Z -> (lval list * arm_extended_op sopn) * pexpr list

val arm_ensure_rsp_alignment :
  var_i -> wsize -> (lval list * arm_extended_op sopn) * pexpr list

val arm_lassign :
  lval -> wsize -> pexpr -> ((lval list * (register, __, __, rflag, condt,
  arm_op, __) extended_op sopn) * pexpr list) option

val arm_liparams :
  (register, __, __, rflag, condt, arm_op, __) extended_op
  linearization_params

val arm_fvars_correct :
  fresh_vars -> Equality.coq_type -> progT -> (register, __, __, rflag,
  condt, arm_op, __) extended_op fun_decl list -> bool

val arm_loparams :
  ((register, __, __, rflag, condt, arm_op, __) extended_op, fresh_vars,
  lowering_options) lowering_params

val condt_of_rflag : rflag -> condt

val condt_not : condt -> condt

val condt_and : condt -> condt -> condt option

val condt_or : condt -> condt -> condt option

val is_rflags_GE : rflag -> rflag -> bool

val assemble_cond : instr_info -> pexpr -> condt cexec

val arm_agparams : (register, __, __, rflag, condt, arm_op, __) asm_gen_params

val arm_is_move_op :
  (register, __, __, rflag, condt, arm_op, __) extended_op asm_op_t -> bool

val arm_params :
  (register, __, __, rflag, condt, arm_op, __, fresh_vars, lowering_options)
  architecture_params
