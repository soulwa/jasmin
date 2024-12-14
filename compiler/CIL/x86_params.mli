open BinInt
open BinNums
open List0
open Arch_decl
open Arch_extra
open Arch_params
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Linearization
open Seq
open Sopn
open Stack_alloc
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize
open X86_decl
open X86_extra
open X86_instr_decl
open X86_lowering

val lea_ptr :
  lval -> pexpr -> assgn_tag -> coq_Z -> (register, register_ext,
  xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op instr_r

type mov_kind =
| MK_LEA
| MK_MOV

val mk_mov : vptr_kind -> mov_kind

val x86_mov_ofs :
  (Var.var -> bool) -> lval -> assgn_tag -> vptr_kind -> pexpr -> coq_Z ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op instr_r option

val x86_saparams :
  (Var.var -> bool) -> (register, register_ext, xmm_register, rflag, condt,
  x86_op, x86_extra_op) extended_op stack_alloc_params

val x86_allocate_stack_frame :
  var_i -> coq_Z -> (lval list * x86_extended_op sopn) * pexpr list

val x86_free_stack_frame :
  var_i -> coq_Z -> (lval list * x86_extended_op sopn) * pexpr list

val x86_ensure_rsp_alignment :
  var_i -> wsize -> (lval list * x86_extended_op sopn) * pexpr list

val x86_lassign :
  lval -> wsize -> pexpr -> ((lval list * x86_extended_op sopn) * pexpr list)
  option

val x86_liparams :
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op linearization_params

val x86_loparams :
  ((register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op, fresh_vars, lowering_options) lowering_params

val not_condt : condt -> condt

val or_condt : instr_info -> pexpr -> condt -> condt -> condt cexec

val and_condt :
  instr_info -> pexpr -> condt -> condt -> (pp_error_loc, condt) result

val of_var_e_bool : instr_info -> var_i -> rflag cexec

val assemble_cond_r : instr_info -> pexpr -> condt cexec

val assemble_cond : instr_info -> pexpr -> condt cexec

val x86_agparams :
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  asm_gen_params

val x86_is_move_op :
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op asm_op_t -> bool

val x86_params :
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op,
  fresh_vars, lowering_options) architecture_params
