open BinNums
open Arch_extra
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Eqtype
open Expr
open Lowering
open Seq
open Shift_kind
open Sopn
open Ssrbool
open Type
open Utils0
open Var0
open Wsize

type __ = Obj.t

type fresh_vars = { fv_NF : Equality.sort; fv_ZF : Equality.sort;
                    fv_CF : Equality.sort; fv_VF : Equality.sort }

val all_fresh_vars : fresh_vars -> Equality.sort list

val fvNF : fresh_vars -> Var.var

val fvZF : fresh_vars -> Var.var

val fvCF : fresh_vars -> Var.var

val fvVF : fresh_vars -> Var.var

val fresh_flags : fresh_vars -> Var.var list

val fvars : fresh_vars -> SvExtra.Sv.t

val mk_fv_vari : Var.var -> var_i

val mk_fv_gvar : Var.var -> gvar

val lflags_of_mn : fresh_vars -> arm_mnemonic -> lval list

val lower_TST : pexpr -> pexpr -> pexpr list option

val lower_condition_Papp2 :
  fresh_vars -> sop2 -> pexpr -> pexpr -> ((arm_mnemonic * pexpr) * pexpr
  list) option

val lower_condition_pexpr :
  fresh_vars -> pexpr -> (((lval list * (register, __, __, rflag, condt,
  arm_op, __) extended_op sopn) * pexpr list) * pexpr) option

val lower_condition :
  fresh_vars -> pexpr -> (register, __, __, rflag, condt, arm_op, __)
  extended_op instr_r list * pexpr

val get_arg_shift :
  wsize -> pexpr list -> ((pexpr * shift_kind) * pexpr) option

val arg_shift : arm_mnemonic -> wsize -> pexpr list -> arm_op * pexpr list

val lower_Pvar :
  (var_i -> bool) -> wsize -> gvar -> (arm_op * pexpr list) option

val lower_Pload :
  wsize -> wsize -> var_i -> pexpr -> (arm_op * pexpr list) option

val is_load : (var_i -> bool) -> pexpr -> bool

val lower_Papp1 :
  (var_i -> bool) -> wsize -> sop1 -> pexpr -> (arm_op * pexpr list) option

val lower_Papp2_op :
  wsize -> sop2 -> pexpr -> pexpr -> ((arm_mnemonic * pexpr) * pexpr list)
  option

val lower_Papp2 :
  wsize -> sop2 -> pexpr -> pexpr -> (arm_op * pexpr list) option

val lower_pexpr_aux :
  (var_i -> bool) -> wsize -> pexpr -> (arm_op * pexpr list) option

val no_pre :
  (arm_op * pexpr list) option -> (((register, __, __, rflag, condt, arm_op,
  __) extended_op instr_r list * arm_op) * pexpr list) option

val lower_pexpr :
  fresh_vars -> (var_i -> bool) -> wsize -> pexpr -> (((register, __, __,
  rflag, condt, arm_op, __) extended_op instr_r list * arm_op) * pexpr list)
  option

val lower_store : wsize -> pexpr -> (arm_op * pexpr list) option

val lower_cassgn :
  fresh_vars -> (var_i -> bool) -> lval -> stype -> pexpr -> ((register, __,
  __, rflag, condt, arm_op, __) extended_op instr_r list * ((lval
  list * (register, __, __, rflag, condt, arm_op, __) extended_op
  sopn) * pexpr list)) option

val lower_add_carry :
  lval list -> pexpr list -> ((lval list * (register, __, __, rflag, condt,
  arm_op, __) extended_op sopn) * pexpr list) option

val lower_base_op :
  lval list -> arm_op -> pexpr list -> ((lval list * (register, __, __,
  rflag, condt, arm_op, __) extended_op sopn) * pexpr list) option

val lower_copn :
  lval list -> (register, __, __, rflag, condt, arm_op, __) extended_op sopn
  -> pexpr list -> ((lval list * (register, __, __, rflag, condt, arm_op, __)
  extended_op sopn) * pexpr list) option

type lowering_options = unit

val lower_i :
  fresh_vars -> (var_i -> bool) -> (register, __, __, rflag, condt, arm_op,
  __) extended_op instr -> (register, __, __, rflag, condt, arm_op, __)
  extended_op instr list
