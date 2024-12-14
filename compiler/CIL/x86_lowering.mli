open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Compiler_util
open Eqtype
open Expr
open Lea
open Lowering
open Seq
open Sopn
open Ssralg
open Ssrfun
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize
open X86_decl
open X86_extra
open X86_instr_decl

val is_regx_e : (Var.var -> bool) -> pexpr -> bool

val is_regx_l : (Var.var -> bool) -> lval -> bool

val mov_ws :
  (Var.var -> bool) -> wsize -> lval -> pexpr -> assgn_tag -> x86_extended_op
  instr_r

type fresh_vars = { fresh_OF : Equality.sort; fresh_CF : Equality.sort;
                    fresh_SF : Equality.sort; fresh_PF : Equality.sort;
                    fresh_ZF : Equality.sort;
                    fresh_multiplicand : (wsize -> Equality.sort);
                    is_regx : (Var.var -> bool) }

type lowering_options = { use_lea : bool; use_set0 : bool }

val vword : wsize -> Equality.sort -> Var.var

val fv_of : fresh_vars -> Var.var

val fv_cf : fresh_vars -> Var.var

val fv_sf : fresh_vars -> Var.var

val fv_pf : fresh_vars -> Var.var

val fv_zf : fresh_vars -> Var.var

val fvars : fresh_vars -> SvExtra.Sv.t

val disj_fvars : fresh_vars -> SvExtra.Sv.t -> bool

val fvars_correct :
  fresh_vars -> Equality.coq_type -> progT -> (register, register_ext,
  xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op fun_decl list
  -> bool

val var_info_of_lval : lval -> var_info

val stype_of_lval : lval -> stype

val wsize_of_stype : stype -> wsize

val wsize_of_lval : lval -> wsize

val lower_cond_classify :
  fresh_vars -> var_info -> pexpr -> ((((lval
  list * wsize) * pexpr) * pexpr) * pexpr) option

val lower_condition :
  fresh_vars -> var_info -> pexpr -> (register, register_ext, xmm_register,
  rflag, condt, x86_op, x86_extra_op) extended_op instr_r list * pexpr

type add_inc_dec =
| AddInc of pexpr
| AddDec of pexpr
| AddNone

val add_inc_dec_classify : wsize -> pexpr -> pexpr -> add_inc_dec

type sub_inc_dec =
| SubInc
| SubDec
| SubNone

val sub_inc_dec_classify : Equality.sort -> pexpr -> sub_inc_dec

type divmod_pos =
| DM_Fst
| DM_Snd

type lower_cassgn_t =
| LowerMov of bool
| LowerCopn of (register, register_ext, xmm_register, rflag, condt, x86_op,
               x86_extra_op) extended_op sopn * pexpr list
| LowerInc of (register, register_ext, xmm_register, rflag, condt, x86_op,
              x86_extra_op) extended_op sopn * pexpr
| LowerLea of wsize * lea
| LowerFopn of wsize
   * (register, register_ext, xmm_register, rflag, condt, x86_op,
     x86_extra_op) extended_op sopn * pexpr list * wsize option
| LowerDiscardFlags of nat
   * (register, register_ext, xmm_register, rflag, condt, x86_op,
     x86_extra_op) extended_op sopn * pexpr list
| LowerCond
| LowerIf of stype * pexpr * pexpr * pexpr
| LowerDivMod of divmod_pos * signedness * wsize
   * (register, register_ext, xmm_register, rflag, condt, x86_op,
     x86_extra_op) extended_op sopn * pexpr * pexpr
| LowerConcat of pexpr * pexpr
| LowerAssgn

val is_lea : (var_i -> bool) -> wsize -> lval -> pexpr -> lea option

val is_lnot : pexpr -> pexpr option

val is_andn : pexpr -> pexpr -> (pexpr * pexpr) option

val mulr : wsize -> pexpr -> pexpr -> x86_op * pexpr list

val lower_cassgn_classify :
  (var_i -> bool) -> Equality.sort -> pexpr -> lval -> lower_cassgn_t

val coq_Lnone_b : var_info -> lval

type opn_5flags_cases_t =
| Opn5f_large_immed of pexpr * pexpr * pexpr list
| Opn5f_other

val check_signed_range : wsize option -> wsize -> coq_Z -> bool

val opn_5flags_cases :
  pexpr list -> wsize option -> wsize -> opn_5flags_cases_t

val opn_no_imm :
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op sopn -> (register, register_ext, xmm_register, rflag, condt,
  x86_op, x86_extra_op) extended_op sopn

val opn_5flags :
  fresh_vars -> wsize option -> wsize -> var_info -> lval -> lval ->
  assgn_tag -> (register, register_ext, xmm_register, rflag, condt, x86_op,
  x86_extra_op) extended_op sopn -> pexpr list -> (register, register_ext,
  xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op instr_r list

val reduce_wconst : wsize -> pexpr -> pexpr

val lower_cassgn :
  lowering_options -> (instr_info -> warning_msg -> instr_info) -> fresh_vars
  -> (var_i -> bool) -> instr_info -> lval -> assgn_tag -> stype -> pexpr ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op instr list

val lower_addcarry_classify :
  bool -> lval list -> pexpr list -> ((((var_info * (wsize ->
  x86_op)) * pexpr list) * lval) * lval) option

val lower_addcarry :
  fresh_vars -> wsize -> bool -> lval list -> assgn_tag -> pexpr list ->
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op instr_r list

val lower_mulu :
  fresh_vars -> wsize -> lval list -> assgn_tag -> pexpr list -> (register,
  register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op
  instr_r list

val lower_copn :
  fresh_vars -> lval list -> assgn_tag -> (register, register_ext,
  xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op sopn -> pexpr
  list -> (register, register_ext, xmm_register, rflag, condt, x86_op,
  x86_extra_op) extended_op instr_r list

val lower_i :
  lowering_options -> (instr_info -> warning_msg -> instr_info) -> fresh_vars
  -> (var_i -> bool) -> (register, register_ext, xmm_register, rflag, condt,
  x86_op, x86_extra_op) extended_op instr -> (register, register_ext,
  xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op instr list
