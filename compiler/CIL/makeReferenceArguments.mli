open Datatypes
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Syscall
open Type
open Utils0
open Var0

module E :
 sig
  val pass : char list

  val make_ref_error : instr_info -> char list -> pp_error_loc
 end

val with_id :
  (Equality.sort -> stype -> Equality.sort) -> var_info -> Equality.sort ->
  stype -> var_i

val is_reg_ptr_expr :
  (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort) -> bool ->
  Equality.sort -> stype -> pexpr -> var_i option

val is_reg_ptr_lval :
  (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort) -> bool ->
  Equality.sort -> stype -> lval -> var_i option

val make_prologue :
  'a1 asmOp -> (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort)
  -> instr_info -> SvExtra.Sv.t -> ((bool * Equality.sort) * stype) list ->
  pexpr list -> (pp_error_loc, 'a1 instr list * pexpr list) result

type pseudo_instr =
| PI_lv of lval
| PI_i of lval * stype * var_i

val make_pseudo_epilogue :
  (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort) ->
  instr_info -> SvExtra.Sv.t -> ((bool * Equality.sort) * stype) list -> lval
  list -> (pp_error_loc, pseudo_instr list) result

val mk_ep_i : 'a1 asmOp -> instr_info -> lval -> stype -> var_i -> 'a1 instr

val noload : pexpr -> bool

val wf_lv : lval -> bool

val swapable :
  'a1 asmOp -> instr_info -> pseudo_instr list -> (pp_error_loc, lval
  list * 'a1 instr list) result

val make_epilogue :
  'a1 asmOp -> (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort)
  -> instr_info -> SvExtra.Sv.t -> ((bool * Equality.sort) * stype) list ->
  lval list -> (pp_error_loc, lval list * 'a1 instr list) result

val update_c :
  'a1 asmOp -> ('a1 instr -> 'a1 instr list cexec) -> 'a1 instr list ->
  (pp_error_loc, 'a1 instr list) result

val mk_info :
  (Var.var -> bool) -> var_i -> stype -> (bool * Equality.sort) * stype

val get_sig :
  'a1 asmOp -> (Var.var -> bool) -> 'a1 uprog -> funname ->
  ((bool * Equality.sort) * stype) list * ((bool * Equality.sort) * stype)
  list

val get_syscall_sig :
  BinNums.positive Syscall_t.syscall_t -> ((bool * char list) * stype)
  list * ((bool * char list) * stype) list

val update_i :
  'a1 asmOp -> (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort)
  -> 'a1 uprog -> SvExtra.Sv.t -> 'a1 instr -> 'a1 instr list cexec

val update_fd :
  'a1 asmOp -> (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort)
  -> 'a1 uprog -> 'a1 ufundef -> (pp_error_loc, ('a1, Equality.sort) _fundef)
  result

val makereference_prog :
  'a1 asmOp -> (Var.var -> bool) -> (Equality.sort -> stype -> Equality.sort)
  -> 'a1 uprog -> 'a1 uprog cexec
