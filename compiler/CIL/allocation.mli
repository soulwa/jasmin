open Datatypes
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Ssrfun
open Syscall
open Type
open Utils0
open Var0
open Warray_
open Word_ssrZ
open Wsize

module E :
 sig
  val pass_name : char list

  val gen_error : bool -> instr_info option -> char list -> pp_error_loc

  val error : char list -> pp_error_loc

  val loop_iterator : pp_error_loc

  val fold2 : pp_error_loc
 end

val wextend_type : Equality.sort -> Equality.sort -> bool

module M :
 sig
  module Mv :
   sig
    val oget : SvExtra.Sv.t Mvar.t -> Equality.sort -> SvExtra.Sv.t

    type t_ = { mvar : Var.var Mvar.t; mid : SvExtra.Sv.t Mvar.t }

    val mvar : t_ -> Var.var Mvar.t

    val mid : t_ -> SvExtra.Sv.t Mvar.t

    type t = t_

    val get : t -> Var.var -> Var.var option

    val rm_id : t -> Equality.sort -> Var.var Mvar.t

    val ms_upd :
      SvExtra.Sv.t Mvar.t -> (SvExtra.Sv.t -> SvExtra.Sv.t) -> Equality.sort
      -> SvExtra.Sv.t Mvar.Map.t

    val rm_x : t -> Equality.sort -> SvExtra.Sv.t Mvar.Map.t

    val remove : t -> Equality.sort -> t_

    val set : t -> Equality.sort -> Equality.sort -> t_

    val add : t_ -> Equality.sort -> Var.var -> t_

    val empty : t_
   end

  val bool_dec : bool -> bool

  val v_wextendty : Var.var -> Var.var -> bool

  val v_wextendtyP : Var.var -> Var.var -> bool

  type t_ = { mv : Mv.t; mset : SvExtra.Sv.t }

  val mv : t_ -> Mv.t

  val mset : t_ -> SvExtra.Sv.t

  type t = t_

  val get : t -> Var.var -> Var.var option

  val set : t_ -> Var.var -> Var.var -> t_

  val add : t_ -> Var.var -> Var.var -> t_

  val addc : t_ -> Var.var -> Var.var -> t_

  val empty_s : SvExtra.Sv.t -> t_

  val empty : t_

  val merge_aux : t_ -> t_ -> Equality.sort Mvar.t

  val merge : t_ -> t_ -> t_

  val remove : t_ -> Equality.sort -> t_

  val incl : t_ -> t_ -> bool
 end

val alloc_error : char list -> pp_error_loc

val cerr_varalloc : Var.var -> Var.var -> char list -> pp_error_loc

val check_v : var_i -> var_i -> M.t -> M.t cexec

val error_e : pp_error_loc

val check_gv : gvar -> gvar -> M.t -> M.t cexec

val check_e : pexpr -> pexpr -> M.t -> M.t cexec

val check_var_aux : Var.var -> Var.var -> M.t_ -> M.t cexec

val check_varc : var_i -> var_i -> M.t_ -> M.t cexec

val is_Pvar : (stype * pexpr) option -> (stype * var_i) option

val error_lv : pp_error_loc

val check_lval : (stype * pexpr) option -> lval -> lval -> M.t -> M.t cexec

val loop : (M.t -> M.t cexec) -> nat -> M.t -> (pp_error_loc, M.t) result

val loop2 :
  (M.t -> (M.t * M.t) cexec) -> nat -> M.t -> (pp_error_loc, M.t) result

val check_es : pexpr list -> pexpr list -> M.t -> (pp_error_loc, M.t) result

val check_lvals : lval list -> lval list -> M.t -> (pp_error_loc, M.t) result

val check_var : var_i -> var_i -> M.t -> M.t cexec

val check_vars : var_i list -> var_i list -> M.t -> (pp_error_loc, M.t) result

val check_I :
  'a1 asmOp -> 'a1 instr -> 'a1 instr -> M.t_ -> (pp_error_loc, M.t_) result

val check_cmd :
  'a1 asmOp -> 'a1 instr list -> 'a1 instr list -> M.t_ -> (pp_error_loc,
  M.t_) result

val check_fundef :
  'a1 asmOp -> Equality.coq_type -> progT -> (Equality.sort -> extra_prog_t
  -> extra_prog_t -> M.t cexec) -> extra_prog_t -> extra_prog_t ->
  (funname * 'a1 fundef) -> (funname * 'a1 fundef) -> unit -> unit cexec

val check_prog_error : pp_error_loc

val check_prog :
  'a1 asmOp -> Equality.coq_type -> progT -> (Equality.sort -> extra_prog_t
  -> extra_prog_t -> M.t cexec) -> extra_prog_t -> (funname * 'a1 fundef)
  list -> extra_prog_t -> (funname * 'a1 fundef) list -> (pp_error_loc, unit)
  result

val init_alloc_uprog :
  Equality.sort -> extra_prog_t -> extra_prog_t -> M.t cexec

val check_ufundef :
  'a1 asmOp -> extra_prog_t -> extra_prog_t -> (funname * 'a1 fundef) ->
  (funname * 'a1 fundef) -> unit -> unit cexec

val check_uprog :
  'a1 asmOp -> extra_prog_t -> (funname * 'a1 fundef) list -> extra_prog_t ->
  (funname * 'a1 fundef) list -> (pp_error_loc, unit) result

val init_alloc_sprog :
  coq_PointerData -> Equality.sort -> extra_prog_t -> extra_prog_t -> M.t
  cexec

val check_sprog :
  'a1 asmOp -> coq_PointerData -> extra_prog_t -> (funname * 'a1 fundef) list
  -> extra_prog_t -> (funname * 'a1 fundef) list -> (pp_error_loc, unit)
  result
