open BinInt
open BinNums
open BinPos
open Datatypes
open Compiler_util
open Constant_prop
open Eqtype
open Expr
open Label
open Linear
open Memory_model
open Seq
open Sopn
open Ssralg
open Ssrint
open Ssrnat
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize

module E :
 sig
  val pass_name : char list

  val my_error : pp_error -> pp_error_loc

  val gen_error : bool -> instr_info option -> char list -> pp_error_loc

  val ii_error : instr_info -> char list -> pp_error_loc

  val error : char list -> pp_error_loc

  val internal_error : char list -> pp_error_loc
 end

type 'asm_op linearization_params = { lip_tmp : Equality.sort;
                                      lip_allocate_stack_frame : (var_i ->
                                                                 coq_Z ->
                                                                 (lval
                                                                 list * 'asm_op
                                                                 sopn) * pexpr
                                                                 list);
                                      lip_free_stack_frame : (var_i -> coq_Z
                                                             -> (lval
                                                             list * 'asm_op
                                                             sopn) * pexpr
                                                             list);
                                      lip_ensure_rsp_alignment : (var_i ->
                                                                 wsize ->
                                                                 (lval
                                                                 list * 'asm_op
                                                                 sopn) * pexpr
                                                                 list);
                                      lip_lassign : (lval -> wsize -> pexpr
                                                    -> ((lval list * 'asm_op
                                                    sopn) * pexpr list)
                                                    option) }

val lassign :
  'a1 asmOp -> 'a1 linearization_params -> lval -> wsize -> pexpr -> 'a1
  linstr_r option

val lmove :
  'a1 asmOp -> 'a1 linearization_params -> var_i -> wsize -> gvar -> 'a1
  linstr_r option

val lload :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> var_i -> wsize
  -> var_i -> coq_Z -> 'a1 linstr_r option

val lstore :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> var_i -> coq_Z
  -> wsize -> gvar -> 'a1 linstr_r option

val mkli_dummy : 'a1 asmOp -> 'a1 linstr_r -> 'a1 linstr

val dummy_linstr : 'a1 asmOp -> 'a1 linstr

val of_olinstr_r :
  'a1 asmOp -> instr_info -> 'a1 linstr_r option -> 'a1 linstr

val stack_frame_allocation_size : stk_fun_extra -> coq_Z

val check_c :
  'a1 asmOp -> ('a1 instr -> unit cexec) -> 'a1 instr list -> unit cexec

val check_i :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (instr_info -> Var.var option) -> funname -> wsize -> 'a1 instr -> unit
  cexec

val all_disjoint_aligned_between :
  coq_PointerData -> coq_Z -> coq_Z -> wsize -> 'a1 list -> ('a1 ->
  (coq_Z * wsize) cexec) -> unit cexec

val check_to_save_slot :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (Var.var * coq_Z) -> (coq_Z * wsize) cexec

val check_to_save :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  stk_fun_extra -> unit cexec

val linear_c :
  'a1 asmOp -> ('a1 instr -> label -> 'a1 lcmd -> label * 'a1 lcmd) -> 'a1
  instr list -> label -> 'a1 lcmd -> label * 'a1 lcmd

val next_lbl : positive -> positive

val add_align :
  'a1 asmOp -> instr_info -> align -> 'a1 lcmd -> 'a1 linstr list

val align :
  'a1 asmOp -> instr_info -> align -> (label * 'a1 lcmd) -> label * 'a1 lcmd

val check_fd :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (instr_info -> Var.var option) -> funname -> 'a1 sfundef -> (pp_error_loc,
  unit) result

val check_prog :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (instr_info -> Var.var option) -> (pp_error_loc, unit) result

val allocate_stack_frame :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  bool -> instr_info -> coq_Z -> 'a1 lcmd

val ensure_rsp_alignment :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  instr_info -> wsize -> 'a1 linstr

val push_to_save :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  instr_info -> (Var.var * coq_Z) list -> 'a1 lcmd

val pop_to_save :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  instr_info -> (Var.var * coq_Z) list -> 'a1 lcmd

val linear_i :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (instr_info -> Var.var option) -> funname -> 'a1 instr -> label -> 'a1 lcmd
  -> label * 'a1 lcmd

val linear_body :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (instr_info -> Var.var option) -> funname -> stk_fun_extra -> 'a1 instr
  list -> label * 'a1 lcmd

val linear_fd :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (instr_info -> Var.var option) -> funname -> 'a1 sfundef -> label * 'a1
  lfundef

val linear_prog :
  coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
  (instr_info -> Var.var option) -> 'a1 lprog cexec
