open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Utils0
open Var0

val disj_fvars : SvExtra.Sv.t -> SvExtra.Sv.t -> bool

val fvars_correct :
  'a1 asmOp -> Equality.coq_type -> progT -> Equality.sort list ->
  SvExtra.Sv.t -> 'a1 fun_decl list -> bool

val is_lval_in_memory : (var_i -> bool) -> lval -> bool

val lower_cmd :
  'a1 asmOp -> ('a2 -> (instr_info -> warning_msg -> instr_info) -> 'a3 ->
  (var_i -> bool) -> 'a1 instr -> 'a1 instr list) -> 'a2 -> (instr_info ->
  warning_msg -> instr_info) -> 'a3 -> (var_i -> bool) -> 'a1 instr list ->
  'a1 instr list

val lower_fd :
  'a1 asmOp -> ('a2 -> (instr_info -> warning_msg -> instr_info) -> 'a3 ->
  (var_i -> bool) -> 'a1 instr -> 'a1 instr list) -> 'a2 -> (instr_info ->
  warning_msg -> instr_info) -> 'a3 -> Equality.coq_type -> progT -> (var_i
  -> bool) -> 'a1 fundef -> 'a1 fundef

val lower_prog :
  'a1 asmOp -> ('a2 -> (instr_info -> warning_msg -> instr_info) -> 'a3 ->
  (var_i -> bool) -> 'a1 instr -> 'a1 instr list) -> 'a2 -> (instr_info ->
  warning_msg -> instr_info) -> 'a3 -> Equality.coq_type -> progT -> (var_i
  -> bool) -> 'a1 prog -> 'a1 prog
