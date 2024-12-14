open Datatypes
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Type
open Var0

val is_array_init : pexpr -> bool

val remove_init_i :
  'a1 asmOp -> (Var.var -> bool) -> 'a1 instr -> 'a1 instr list

val remove_init_c :
  'a1 asmOp -> (Var.var -> bool) -> 'a1 instr list -> 'a1 instr list

val remove_init_fd :
  'a1 asmOp -> (Var.var -> bool) -> Equality.coq_type -> progT -> 'a1 fundef
  -> ('a1, Equality.sort) _fundef

val remove_init_prog :
  'a1 asmOp -> (Var.var -> bool) -> Equality.coq_type -> progT -> 'a1 prog ->
  'a1 prog

val add_init_c :
  'a1 asmOp -> (SvExtra.Sv.t -> 'a1 instr -> 'a1 instr list * SvExtra.Sv.t)
  -> SvExtra.Sv.t -> 'a1 instr list -> 'a1 instr list * SvExtra.Sv.t

val add_init_aux :
  'a1 asmOp -> (Var.var -> bool) -> instr_info -> Var.var -> 'a1 instr list
  -> 'a1 instr list

val add_init :
  'a1 asmOp -> (Var.var -> bool) -> instr_info -> SvExtra.Sv.t ->
  SvExtra.Sv.t -> 'a1 instr -> 'a1 instr list

val add_init_i :
  'a1 asmOp -> (Var.var -> bool) -> SvExtra.Sv.t -> 'a1 instr -> 'a1 instr
  list * SvExtra.Sv.t

val add_init_fd :
  'a1 asmOp -> (Var.var -> bool) -> Equality.coq_type -> progT -> 'a1 fundef
  -> ('a1, Equality.sort) _fundef

val add_init_prog :
  'a1 asmOp -> (Var.var -> bool) -> Equality.coq_type -> progT -> 'a1 prog ->
  'a1 prog
