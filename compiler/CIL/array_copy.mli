open BinInt
open BinNums
open Datatypes
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Type
open Utils0
open Var0
open Warray_
open Word0
open Wsize

module E :
 sig
  val pass : char list

  val error : pp_error_loc
 end

val array_copy :
  'a1 asmOp -> Equality.sort -> instr_info -> var_i -> wsize -> positive ->
  gvar -> 'a1 instr list

val array_copy_c :
  'a1 asmOp -> ('a1 instr -> 'a1 instr list cexec) -> 'a1 instr list -> 'a1
  instr list cexec

val is_copy : 'a1 asmOp -> 'a1 sopn -> (wsize * positive) option

val is_Pvar : pexpr list -> gvar option

val is_Lvar : lval list -> var_i option

val array_copy_i :
  'a1 asmOp -> Equality.sort -> 'a1 instr -> 'a1 instr list cexec

val array_copy_fd :
  'a1 asmOp -> Equality.sort -> Equality.coq_type -> progT -> 'a1 fundef ->
  (pp_error_loc, ('a1, Equality.sort) _fundef) result

val array_copy_prog :
  'a1 asmOp -> Equality.sort -> Equality.coq_type -> progT -> 'a1 prog ->
  (pp_error_loc, ('a1, Equality.sort, extra_prog_t) _prog) result
