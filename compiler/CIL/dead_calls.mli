open Datatypes
open Compiler_util
open Eqtype
open Expr
open Gen_map
open Seq
open Sopn
open Utils0

module E :
 sig
  val pass : char list

  val dead_calls_error : char list -> pp_error_loc
 end

val i_calls : 'a1 asmOp -> PosSet.Sp.t -> 'a1 instr -> PosSet.Sp.t

val c_calls : 'a1 asmOp -> PosSet.Sp.t -> 'a1 instr list -> PosSet.Sp.t

val live_calls :
  'a1 asmOp -> Equality.coq_type -> progT -> PosSet.Sp.t -> 'a1 fun_decl list
  -> PosSet.Sp.t

val dead_calls :
  'a1 asmOp -> Equality.coq_type -> progT -> PosSet.Sp.t -> 'a1 fun_decl list
  -> (PosSet.Sp.elt * 'a1 fundef) list

val dead_calls_err :
  'a1 asmOp -> Equality.coq_type -> progT -> PosSet.Sp.t -> 'a1 prog -> 'a1
  prog cexec

val dead_calls_err_seq :
  'a1 asmOp -> Equality.coq_type -> progT -> funname list -> 'a1 prog -> 'a1
  prog cexec
