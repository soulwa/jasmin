open BinInt
open BinNums
open Datatypes
open Eqtype
open Expr
open Flag_combination
open Global
open Low_memory
open Memory_model
open Sem_op_typed
open Sem_pexpr_params
open Sem_type
open Sopn
open Ssralg
open Ssrbool
open Ssreflect
open Syscall
open Type
open Utils0
open Values
open Var0
open Warray_
open Word0
open Wsize
open Xseq

type __ = Obj.t

val undef_addr : stype -> sem_t exec

val vmap0 : sem_t exec Fv.t

val on_vu : ('a1 -> 'a2) -> 'a2 exec -> 'a1 exec -> 'a2 exec

val on_vuP :
  ('a1 -> 'a2) -> 'a2 exec -> 'a1 exec -> 'a2 -> ('a1 -> __ -> __ -> 'a3) ->
  (__ -> __ -> 'a3) -> 'a3

val get_var : sem_t exec Fv.t -> Var.var -> value exec

val set_var : sem_t exec Fv.t -> Var.var -> value -> sem_t exec Fv.t exec

val set_varP :
  sem_t exec Fv.t -> sem_t exec Fv.t -> Var.var -> value -> (sem_t -> __ ->
  __ -> 'a1) -> (__ -> __ -> __ -> 'a1) -> 'a1

val sem_sop1 : sop1 -> value -> value exec

val sem_sop2 : sop2 -> value -> value -> value exec

val sem_opN : coq_FlagCombinationParams -> opN -> values -> value exec

type 'syscall_state estate = { escs : 'syscall_state syscall_state_t;
                               emem : Memory.mem; evm : sem_t exec Fv.t }

val escs :
  coq_PointerData -> 'a1 syscall_sem -> 'a1 estate -> 'a1 syscall_state_t

val emem : coq_PointerData -> 'a1 syscall_sem -> 'a1 estate -> Memory.mem

val evm : coq_PointerData -> 'a1 syscall_sem -> 'a1 estate -> sem_t exec Fv.t

val get_global_value : glob_decl list -> Var.var -> glob_value option

val gv2val : glob_value -> value

val get_global : glob_decl list -> Var.var -> value exec

val get_gvar : glob_decl list -> sem_t exec Fv.t -> gvar -> value exec

val on_arr_var :
  value exec -> (positive -> WArray.array -> 'a1 exec) -> (error, 'a1) result

val on_arr_varP :
  coq_PointerData -> 'a1 syscall_sem -> (positive -> WArray.array -> 'a2
  exec) -> 'a2 -> 'a1 estate -> Var.var -> (positive -> WArray.array -> __ ->
  __ -> __ -> 'a3) -> 'a3

val on_arr_gvarP :
  (positive -> WArray.array -> 'a1 exec) -> 'a1 -> glob_decl list -> sem_t
  exec Fv.t -> gvar -> (positive -> WArray.array -> __ -> __ -> __ -> 'a2) ->
  'a2

val sem_pexpr :
  ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a2 estate -> pexpr ->
  value exec

val sem_pexprs :
  ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a2 estate -> pexpr list
  -> (error, value list) result

val write_var :
  ('a1, 'a2) coq_SemPexprParams -> var_i -> value -> 'a2 estate -> 'a2 estate
  exec

val write_vars :
  ('a1, 'a2) coq_SemPexprParams -> var_i list -> value list -> 'a2 estate ->
  (error, 'a2 estate) result

val write_none :
  ('a1, 'a2) coq_SemPexprParams -> 'a2 estate -> Equality.sort -> value ->
  'a2 estate exec

val write_lval :
  ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> lval -> value -> 'a2
  estate -> 'a2 estate exec

val write_lvals :
  ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a2 estate -> lval list
  -> value list -> (error, 'a2 estate) result

val exec_sopn :
  ('a1, 'a2) coq_SemPexprParams -> 'a1 sopn -> values -> values exec

val sem_sopn :
  ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a1 sopn -> 'a2 estate
  -> lval list -> pexpr list -> (error, 'a2 estate) result

val exec_getrandom :
  ('a1, 'a2) coq_SemPexprParams -> 'a2 syscall_state_t -> positive -> value
  list -> (error, 'a2 syscall_state_t * value list) result

val exec_syscall :
  ('a1, 'a2) coq_SemPexprParams -> 'a2 syscall_state_t -> Memory.mem ->
  BinNums.positive Syscall_t.syscall_t -> values -> (('a2
  syscall_state_t * Memory.mem) * values) exec
