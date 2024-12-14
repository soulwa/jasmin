open BinInt
open BinNums
open Bool
open Datatypes
open Eqtype
open Sem_type
open Seq
open Shift_kind
open Ssrbool
open Type
open Utils0
open Var0
open Warray_
open Word0
open Wsize

type implicit_arg =
| IArflag of Var.var
| IAreg of Var.var

type arg_desc =
| ADImplicit of implicit_arg
| ADExplicit of nat * Var.var option

type instruction_desc = { str : (unit -> char list); tin : stype list;
                          i_in : arg_desc list; tout : stype list;
                          i_out : arg_desc list;
                          semi : sem_tuple exec sem_prod;
                          i_safe : safe_cond list }

val str : instruction_desc -> unit -> char list

val tin : instruction_desc -> stype list

val i_in : instruction_desc -> arg_desc list

val tout : instruction_desc -> stype list

val i_out : instruction_desc -> arg_desc list

val semi : instruction_desc -> sem_tuple exec sem_prod

val i_safe : instruction_desc -> safe_cond list

type 'asm_op prim_constructor =
| PrimP of wsize * (wsize option -> wsize -> 'asm_op)
| PrimM of (wsize option -> 'asm_op)
| PrimV of (wsize option -> signedness -> velem -> wsize -> 'asm_op)
| PrimX of (wsize option -> wsize -> wsize -> 'asm_op)
| PrimVV of (wsize option -> velem -> wsize -> velem -> wsize -> 'asm_op)
| PrimARM of (bool -> bool -> shift_kind option -> 'asm_op)

type 'asm_op asmOp = { _eqT : 'asm_op eqTypeC;
                       asm_op_instr : ('asm_op -> instruction_desc);
                       prim_string : (char list * 'asm_op prim_constructor)
                                     list }

val _eqT : 'a1 asmOp -> 'a1 eqTypeC

val asm_op_instr : 'a1 asmOp -> 'a1 -> instruction_desc

val prim_string : 'a1 asmOp -> (char list * 'a1 prim_constructor) list

type 'asm_op asm_op_t = 'asm_op

type 'asm_op sopn =
| Ocopy of wsize * positive
| Onop
| Omulu of wsize
| Oaddcarry of wsize
| Osubcarry of wsize
| Oasm of 'asm_op asm_op_t

val sopn_beq : 'a1 asmOp -> 'a1 sopn -> 'a1 sopn -> bool

val sopn_eq_axiom : 'a1 asmOp -> 'a1 sopn Equality.axiom

val sopn_eqMixin : 'a1 asmOp -> 'a1 sopn Equality.mixin_of

val sopn_eqType : 'a1 asmOp -> Equality.coq_type

val coq_Ocopy_instr : wsize -> positive -> instruction_desc

val coq_Onop_instr : instruction_desc

val coq_Omulu_instr : wsize -> instruction_desc

val coq_Oaddcarry_instr : wsize -> instruction_desc

val coq_Osubcarry_instr : wsize -> instruction_desc

val get_instr_desc : 'a1 asmOp -> 'a1 sopn -> instruction_desc

val string_of_sopn : 'a1 asmOp -> 'a1 sopn -> char list

val sopn_tin : 'a1 asmOp -> 'a1 sopn -> stype list

val sopn_tout : 'a1 asmOp -> 'a1 sopn -> stype list

val sopn_sem : 'a1 asmOp -> 'a1 sopn -> sem_tuple exec sem_prod

val eqC_sopn : 'a1 asmOp -> 'a1 sopn eqTypeC

val map_prim_constructor :
  ('a1 -> 'a2) -> 'a1 prim_constructor -> 'a2 prim_constructor

val sopn_prim_string :
  coq_PointerData -> 'a1 asmOp -> (char list * 'a1 sopn prim_constructor) list

val asmOp_sopn : coq_PointerData -> 'a1 asmOp -> 'a1 sopn asmOp
