open Bool
open Arch_decl
open Arch_extra
open Arm
open Arm_decl
open Arm_instr_decl
open Compiler_util
open Eqtype
open Expr
open Sopn
open Ssrbool
open Utils0

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val arm_extra_op_beq : __ -> bool **)

let arm_extra_op_beq _ =
  assert false (* absurd case *)

(** val arm_extra_op_eq_dec : __ -> bool **)

let arm_extra_op_eq_dec _ =
  let b = arm_extra_op_beq __ in if b then true else false

(** val arm_extra_op_eq_axiom : __ -> reflect **)

let arm_extra_op_eq_axiom _ =
  iffP (arm_extra_op_beq __)
    (if arm_extra_op_beq __ then ReflectT else ReflectF)

(** val arm_extra_op_eqMixin : __ Equality.mixin_of **)

let arm_extra_op_eqMixin =
  { Equality.op = (fun _ -> arm_extra_op_beq); Equality.mixin_of__1 =
    (fun _ -> arm_extra_op_eq_axiom) }

(** val arm_extra_op_eqType : Equality.coq_type **)

let arm_extra_op_eqType =
  Obj.magic arm_extra_op_eqMixin

(** val eqTC_arm_extra_op : __ eqTypeC **)

let eqTC_arm_extra_op =
  { beq = (fun _ -> arm_extra_op_beq); ceqP = (fun _ ->
    arm_extra_op_eq_axiom) }

(** val get_instr_desc : __ -> instruction_desc **)

let get_instr_desc _ =
  assert false (* absurd case *)

(** val arm_extra_op_decl : __ asmOp **)

let arm_extra_op_decl =
  { _eqT = eqTC_arm_extra_op; asm_op_instr = get_instr_desc; prim_string =
    [] }

(** val assemble_extra :
    instr_info -> lval list -> pexpr list -> (((register, __, __, rflag,
    condt, arm_op) asm_op_msb_t * lval list) * pexpr list) cexec **)

let assemble_extra _ _ _ =
  assert false (* absurd case *)

(** val arm_extra : (register, __, __, rflag, condt, arm_op, __) asm_extra **)

let arm_extra =
  { _asm = arm; _extra = arm_extra_op_decl; to_asm = (fun x _ ->
    assemble_extra x) }

type arm_extended_op =
  (register, __, __, rflag, condt, arm_op, __) extended_op

(** val coq_Oarm : arm_op -> arm_extended_op sopn **)

let coq_Oarm o =
  Oasm (BaseOp (None, o))
