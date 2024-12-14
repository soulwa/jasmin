open BinInt
open BinNums
open Bool
open Arch_decl
open Eqtype
open Expr
open Fintype
open Flag_combination
open Seq
open Shift_kind
open Ssralg
open Ssrbool
open Utils0
open Word0
open Wsize

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type register =
| R00
| R01
| R02
| R03
| R04
| R05
| R06
| R07
| R08
| R09
| R10
| R11
| R12
| LR
| SP

(** val register_dec_eq : register -> register -> bool **)

let register_dec_eq r0 r1 =
  match r0 with
  | R00 -> (match r1 with
            | R00 -> true
            | _ -> false)
  | R01 -> (match r1 with
            | R01 -> true
            | _ -> false)
  | R02 -> (match r1 with
            | R02 -> true
            | _ -> false)
  | R03 -> (match r1 with
            | R03 -> true
            | _ -> false)
  | R04 -> (match r1 with
            | R04 -> true
            | _ -> false)
  | R05 -> (match r1 with
            | R05 -> true
            | _ -> false)
  | R06 -> (match r1 with
            | R06 -> true
            | _ -> false)
  | R07 -> (match r1 with
            | R07 -> true
            | _ -> false)
  | R08 -> (match r1 with
            | R08 -> true
            | _ -> false)
  | R09 -> (match r1 with
            | R09 -> true
            | _ -> false)
  | R10 -> (match r1 with
            | R10 -> true
            | _ -> false)
  | R11 -> (match r1 with
            | R11 -> true
            | _ -> false)
  | R12 -> (match r1 with
            | R12 -> true
            | _ -> false)
  | LR -> (match r1 with
           | LR -> true
           | _ -> false)
  | SP -> (match r1 with
           | SP -> true
           | _ -> false)

(** val register_beq : register -> register -> bool **)

let register_beq r0 r1 =
  if register_dec_eq r0 r1 then true else false

(** val register_eq_axiom : register Equality.axiom **)

let register_eq_axiom x y =
  iffP (register_beq x y) (if register_beq x y then ReflectT else ReflectF)

(** val eqTC_register : register eqTypeC **)

let eqTC_register =
  { beq = register_beq; ceqP = register_eq_axiom }

(** val arm_register_eqType : Equality.coq_type **)

let arm_register_eqType =
  ceqT_eqType eqTC_register

(** val registers : register list **)

let registers =
  R00 :: (R01 :: (R02 :: (R03 :: (R04 :: (R05 :: (R06 :: (R07 :: (R08 :: (R09 :: (R10 :: (R11 :: (R12 :: (LR :: (SP :: []))))))))))))))

(** val finTC_register : register finTypeC **)

let finTC_register =
  { _eqC = eqTC_register; cenum = registers }

(** val register_finType : Finite.coq_type **)

let register_finType =
  cfinT_finType finTC_register

(** val string_of_register : register -> char list **)

let string_of_register = function
| R00 -> 'r'::('0'::[])
| R01 -> 'r'::('1'::[])
| R02 -> 'r'::('2'::[])
| R03 -> 'r'::('3'::[])
| R04 -> 'r'::('4'::[])
| R05 -> 'r'::('5'::[])
| R06 -> 'r'::('6'::[])
| R07 -> 'r'::('7'::[])
| R08 -> 'r'::('8'::[])
| R09 -> 'r'::('9'::[])
| R10 -> 'r'::('1'::('0'::[]))
| R11 -> 'r'::('1'::('1'::[]))
| R12 -> 'r'::('1'::('2'::[]))
| LR -> 'l'::('r'::[])
| SP -> 's'::('p'::[])

(** val reg_toS : register coq_ToString **)

let reg_toS =
  { category = ('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))));
    _finC = finTC_register; to_string = string_of_register; strings =
    (map (fun x -> ((string_of_register (Obj.magic x)), (Obj.magic x)))
      (enum_mem
        (Finite.clone register_finType (Finite.coq_class register_finType))
        (mem predPredType
          (Obj.magic PredOfSimpl.coerce (coq_SimplPred (fun _ -> true)))))) }

(** val register_ext_dec_eq : __ -> bool **)

let register_ext_dec_eq _ =
  assert false (* absurd case *)

(** val register_ext_beq : __ -> bool **)

let register_ext_beq _ =
  if register_ext_dec_eq __ then true else false

(** val regx_eq_axiom : __ -> reflect **)

let regx_eq_axiom _ =
  iffP (register_ext_beq __)
    (if register_ext_beq __ then ReflectT else ReflectF)

(** val regx_eqMixin : __ Equality.mixin_of **)

let regx_eqMixin =
  { Equality.op = (fun _ -> register_ext_beq); Equality.mixin_of__1 =
    (fun _ -> regx_eq_axiom) }

(** val regx_eqType : Equality.coq_type **)

let regx_eqType =
  Obj.magic regx_eqMixin

(** val register_exts : __ list **)

let register_exts =
  []

(** val regx_choiceMixin : Equality.sort Choice.Choice.mixin_of **)

let regx_choiceMixin =
  Choice.coq_PcanChoiceMixin Choice.nat_choiceType
    (Obj.magic FinIsCount.pickle regx_eqType register_exts)
    (Obj.magic FinIsCount.unpickle regx_eqType register_exts)

(** val regx_choiceType : Choice.Choice.coq_type **)

let regx_choiceType =
  { Choice.Choice.base = (Equality.coq_class regx_eqType);
    Choice.Choice.mixin = regx_choiceMixin }

(** val regx_countMixin : Equality.sort Choice.Countable.mixin_of **)

let regx_countMixin =
  Choice.coq_PcanCountMixin Choice.nat_countType
    (Obj.magic FinIsCount.pickle regx_eqType register_exts)
    (Obj.magic FinIsCount.unpickle regx_eqType register_exts)

(** val regx_countType : Choice.Countable.coq_type **)

let regx_countType =
  { Choice.Countable.base = (Choice.Choice.coq_class regx_choiceType);
    Choice.Countable.mixin = regx_countMixin }

(** val regx_finMixin : Finite.mixin_of **)

let regx_finMixin =
  Finite.coq_EnumMixin regx_countType (Obj.magic register_exts)

(** val regx_finType : Finite.coq_type **)

let regx_finType =
  { Finite.base = (Choice.Choice.coq_class regx_choiceType); Finite.mixin =
    regx_finMixin }

(** val string_of_register_ext : __ -> char list **)

let string_of_register_ext _ =
  assert false (* absurd case *)

(** val eqTC_register_ext : __ eqTypeC **)

let eqTC_register_ext =
  { beq = (fun _ -> register_ext_beq); ceqP = (fun _ -> regx_eq_axiom) }

(** val finC_register_ext : __ finTypeC **)

let finC_register_ext =
  { _eqC = eqTC_register_ext; cenum = register_exts }

(** val regx_toS : __ coq_ToString **)

let regx_toS =
  { category = ('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))));
    _finC = finC_register_ext; to_string = string_of_register_ext; strings =
    (map (Obj.magic (fun _ -> ((string_of_register_ext __), __)))
      (enum_mem (Finite.clone regx_finType (Finite.coq_class regx_finType))
        (mem predPredType
          (Obj.magic PredOfSimpl.coerce (coq_SimplPred (fun _ -> true)))))) }

(** val xregister_dec_eq : __ -> bool **)

let xregister_dec_eq _ =
  assert false (* absurd case *)

(** val xregister_beq : __ -> bool **)

let xregister_beq _ =
  if xregister_dec_eq __ then true else false

(** val xregister_eq_axiom : __ -> reflect **)

let xregister_eq_axiom _ =
  iffP (xregister_beq __) (if xregister_beq __ then ReflectT else ReflectF)

(** val eqTC_xregister : __ eqTypeC **)

let eqTC_xregister =
  { beq = (fun _ -> xregister_beq); ceqP = (fun _ -> xregister_eq_axiom) }

(** val xregister_eqType : Equality.coq_type **)

let xregister_eqType =
  ceqT_eqType eqTC_xregister

(** val xregisters : __ list **)

let xregisters =
  []

(** val finTC_xregister : __ finTypeC **)

let finTC_xregister =
  { _eqC = eqTC_xregister; cenum = xregisters }

(** val xregister_finType : Finite.coq_type **)

let xregister_finType =
  cfinT_finType finTC_xregister

(** val string_of_xregister : __ -> char list **)

let string_of_xregister _ =
  assert false (* absurd case *)

(** val xreg_toS : __ coq_ToString **)

let xreg_toS =
  { category =
    ('x'::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[])))))))));
    _finC = finTC_xregister; to_string = string_of_xregister; strings =
    (map (Obj.magic (fun _ -> ((string_of_xregister __), __)))
      (enum_mem
        (Finite.clone xregister_finType (Finite.coq_class xregister_finType))
        (mem predPredType
          (Obj.magic PredOfSimpl.coerce (coq_SimplPred (fun _ -> true)))))) }

type rflag =
| NF
| ZF
| CF
| VF

(** val rflag_dec_eq : rflag -> rflag -> bool **)

let rflag_dec_eq f0 f1 =
  match f0 with
  | NF -> (match f1 with
           | NF -> true
           | _ -> false)
  | ZF -> (match f1 with
           | ZF -> true
           | _ -> false)
  | CF -> (match f1 with
           | CF -> true
           | _ -> false)
  | VF -> (match f1 with
           | VF -> true
           | _ -> false)

(** val rflag_beq : rflag -> rflag -> bool **)

let rflag_beq f0 f1 =
  if rflag_dec_eq f0 f1 then true else false

(** val rflag_eq_axiom : rflag Equality.axiom **)

let rflag_eq_axiom x y =
  iffP (rflag_beq x y) (if rflag_beq x y then ReflectT else ReflectF)

(** val eqTC_rflag : rflag eqTypeC **)

let eqTC_rflag =
  { beq = rflag_beq; ceqP = rflag_eq_axiom }

(** val rflag_eqType : Equality.coq_type **)

let rflag_eqType =
  ceqT_eqType eqTC_rflag

(** val rflags : rflag list **)

let rflags =
  NF :: (ZF :: (CF :: (VF :: [])))

(** val finTC_rflag : rflag finTypeC **)

let finTC_rflag =
  { _eqC = eqTC_rflag; cenum = rflags }

(** val rflag_finType : Finite.coq_type **)

let rflag_finType =
  cfinT_finType finTC_rflag

(** val string_of_rflag : rflag -> char list **)

let string_of_rflag = function
| NF -> 'N'::('F'::[])
| ZF -> 'Z'::('F'::[])
| CF -> 'C'::('F'::[])
| VF -> 'V'::('F'::[])

(** val rflag_toS : rflag coq_ToString **)

let rflag_toS =
  { category = ('r'::('f'::('l'::('a'::('g'::[]))))); _finC = finTC_rflag;
    to_string = string_of_rflag; strings =
    (map (fun x -> ((string_of_rflag (Obj.magic x)), (Obj.magic x)))
      (enum_mem (Finite.clone rflag_finType (Finite.coq_class rflag_finType))
        (mem predPredType
          (Obj.magic PredOfSimpl.coerce (coq_SimplPred (fun _ -> true)))))) }

type condt =
| EQ_ct
| NE_ct
| CS_ct
| CC_ct
| MI_ct
| PL_ct
| VS_ct
| VC_ct
| HI_ct
| LS_ct
| GE_ct
| LT_ct
| GT_ct
| LE_ct

(** val condt_dec_eq : condt -> condt -> bool **)

let condt_dec_eq c0 c1 =
  match c0 with
  | EQ_ct -> (match c1 with
              | EQ_ct -> true
              | _ -> false)
  | NE_ct -> (match c1 with
              | NE_ct -> true
              | _ -> false)
  | CS_ct -> (match c1 with
              | CS_ct -> true
              | _ -> false)
  | CC_ct -> (match c1 with
              | CC_ct -> true
              | _ -> false)
  | MI_ct -> (match c1 with
              | MI_ct -> true
              | _ -> false)
  | PL_ct -> (match c1 with
              | PL_ct -> true
              | _ -> false)
  | VS_ct -> (match c1 with
              | VS_ct -> true
              | _ -> false)
  | VC_ct -> (match c1 with
              | VC_ct -> true
              | _ -> false)
  | HI_ct -> (match c1 with
              | HI_ct -> true
              | _ -> false)
  | LS_ct -> (match c1 with
              | LS_ct -> true
              | _ -> false)
  | GE_ct -> (match c1 with
              | GE_ct -> true
              | _ -> false)
  | LT_ct -> (match c1 with
              | LT_ct -> true
              | _ -> false)
  | GT_ct -> (match c1 with
              | GT_ct -> true
              | _ -> false)
  | LE_ct -> (match c1 with
              | LE_ct -> true
              | _ -> false)

(** val condt_beq : condt -> condt -> bool **)

let condt_beq c0 c1 =
  if condt_dec_eq c0 c1 then true else false

(** val condt_eq_axiom : condt Equality.axiom **)

let condt_eq_axiom x y =
  iffP (condt_beq x y) (if condt_beq x y then ReflectT else ReflectF)

(** val eqTC_condt : condt eqTypeC **)

let eqTC_condt =
  { beq = condt_beq; ceqP = condt_eq_axiom }

(** val condt_eqType : Equality.coq_type **)

let condt_eqType =
  ceqT_eqType eqTC_condt

(** val condts : condt list **)

let condts =
  EQ_ct :: (NE_ct :: (CS_ct :: (CC_ct :: (MI_ct :: (PL_ct :: (VS_ct :: (VC_ct :: (HI_ct :: (LS_ct :: (GE_ct :: (LT_ct :: (GT_ct :: (LE_ct :: [])))))))))))))

(** val finTC_condt : condt finTypeC **)

let finTC_condt =
  { _eqC = eqTC_condt; cenum = condts }

(** val condt_finType : Finite.coq_type **)

let condt_finType =
  cfinT_finType finTC_condt

(** val string_of_condt : condt -> char list **)

let string_of_condt = function
| EQ_ct -> 'e'::('q'::[])
| NE_ct -> 'n'::('e'::[])
| CS_ct -> 'c'::('s'::[])
| CC_ct -> 'c'::('c'::[])
| MI_ct -> 'm'::('i'::[])
| PL_ct -> 'p'::('l'::[])
| VS_ct -> 'v'::('s'::[])
| VC_ct -> 'v'::('c'::[])
| HI_ct -> 'h'::('i'::[])
| LS_ct -> 'l'::('s'::[])
| GE_ct -> 'g'::('e'::[])
| LT_ct -> 'l'::('t'::[])
| GT_ct -> 'g'::('t'::[])
| LE_ct -> 'l'::('e'::[])

(** val shift_kind_dec_eq : shift_kind -> shift_kind -> bool **)

let shift_kind_dec_eq sk0 sk1 =
  match sk0 with
  | SLSL -> (match sk1 with
             | SLSL -> true
             | _ -> false)
  | SLSR -> (match sk1 with
             | SLSR -> true
             | _ -> false)
  | SASR -> (match sk1 with
             | SASR -> true
             | _ -> false)
  | SROR -> (match sk1 with
             | SROR -> true
             | _ -> false)

(** val shift_kind_beq : shift_kind -> shift_kind -> bool **)

let shift_kind_beq sk0 sk1 =
  if shift_kind_dec_eq sk0 sk1 then true else false

(** val shift_kind_eq_axiom : shift_kind Equality.axiom **)

let shift_kind_eq_axiom sk0 sk1 =
  iffP (shift_kind_beq sk0 sk1)
    (if shift_kind_beq sk0 sk1 then ReflectT else ReflectF)

(** val eqTC_shift_kind : shift_kind eqTypeC **)

let eqTC_shift_kind =
  { beq = shift_kind_beq; ceqP = shift_kind_eq_axiom }

(** val shift_kind_eqType : Equality.coq_type **)

let shift_kind_eqType =
  ceqT_eqType eqTC_shift_kind

(** val shift_kinds : shift_kind list **)

let shift_kinds =
  SLSL :: (SLSR :: (SASR :: (SROR :: [])))

(** val finTC_shift_kind : shift_kind finTypeC **)

let finTC_shift_kind =
  { _eqC = eqTC_shift_kind; cenum = shift_kinds }

(** val shift_kind_finType : Finite.coq_type **)

let shift_kind_finType =
  cfinT_finType finTC_shift_kind

(** val string_of_shift_kind : shift_kind -> char list **)

let string_of_shift_kind = function
| SLSL -> 'l'::('s'::('l'::[]))
| SLSR -> 'l'::('s'::('r'::[]))
| SASR -> 'a'::('s'::('r'::[]))
| SROR -> 'r'::('o'::('r'::[]))

(** val shift_kind_toS : shift_kind coq_ToString **)

let shift_kind_toS =
  { category = ('s'::('h'::('i'::('f'::('t'::[]))))); _finC =
    finTC_shift_kind; to_string = string_of_shift_kind; strings =
    (map (fun x -> ((string_of_shift_kind (Obj.magic x)), (Obj.magic x)))
      (enum_mem
        (Finite.clone shift_kind_finType
          (Finite.coq_class shift_kind_finType))
        (mem predPredType
          (Obj.magic PredOfSimpl.coerce (coq_SimplPred (fun _ -> true)))))) }

(** val check_shift_amount : shift_kind -> coq_Z -> bool **)

let check_shift_amount sk z =
  match sk with
  | SLSL ->
    (&&) (Z.leb Z0 z)
      (Z.leb z (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))
  | _ ->
    (&&) (Z.leb (Zpos Coq_xH) z)
      (Z.leb z (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH))))))

(** val shift_op :
    shift_kind -> wsize -> GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort **)

let shift_op = function
| SLSL -> wshl
| SLSR -> wshr
| SASR -> wsar
| SROR -> wror

(** val shift_of_sop2 : wsize -> sop2 -> shift_kind option **)

let shift_of_sop2 ws op0 =
  match ws with
  | U32 ->
    (match op0 with
     | Olsr w -> (match w with
                  | U32 -> Some SLSR
                  | _ -> None)
     | Olsl o ->
       (match o with
        | Op_int -> None
        | Op_w w -> (match w with
                     | U32 -> Some SLSL
                     | _ -> None))
     | Oasr o ->
       (match o with
        | Op_int -> None
        | Op_w w -> (match w with
                     | U32 -> Some SASR
                     | _ -> None))
     | Oror w -> (match w with
                  | U32 -> Some SROR
                  | _ -> None)
     | _ -> None)
  | _ -> None

(** val arm_fc_of_cfc : combine_flags_core -> flag_combination **)

let arm_fc_of_cfc cfc =
  let vnf = FCVar0 in
  let vzf = FCVar1 in
  let vcf = FCVar2 in
  let vvf = FCVar3 in
  (match cfc with
   | CFC_O -> vvf
   | CFC_B -> FCNot vcf
   | CFC_E -> vzf
   | CFC_S -> vnf
   | CFC_L -> FCNot (FCEq (vnf, vvf))
   | CFC_BE -> FCOr ((FCNot vcf), vzf)
   | CFC_LE -> FCOr (vzf, (FCNot (FCEq (vnf, vvf)))))

(** val arm_fcp : coq_FlagCombinationParams **)

let arm_fcp =
  arm_fc_of_cfc

(** val arm_decl : (register, __, __, rflag, condt) arch_decl **)

let arm_decl =
  { reg_size = U32; xreg_size = U64; cond_eqC = eqTC_condt; toS_r = reg_toS;
    toS_rx = regx_toS; toS_x = xreg_toS; toS_f = rflag_toS; ad_rsp = SP;
    ad_fcp = arm_fcp }

(** val arm_linux_call_conv :
    (register, __, __, rflag, condt) calling_convention **)

let arm_linux_call_conv =
  { callee_saved =
    (map (fun x -> ARReg x)
      (R04 :: (R05 :: (R06 :: (R07 :: (R08 :: (R09 :: (R10 :: (R11 :: [])))))))));
    call_reg_args = (R00 :: (R01 :: (R02 :: (R03 :: [])))); call_xreg_args =
    []; call_reg_ret = (R00 :: (R01 :: [])); call_xreg_ret = [] }
