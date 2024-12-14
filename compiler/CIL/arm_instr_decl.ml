open BinInt
open BinNums
open Bool
open Datatypes
open Arch_decl
open Arm_decl
open Eqtype
open Fintype
open Sem_type
open Seq
open Shift_kind
open Ssralg
open Ssrbool
open Ssrnat
open Type
open Utils0
open Word0
open Word_ssrZ
open Wsize
open Xseq

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type arm_options = { set_flags : bool; is_conditional : bool;
                     has_shift : shift_kind option }

(** val set_flags : arm_options -> bool **)

let set_flags a =
  a.set_flags

(** val is_conditional : arm_options -> bool **)

let is_conditional a =
  a.is_conditional

(** val has_shift : arm_options -> shift_kind option **)

let has_shift a =
  a.has_shift

(** val arm_options_beq : arm_options -> arm_options -> bool **)

let arm_options_beq ao0 ao1 =
  (&&)
    (eq_op bool_eqType (Obj.magic ao0.set_flags) (Obj.magic ao1.set_flags))
    ((&&)
      (eq_op bool_eqType (Obj.magic ao0.is_conditional)
        (Obj.magic ao1.is_conditional))
      (eq_op (option_eqType shift_kind_eqType) (Obj.magic ao0.has_shift)
        (Obj.magic ao1.has_shift)))

(** val arm_options_eq_axiom : arm_options Equality.axiom **)

let arm_options_eq_axiom __top_assumption_ =
  let _evar_0_ =
    fun _set_flags_ _is_conditional_ _has_shift_ __top_assumption_0 ->
    let _evar_0_ = fun _set_flags1_ _is_conditional1_ _has_shift1_ ->
      iffP
        (arm_options_beq { set_flags = _set_flags_; is_conditional =
          _is_conditional_; has_shift = _has_shift_ } { set_flags =
          _set_flags1_; is_conditional = _is_conditional1_; has_shift =
          _has_shift1_ })
        (if arm_options_beq { set_flags = _set_flags_; is_conditional =
              _is_conditional_; has_shift = _has_shift_ } { set_flags =
              _set_flags1_; is_conditional = _is_conditional1_; has_shift =
              _has_shift1_ }
         then ReflectT
         else ReflectF)
    in
    let { set_flags = set_flags0; is_conditional = is_conditional0;
      has_shift = has_shift0 } = __top_assumption_0
    in
    _evar_0_ set_flags0 is_conditional0 has_shift0
  in
  let { set_flags = set_flags0; is_conditional = is_conditional0; has_shift =
    has_shift0 } = __top_assumption_
  in
  _evar_0_ set_flags0 is_conditional0 has_shift0

(** val eqTC_arm_options : arm_options eqTypeC **)

let eqTC_arm_options =
  { beq = arm_options_beq; ceqP = arm_options_eq_axiom }

(** val arm_options_eqType : Equality.coq_type **)

let arm_options_eqType =
  ceqT_eqType eqTC_arm_options

(** val arm_options_dec_eq : arm_options -> arm_options -> bool **)

let arm_options_dec_eq ao0 ao1 =
  let _evar_0_ = fun _ -> true in
  let _evar_0_0 = fun _ -> false in
  (match arm_options_eq_axiom ao0 ao1 with
   | ReflectT -> _evar_0_ __
   | ReflectF -> _evar_0_0 __)

(** val default_opts : arm_options **)

let default_opts =
  { set_flags = false; is_conditional = false; has_shift = None }

(** val set_is_conditional : arm_options -> arm_options **)

let set_is_conditional ao =
  { set_flags = ao.set_flags; is_conditional = true; has_shift =
    ao.has_shift }

(** val unset_is_conditional : arm_options -> arm_options **)

let unset_is_conditional ao =
  { set_flags = ao.set_flags; is_conditional = false; has_shift =
    ao.has_shift }

type arm_mnemonic =
| ADD
| ADC
| MUL
| SDIV
| SUB
| RSB
| UDIV
| UMULL
| AND
| BIC
| EOR
| MVN
| ORR
| ASR
| LSL
| LSR
| ROR
| MOV
| MOVT
| UBFX
| UXTB
| UXTH
| SBFX
| CMP
| TST
| LDR
| LDRB
| LDRH
| LDRSB
| LDRSH
| STR
| STRB
| STRH

(** val arm_mnemonic_dec_eq : arm_mnemonic -> arm_mnemonic -> bool **)

let arm_mnemonic_dec_eq mn0 mn1 =
  match mn0 with
  | ADD -> (match mn1 with
            | ADD -> true
            | _ -> false)
  | ADC -> (match mn1 with
            | ADC -> true
            | _ -> false)
  | MUL -> (match mn1 with
            | MUL -> true
            | _ -> false)
  | SDIV -> (match mn1 with
             | SDIV -> true
             | _ -> false)
  | SUB -> (match mn1 with
            | SUB -> true
            | _ -> false)
  | RSB -> (match mn1 with
            | RSB -> true
            | _ -> false)
  | UDIV -> (match mn1 with
             | UDIV -> true
             | _ -> false)
  | UMULL -> (match mn1 with
              | UMULL -> true
              | _ -> false)
  | AND -> (match mn1 with
            | AND -> true
            | _ -> false)
  | BIC -> (match mn1 with
            | BIC -> true
            | _ -> false)
  | EOR -> (match mn1 with
            | EOR -> true
            | _ -> false)
  | MVN -> (match mn1 with
            | MVN -> true
            | _ -> false)
  | ORR -> (match mn1 with
            | ORR -> true
            | _ -> false)
  | ASR -> (match mn1 with
            | ASR -> true
            | _ -> false)
  | LSL -> (match mn1 with
            | LSL -> true
            | _ -> false)
  | LSR -> (match mn1 with
            | LSR -> true
            | _ -> false)
  | ROR -> (match mn1 with
            | ROR -> true
            | _ -> false)
  | MOV -> (match mn1 with
            | MOV -> true
            | _ -> false)
  | MOVT -> (match mn1 with
             | MOVT -> true
             | _ -> false)
  | UBFX -> (match mn1 with
             | UBFX -> true
             | _ -> false)
  | UXTB -> (match mn1 with
             | UXTB -> true
             | _ -> false)
  | UXTH -> (match mn1 with
             | UXTH -> true
             | _ -> false)
  | SBFX -> (match mn1 with
             | SBFX -> true
             | _ -> false)
  | CMP -> (match mn1 with
            | CMP -> true
            | _ -> false)
  | TST -> (match mn1 with
            | TST -> true
            | _ -> false)
  | LDR -> (match mn1 with
            | LDR -> true
            | _ -> false)
  | LDRB -> (match mn1 with
             | LDRB -> true
             | _ -> false)
  | LDRH -> (match mn1 with
             | LDRH -> true
             | _ -> false)
  | LDRSB -> (match mn1 with
              | LDRSB -> true
              | _ -> false)
  | LDRSH -> (match mn1 with
              | LDRSH -> true
              | _ -> false)
  | STR -> (match mn1 with
            | STR -> true
            | _ -> false)
  | STRB -> (match mn1 with
             | STRB -> true
             | _ -> false)
  | STRH -> (match mn1 with
             | STRH -> true
             | _ -> false)

(** val arm_mnemonic_beq : arm_mnemonic -> arm_mnemonic -> bool **)

let arm_mnemonic_beq mn0 mn1 =
  if arm_mnemonic_dec_eq mn0 mn1 then true else false

(** val arm_mnemonic_eq_axiom : arm_mnemonic Equality.axiom **)

let arm_mnemonic_eq_axiom mn0 mn1 =
  iffP (arm_mnemonic_beq mn0 mn1)
    (if arm_mnemonic_beq mn0 mn1 then ReflectT else ReflectF)

(** val eqTC_arm_mnemonic : arm_mnemonic eqTypeC **)

let eqTC_arm_mnemonic =
  { beq = arm_mnemonic_beq; ceqP = arm_mnemonic_eq_axiom }

(** val arm_mnemonic_eqType : Equality.coq_type **)

let arm_mnemonic_eqType =
  ceqT_eqType eqTC_arm_mnemonic

(** val arm_mnemonics : arm_mnemonic list **)

let arm_mnemonics =
  ADD :: (ADC :: (MUL :: (SDIV :: (SUB :: (RSB :: (UDIV :: (UMULL :: (AND :: (BIC :: (EOR :: (MVN :: (ORR :: (ASR :: (LSL :: (LSR :: (ROR :: (MOV :: (MOVT :: (UBFX :: (UXTB :: (UXTH :: (SBFX :: (CMP :: (TST :: (LDR :: (LDRB :: (LDRH :: (LDRSB :: (LDRSH :: (STR :: (STRB :: (STRH :: []))))))))))))))))))))))))))))))))

(** val finTC_arm_mnemonic : arm_mnemonic finTypeC **)

let finTC_arm_mnemonic =
  { _eqC = eqTC_arm_mnemonic; cenum = arm_mnemonics }

(** val arm_mnemonic_finType : Finite.coq_type **)

let arm_mnemonic_finType =
  cfinT_finType finTC_arm_mnemonic

(** val set_flags_mnemonics : arm_mnemonic list **)

let set_flags_mnemonics =
  ADD :: (ADC :: (MUL :: (SUB :: (RSB :: (AND :: (BIC :: (EOR :: (MVN :: (ORR :: (ASR :: (LSL :: (LSR :: (ROR :: (MOV :: []))))))))))))))

(** val has_shift_mnemonics : arm_mnemonic list **)

let has_shift_mnemonics =
  ADD :: (ADC :: (SUB :: (RSB :: (AND :: (BIC :: (EOR :: (MVN :: (ORR :: (CMP :: (TST :: []))))))))))

(** val condition_mnemonics : arm_mnemonic list **)

let condition_mnemonics =
  CMP :: (TST :: [])

(** val wsize_uload_mn : (wsize * arm_mnemonic) list **)

let wsize_uload_mn =
  (U8, LDRB) :: ((U16, LDRH) :: ((U32, LDR) :: []))

(** val uload_mn_of_wsize : wsize -> arm_mnemonic option **)

let uload_mn_of_wsize ws =
  assoc wsize_eqType (Obj.magic wsize_uload_mn) (Obj.magic ws)

(** val wsize_of_uload_mn : arm_mnemonic -> wsize option **)

let wsize_of_uload_mn mn =
  assoc arm_mnemonic_eqType
    (map (fun x -> ((snd (Obj.magic x)), (fst x))) wsize_uload_mn)
    (Obj.magic mn)

(** val wsize_sload_mn : (wsize * arm_mnemonic) list **)

let wsize_sload_mn =
  (U8, LDRSB) :: ((U16, LDRSH) :: [])

(** val sload_mn_of_wsize : wsize -> arm_mnemonic option **)

let sload_mn_of_wsize ws =
  assoc wsize_eqType (Obj.magic wsize_sload_mn) (Obj.magic ws)

(** val wsize_of_sload_mn : arm_mnemonic -> wsize option **)

let wsize_of_sload_mn mn =
  assoc arm_mnemonic_eqType
    (map (fun x -> ((snd (Obj.magic x)), (fst x))) wsize_sload_mn)
    (Obj.magic mn)

(** val wsize_of_load_mn : arm_mnemonic -> wsize option **)

let wsize_of_load_mn mn =
  match wsize_of_uload_mn mn with
  | Some ws -> Some ws
  | None -> wsize_of_sload_mn mn

(** val wsize_store_mn : (wsize * arm_mnemonic) list **)

let wsize_store_mn =
  (U8, STRB) :: ((U16, STRH) :: ((U32, STR) :: []))

(** val store_mn_of_wsize : wsize -> arm_mnemonic option **)

let store_mn_of_wsize ws =
  assoc wsize_eqType (Obj.magic wsize_store_mn) (Obj.magic ws)

(** val wsize_of_store_mn : arm_mnemonic -> wsize option **)

let wsize_of_store_mn mn =
  assoc arm_mnemonic_eqType
    (map (fun x -> ((snd (Obj.magic x)), (fst x))) wsize_store_mn)
    (Obj.magic mn)

(** val string_of_arm_mnemonic : arm_mnemonic -> char list **)

let string_of_arm_mnemonic = function
| ADD -> 'A'::('D'::('D'::[]))
| ADC -> 'A'::('D'::('C'::[]))
| MUL -> 'M'::('U'::('L'::[]))
| SDIV -> 'S'::('D'::('I'::('V'::[])))
| SUB -> 'S'::('U'::('B'::[]))
| RSB -> 'R'::('S'::('B'::[]))
| UDIV -> 'U'::('D'::('I'::('V'::[])))
| UMULL -> 'U'::('M'::('U'::('L'::('L'::[]))))
| AND -> 'A'::('N'::('D'::[]))
| BIC -> 'B'::('I'::('C'::[]))
| EOR -> 'E'::('O'::('R'::[]))
| MVN -> 'M'::('V'::('N'::[]))
| ORR -> 'O'::('R'::('R'::[]))
| ASR -> 'A'::('S'::('R'::[]))
| LSL -> 'L'::('S'::('L'::[]))
| LSR -> 'L'::('S'::('R'::[]))
| ROR -> 'R'::('O'::('R'::[]))
| MOV -> 'M'::('O'::('V'::[]))
| MOVT -> 'M'::('O'::('V'::('T'::[])))
| UBFX -> 'U'::('B'::('F'::('X'::[])))
| UXTB -> 'U'::('X'::('T'::('B'::[])))
| UXTH -> 'U'::('X'::('T'::('H'::[])))
| SBFX -> 'S'::('B'::('F'::('X'::[])))
| CMP -> 'C'::('M'::('P'::[]))
| TST -> 'T'::('S'::('T'::[]))
| LDR -> 'L'::('D'::('R'::[]))
| LDRB -> 'L'::('D'::('R'::('B'::[])))
| LDRH -> 'L'::('D'::('R'::('H'::[])))
| LDRSB -> 'L'::('D'::('R'::('S'::('B'::[]))))
| LDRSH -> 'L'::('D'::('R'::('S'::('H'::[]))))
| STR -> 'S'::('T'::('R'::[]))
| STRB -> 'S'::('T'::('R'::('B'::[])))
| STRH -> 'S'::('T'::('R'::('H'::[])))

type arm_op =
| ARM_op of arm_mnemonic * arm_options

(** val arm_op_beq : arm_op -> arm_op -> bool **)

let arm_op_beq op0 op1 =
  let ARM_op (mn0, ao0) = op0 in
  let ARM_op (mn1, ao1) = op1 in
  (&&) (eq_op arm_mnemonic_eqType (Obj.magic mn0) (Obj.magic mn1))
    (eq_op arm_options_eqType (Obj.magic ao0) (Obj.magic ao1))

(** val arm_op_eq_axiom : arm_op Equality.axiom **)

let arm_op_eq_axiom __top_assumption_ =
  let _evar_0_ = fun mn0 ao0 __top_assumption_0 ->
    let _evar_0_ = fun mn1 ao1 ->
      iffP (arm_op_beq (ARM_op (mn0, ao0)) (ARM_op (mn1, ao1)))
        (if arm_op_beq (ARM_op (mn0, ao0)) (ARM_op (mn1, ao1))
         then ReflectT
         else ReflectF)
    in
    let ARM_op (a, a0) = __top_assumption_0 in _evar_0_ a a0
  in
  let ARM_op (a, a0) = __top_assumption_ in _evar_0_ a a0

(** val eqTC_arm_op : arm_op eqTypeC **)

let eqTC_arm_op =
  { beq = arm_op_beq; ceqP = arm_op_eq_axiom }

(** val arm_op_eqType : Equality.coq_type **)

let arm_op_eqType =
  ceqT_eqType eqTC_arm_op

(** val arm_op_dec_eq : arm_op -> arm_op -> bool **)

let arm_op_dec_eq op0 op1 =
  let _evar_0_ = fun _ -> true in
  let _evar_0_0 = fun _ -> false in
  (match arm_op_eq_axiom op0 op1 with
   | ReflectT -> _evar_0_ __
   | ReflectF -> _evar_0_0 __)

(** val ad_nz : (register, __, __, rflag, condt) arg_desc list **)

let ad_nz =
  map (coq_F arm_decl) (NF :: (ZF :: []))

(** val ad_nzc : (register, __, __, rflag, condt) arg_desc list **)

let ad_nzc =
  map (coq_F arm_decl) (NF :: (ZF :: (CF :: [])))

(** val ad_nzcv : (register, __, __, rflag, condt) arg_desc list **)

let ad_nzcv =
  map (coq_F arm_decl) (NF :: (ZF :: (CF :: (VF :: []))))

(** val ak_reg_reg : arg_kind list list list **)

let ak_reg_reg =
  ((CAreg :: []) :: ((CAreg :: []) :: [])) :: []

(** val ak_reg_imm : arg_kind list list list **)

let ak_reg_imm =
  ((CAreg :: []) :: (((CAimm arm_decl.reg_size) :: []) :: [])) :: []

(** val ak_reg_imm8 : arg_kind list list list **)

let ak_reg_imm8 =
  ((CAreg :: []) :: (((CAimm U8) :: []) :: [])) :: []

(** val ak_reg_imm16 : arg_kind list list list **)

let ak_reg_imm16 =
  ((CAreg :: []) :: (((CAimm U16) :: []) :: [])) :: []

(** val ak_reg_reg_reg : arg_kind list list list **)

let ak_reg_reg_reg =
  ((CAreg :: []) :: ((CAreg :: []) :: ((CAreg :: []) :: []))) :: []

(** val ak_reg_reg_reg_reg : arg_kind list list list **)

let ak_reg_reg_reg_reg =
  ((CAreg :: []) :: ((CAreg :: []) :: ((CAreg :: []) :: ((CAreg :: []) :: [])))) :: []

(** val ak_reg_reg_imm : arg_kind list list list **)

let ak_reg_reg_imm =
  ((CAreg :: []) :: ((CAreg :: []) :: (((CAimm
    arm_decl.reg_size) :: []) :: []))) :: []

(** val ak_reg_reg_imm8 : arg_kind list list list **)

let ak_reg_reg_imm8 =
  ((CAreg :: []) :: ((CAreg :: []) :: (((CAimm U8) :: []) :: []))) :: []

(** val ak_reg_reg_imm16 : arg_kind list list list **)

let ak_reg_reg_imm16 =
  ((CAreg :: []) :: ((CAreg :: []) :: (((CAimm U16) :: []) :: []))) :: []

(** val ak_reg_addr : arg_kind list list list **)

let ak_reg_addr =
  ((CAreg :: []) :: (((CAmem true) :: []) :: [])) :: []

(** val coq_NF_of_word : wsize -> GRing.ComRing.sort -> bool **)

let coq_NF_of_word =
  msb

(** val coq_ZF_of_word : wsize -> GRing.ComRing.sort -> bool **)

let coq_ZF_of_word ws w =
  eq_op (GRing.ComRing.eqType (word ws)) w
    (GRing.zero (GRing.ComRing.zmodType (word ws)))

(** val nzcv_of_aluop :
    wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> sem_tuple **)

let nzcv_of_aluop ws res res_unsigned res_signed =
  Obj.magic ((Some (coq_NF_of_word ws res)), ((Some (coq_ZF_of_word ws res)),
    ((Some
    (negb
      (eq_op coq_Z_eqType (Obj.magic wunsigned ws res)
        (Obj.magic res_unsigned)))), (Some
    (negb
      (eq_op coq_Z_eqType (Obj.magic wsigned ws res) (Obj.magic res_signed)))))))

(** val nzcv_w_of_aluop :
    wsize -> GRing.ComRing.sort -> coq_Z -> coq_Z -> ltuple **)

let nzcv_w_of_aluop ws w wun wsi =
  merge_tuple
    (map (Obj.magic __)
      (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))))
    (map (Obj.magic __) ((Coq_sword ws) :: [])) (nzcv_of_aluop ws w wun wsi) w

(** val drop_semi_nz :
    stype list -> stype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let drop_semi_nz tin tout semi =
  behead_tuple tin (behead tout) (behead_tuple tin tout semi)

(** val drop_semi_nzc :
    stype list -> stype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let drop_semi_nzc tin tout semi =
  behead_tuple tin (behead (behead tout))
    (behead_tuple tin (behead tout) (behead_tuple tin tout semi))

(** val drop_semi_nzcv :
    stype list -> stype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let drop_semi_nzcv tin tout semi =
  behead_tuple tin (behead (behead (behead tout)))
    (behead_tuple tin (behead (behead tout))
      (behead_tuple tin (behead tout) (behead_tuple tin tout semi)))

(** val drop_nz :
    (register, __, __, rflag, condt) instr_desc_t -> (register, __, __,
    rflag, condt) instr_desc_t **)

let drop_nz idt =
  { id_msb_flag = MSB_MERGE; id_tin = idt.id_tin; id_in = idt.id_in;
    id_tout = (iter (S (S O)) behead idt.id_tout); id_out =
    (iter (S (S O)) behead idt.id_out); id_semi =
    (drop_semi_nz idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    idt.id_args_kinds; id_nargs = idt.id_nargs; id_str_jas = idt.id_str_jas;
    id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val drop_nzc :
    (register, __, __, rflag, condt) instr_desc_t -> (register, __, __,
    rflag, condt) instr_desc_t **)

let drop_nzc idt =
  { id_msb_flag = MSB_MERGE; id_tin = idt.id_tin; id_in = idt.id_in;
    id_tout = (iter (S (S (S O))) behead idt.id_tout); id_out =
    (iter (S (S (S O))) behead idt.id_out); id_semi =
    (drop_semi_nzc idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    idt.id_args_kinds; id_nargs = idt.id_nargs; id_str_jas = idt.id_str_jas;
    id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val drop_nzcv :
    (register, __, __, rflag, condt) instr_desc_t -> (register, __, __,
    rflag, condt) instr_desc_t **)

let drop_nzcv idt =
  { id_msb_flag = MSB_MERGE; id_tin = idt.id_tin; id_in = idt.id_in;
    id_tout = (iter (S (S (S (S O)))) behead idt.id_tout); id_out =
    (iter (S (S (S (S O)))) behead idt.id_out); id_semi =
    (drop_semi_nzcv idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    idt.id_args_kinds; id_nargs = idt.id_nargs; id_str_jas = idt.id_str_jas;
    id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val mk_semi_cond :
    stype list -> stype list -> sem_tuple exec sem_prod -> sem_tuple exec
    sem_prod **)

let mk_semi_cond tin tout semi =
  let f0 = fun res cond ->
    if cond
    then sem_prod_const tout res
    else sem_prod_app tout (sem_prod_tuple tout) (fun x -> Ok x)
  in
  let f1 = sem_prod_app tin semi f0 in
  add_arguments tin (Coq_sbool :: tout) f1

(** val mk_cond :
    (register, __, __, rflag, condt) instr_desc_t -> (register, __, __,
    rflag, condt) instr_desc_t **)

let mk_cond idt =
  { id_msb_flag = MSB_MERGE; id_tin =
    (cat idt.id_tin (Coq_sbool :: idt.id_tout)); id_in =
    (cat idt.id_in ((coq_E arm_decl idt.id_nargs) :: idt.id_out)); id_tout =
    idt.id_tout; id_out = idt.id_out; id_semi =
    (mk_semi_cond idt.id_tin idt.id_tout idt.id_semi); id_args_kinds =
    (map (fun x -> cat x ((CAcond :: []) :: [])) idt.id_args_kinds);
    id_nargs = (S idt.id_nargs); id_str_jas = idt.id_str_jas; id_safe =
    idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val mk_semi1_shifted :
    shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod **)

let mk_semi1_shifted sk semi =
  Obj.magic (fun wn shift_amount ->
    let sham = wunsigned U8 shift_amount in
    Obj.magic semi (shift_op sk arm_decl.reg_size wn sham))

(** val mk_semi2_2_shifted :
    stype -> shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod **)

let mk_semi2_2_shifted _ sk semi =
  Obj.magic (fun x wm shift_amount ->
    let sham = wunsigned U8 shift_amount in
    Obj.magic semi x (shift_op sk arm_decl.reg_size wm sham))

(** val mk_semi3_2_shifted :
    stype -> stype -> shift_kind -> 'a1 exec sem_prod -> 'a1 exec sem_prod **)

let mk_semi3_2_shifted _ _ sk semi =
  Obj.magic (fun x wm y shift_amount ->
    let sham = wunsigned U8 shift_amount in
    Obj.magic semi x (shift_op sk arm_decl.reg_size wm sham) y)

(** val mk_shifted :
    shift_kind -> (register, __, __, rflag, condt) instr_desc_t -> sem_tuple
    exec sem_prod -> (register, __, __, rflag, condt) instr_desc_t **)

let mk_shifted _ idt semi' =
  { id_msb_flag = MSB_MERGE; id_tin =
    (cat idt.id_tin ((Coq_sword U8) :: [])); id_in =
    (cat idt.id_in ((coq_E arm_decl idt.id_nargs) :: [])); id_tout =
    idt.id_tout; id_out = idt.id_out; id_semi = semi'; id_args_kinds =
    (map (fun x -> cat x (((CAimm arm_decl.reg_size) :: []) :: []))
      idt.id_args_kinds); id_nargs = (S idt.id_nargs); id_str_jas =
    idt.id_str_jas; id_safe = idt.id_safe; id_pp_asm = idt.id_pp_asm }

(** val pp_arm_op :
    arm_mnemonic -> arm_options -> (register, __, __, rflag, condt) asm_arg
    list -> (register, __, __, rflag, condt) pp_asm_op **)

let pp_arm_op mn _ args =
  { pp_aop_name = (string_of_arm_mnemonic mn); pp_aop_ext = PP_name;
    pp_aop_args = (map (fun a -> (arm_decl.reg_size, a)) args) }

(** val arm_ADD_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_ADD_semi wn wm =
  let x =
    nzcv_w_of_aluop arm_decl.reg_size
      (GRing.add (GRing.ComRing.zmodType (word arm_decl.reg_size)) wn wm)
      (Z.add (wunsigned arm_decl.reg_size wn)
        (wunsigned arm_decl.reg_size wm))
      (Z.add (wsigned arm_decl.reg_size wn) (wsigned arm_decl.reg_size wm))
  in
  Ok x

(** val arm_ADD_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_ADD_instr opts =
  let mn = ADD in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: []))))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_ADD_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_ADC_semi : sem_tuple -> sem_tuple -> bool -> sem_tuple exec **)

let arm_ADC_semi wn wm cf =
  let c = Z.b2z cf in
  let x =
    nzcv_w_of_aluop arm_decl.reg_size
      (GRing.add (GRing.ComRing.zmodType (word arm_decl.reg_size))
        (GRing.add (GRing.ComRing.zmodType (word arm_decl.reg_size)) wn wm)
        (wrepr arm_decl.reg_size c))
      (Z.add
        (Z.add (wunsigned arm_decl.reg_size wn)
          (wunsigned arm_decl.reg_size wm)) c)
      (Z.add
        (Z.add (wsigned arm_decl.reg_size wn) (wsigned arm_decl.reg_size wm))
        c)
  in
  Ok x

(** val arm_ADC_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_ADC_instr opts =
  let mn = ADC in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: (Coq_sbool :: []))); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: ((coq_F
                                                                 arm_decl CF) :: [])));
    id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: []))))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_ADC_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x
        (mk_semi3_2_shifted (sreg arm_decl) Coq_sbool sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_MUL_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_MUL_semi wn wm =
  let res = GRing.mul (GRing.ComRing.ringType (word arm_decl.reg_size)) wn wm
  in
  Ok
  (Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), res)))

(** val arm_MUL_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_MUL_instr opts =
  let mn = MUL in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: [])) ((sreg arm_decl) :: [])); id_out =
    (cat ad_nz ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_MUL_semi); id_args_kinds = ak_reg_reg_reg; id_nargs = (S
    (S (S O))); id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe =
    []; id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nz x

(** val arm_SDIV_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_SDIV_semi wn wm =
  Ok (wdivi arm_decl.reg_size wn wm)

(** val arm_SDIV_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_SDIV_instr opts =
  let mn = SDIV in
  { id_msb_flag = MSB_MERGE; id_tin =
  ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
  ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
  ((sreg arm_decl) :: []); id_out = ((coq_E arm_decl O) :: []); id_semi =
  (Obj.magic arm_SDIV_semi); id_args_kinds = ak_reg_reg_reg; id_nargs = (S (S
  (S O))); id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
  id_pp_asm = (pp_arm_op mn opts) }

(** val arm_SUB_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_SUB_semi wn wm =
  let wmnot = wnot arm_decl.reg_size wm in
  let x =
    nzcv_w_of_aluop arm_decl.reg_size
      (GRing.add (GRing.ComRing.zmodType (word arm_decl.reg_size))
        (GRing.add (GRing.ComRing.zmodType (word arm_decl.reg_size)) wn wmnot)
        (GRing.one (GRing.ComRing.ringType (word arm_decl.reg_size))))
      (Z.add
        (Z.add (wunsigned arm_decl.reg_size wn)
          (wunsigned arm_decl.reg_size wmnot)) (Zpos Coq_xH))
      (Z.add
        (Z.add (wsigned arm_decl.reg_size wn)
          (wsigned arm_decl.reg_size wmnot)) (Zpos Coq_xH))
  in
  Ok x

(** val arm_SUB_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_SUB_instr opts =
  let mn = SUB in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: []))))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_SUB_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_RSB_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_RSB_instr opts =
  let mn = RSB in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S (S O))) :: ((coq_E arm_decl (S O)) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: []))))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_SUB_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzcv x0

(** val arm_UDIV_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_UDIV_semi wn wm =
  Ok (wdiv arm_decl.reg_size wn wm)

(** val arm_UDIV_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_UDIV_instr opts =
  let mn = UDIV in
  { id_msb_flag = MSB_MERGE; id_tin =
  ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
  ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
  ((sreg arm_decl) :: []); id_out = ((coq_E arm_decl O) :: []); id_semi =
  (Obj.magic arm_UDIV_semi); id_args_kinds = ak_reg_reg_reg; id_nargs = (S (S
  (S O))); id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
  id_pp_asm = (pp_arm_op mn opts) }

(** val arm_UMULL_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_UMULL_semi wn wm =
  let res =
    GRing.mul (GRing.ComRing.ringType (word U64))
      (zero_extend U64 arm_decl.reg_size wn)
      (zero_extend U64 arm_decl.reg_size wm)
  in
  let lo = zero_extend U32 U64 res in
  let hi =
    zero_extend U32 U64
      (wshr U64 res (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
        Coq_xH)))))))
  in
  Ok (Obj.magic (lo, hi))

(** val arm_UMULL_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_UMULL_instr opts =
  let mn = UMULL in
  { id_msb_flag = MSB_MERGE; id_tin =
  ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
  ((coq_E arm_decl (S (S O))) :: ((coq_E arm_decl (S (S (S O)))) :: []));
  id_tout = ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_out =
  ((coq_E arm_decl O) :: ((coq_E arm_decl (S O)) :: [])); id_semi =
  (Obj.magic arm_UMULL_semi); id_args_kinds = ak_reg_reg_reg_reg; id_nargs =
  (S (S (S (S O)))); id_str_jas = (pp_s (string_of_arm_mnemonic mn));
  id_safe = []; id_pp_asm = (pp_arm_op mn opts) }

(** val arm_bitwise_semi :
    wsize -> (GRing.ComRing.sort -> GRing.ComRing.sort) ->
    (GRing.ComRing.sort -> GRing.ComRing.sort) -> (GRing.ComRing.sort ->
    GRing.ComRing.sort -> GRing.ComRing.sort) -> sem_tuple -> sem_tuple ->
    sem_tuple exec **)

let arm_bitwise_semi ws op0 op1 op wn wm =
  let res = op (op0 wn) (op1 wm) in
  Ok
  (Obj.magic ((Some (coq_NF_of_word ws res)), ((Some
    (coq_ZF_of_word ws res)), (None, res))))

(** val arm_AND_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_AND_instr opts =
  let mn = AND in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_bitwise_semi arm_decl.reg_size (fun x -> x) (fun x -> x)
      (wand arm_decl.reg_size)); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_BIC_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_BIC_instr opts =
  let mn = AND in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_bitwise_semi arm_decl.reg_size (fun x -> x)
      (wnot arm_decl.reg_size) (wand arm_decl.reg_size)); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_EOR_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_EOR_instr opts =
  let mn = EOR in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_bitwise_semi arm_decl.reg_size (fun x -> x) (fun x -> x)
      (wxor arm_decl.reg_size)); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_MVN_semi : sem_tuple -> sem_tuple exec **)

let arm_MVN_semi wn =
  let res = wnot arm_decl.reg_size wn in
  Ok
  (Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), (None, res))))

(** val arm_MVN_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_MVN_instr opts =
  let mn = MVN in
  let x = { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: []);
    id_in = ((coq_E arm_decl (S O)) :: []); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_MVN_semi); id_args_kinds = (cat ak_reg_reg ak_reg_imm);
    id_nargs = (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic mn));
    id_safe = []; id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk -> mk_shifted sk x (mk_semi1_shifted sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_ORR_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_ORR_instr opts =
  let mn = ORR in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_bitwise_semi arm_decl.reg_size (fun x -> x) (fun x -> x)
      (wor arm_decl.reg_size)); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  let x0 =
    match opts.has_shift with
    | Some sk ->
      mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
    | None -> x
  in
  if opts.set_flags then x0 else drop_nzc x0

(** val arm_ASR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple exec **)

let arm_ASR_semi wn wsham =
  let sham =
    wunsigned U8
      (wand U8 wsham
        (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
  in
  let res = wsar arm_decl.reg_size wn sham in
  Ok
  (Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), ((Some
    (msb arm_decl.reg_size res)), res))))

(** val arm_ASR_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_ASR_instr opts =
  let mn = ASR in
  let x = { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
    U8) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_ASR_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_LSL_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple exec **)

let arm_LSL_semi wn wsham =
  let sham =
    wunsigned U8
      (wand U8 wsham
        (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
  in
  let res = wshl arm_decl.reg_size wn sham in
  Ok
  (Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), ((Some
    (msb arm_decl.reg_size res)), res))))

(** val arm_LSL_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_LSL_instr opts =
  let mn = LSL in
  let x = { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
    U8) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_LSL_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_LSR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple exec **)

let arm_LSR_semi wn wsham =
  let sham =
    wunsigned U8
      (wand U8 wsham
        (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
  in
  let res = wshr arm_decl.reg_size wn sham in
  Ok
  (Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), ((Some
    (msb arm_decl.reg_size res)), res))))

(** val arm_LSR_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_LSR_instr opts =
  let mn = LSR in
  let x = { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
    U8) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_LSR_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_ROR_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple exec **)

let arm_ROR_semi wn wsham =
  let sham =
    wunsigned U8
      (wand U8 wsham
        (wrepr U8 (Zpos (Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))))))
  in
  let res = wror arm_decl.reg_size wn sham in
  Ok
  (Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), ((Some
    (msb arm_decl.reg_size res)), res))))

(** val arm_ROR_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_ROR_instr opts =
  let mn = ROR in
  let x = { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
    U8) :: [])); id_in =
    ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzc ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_ROR_semi); id_args_kinds =
    (cat ak_reg_reg_reg ak_reg_reg_imm); id_nargs = (S (S (S O)));
    id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
    id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzc x

(** val arm_MOV_semi : sem_tuple -> sem_tuple exec **)

let arm_MOV_semi wn =
  Ok
    (nzcv_w_of_aluop arm_decl.reg_size wn (wunsigned arm_decl.reg_size wn)
      (wsigned arm_decl.reg_size wn))

(** val arm_MOV_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_MOV_instr opts =
  let mn = MOV in
  let x = { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: []);
    id_in = ((coq_E arm_decl (S O)) :: []); id_tout =
    (cat (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: []))))
      ((sreg arm_decl) :: [])); id_out =
    (cat ad_nzcv ((coq_E arm_decl O) :: [])); id_semi =
    (Obj.magic arm_MOV_semi); id_args_kinds = (cat ak_reg_reg ak_reg_imm);
    id_nargs = (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic mn));
    id_safe = []; id_pp_asm = (pp_arm_op mn opts) }
  in
  if opts.set_flags then x else drop_nzcv x

(** val arm_MOVT_semi : sem_tuple -> GRing.ComRing.sort -> sem_tuple exec **)

let arm_MOVT_semi wn wm =
  let hi =
    wshl arm_decl.reg_size (zero_extend arm_decl.reg_size U16 wm) (Zpos
      (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
  in
  let mask = zero_extend arm_decl.reg_size U16 (wrepr U16 (Zneg Coq_xH)) in
  Ok (wor arm_decl.reg_size hi (wand arm_decl.reg_size wn mask))

(** val arm_MOVT_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_MOVT_instr opts =
  let mn = MOVT in
  { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
  U16) :: [])); id_in =
  ((coq_E arm_decl O) :: ((coq_E arm_decl (S O)) :: [])); id_tout =
  ((sreg arm_decl) :: []); id_out = ((coq_E arm_decl O) :: []); id_semi =
  (Obj.magic arm_MOVT_semi); id_args_kinds = (((CAreg :: []) :: (((CAimm
  U16) :: []) :: [])) :: []); id_nargs = (S (S O)); id_str_jas =
  (pp_s (string_of_arm_mnemonic mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val bit_field_extract_semi :
    ((register, __, __, rflag, condt) wreg -> coq_Z -> (register, __, __,
    rflag, condt) wreg) -> (register, __, __, rflag, condt) wreg ->
    GRing.ComRing.sort -> GRing.ComRing.sort -> (register, __, __, rflag,
    condt) wreg exec **)

let bit_field_extract_semi shr wn widx wwidth =
  let idx = wunsigned U8 widx in
  let width = wunsigned U8 wwidth in
  Ok
  (shr
    (wshl arm_decl.reg_size wn
      (Z.sub (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))) idx))
    (Z.sub (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))) width))

(** val arm_UBFX_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_UBFX_instr opts =
  let mn = UBFX in
  { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
  U8) :: ((Coq_sword U8) :: []))); id_in =
  ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: ((coq_E arm_decl
                                                               (S (S (S O)))) :: [])));
  id_tout = ((sreg arm_decl) :: []); id_out = ((coq_E arm_decl O) :: []);
  id_semi = (Obj.magic bit_field_extract_semi (wshr arm_decl.reg_size));
  id_args_kinds = (((CAreg :: []) :: ((CAreg :: []) :: (((CAimm
  U8) :: []) :: (((CAimm U8) :: []) :: [])))) :: []); id_nargs = (S (S (S (S
  O)))); id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
  id_pp_asm = (pp_arm_op mn opts) }

(** val extend_bits_semi :
    coq_Z -> (register, __, __, rflag, condt) wreg -> GRing.ComRing.sort ->
    (register, __, __, rflag, condt) wreg exec **)

let extend_bits_semi len wn wroram =
  let mask =
    wrepr arm_decl.reg_size
      (Z.sub (Z.pow (Zpos (Coq_xO Coq_xH)) len) (Zpos Coq_xH))
  in
  let roram = wunsigned U8 wroram in
  Ok (wand arm_decl.reg_size mask (wror arm_decl.reg_size wn roram))

(** val arm_UXTB_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_UXTB_instr opts =
  let mn = UXTB in
  { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
  U8) :: [])); id_in =
  ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
  ((sreg arm_decl) :: []); id_out = ((coq_E arm_decl O) :: []); id_semi =
  (Obj.magic (fun wn wroram ->
    extend_bits_semi (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))) wn
      (GRing.mul (GRing.ComRing.ringType (word U8))
        (wrepr U8 (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) wroram)));
  id_args_kinds = ak_reg_reg_imm8; id_nargs = (S (S (S O))); id_str_jas =
  (pp_s (string_of_arm_mnemonic mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_UXTH_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_UXTH_instr opts =
  let mn = UXTH in
  { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
  U8) :: [])); id_in =
  ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: [])); id_tout =
  ((sreg arm_decl) :: []); id_out = ((coq_E arm_decl O) :: []); id_semi =
  (Obj.magic (fun wn wroram ->
    extend_bits_semi (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) wn
      (GRing.mul (GRing.ComRing.ringType (word U8))
        (wrepr U8 (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))) wroram)));
  id_args_kinds = ak_reg_reg_imm16; id_nargs = (S (S (S (S O))));
  id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_SBFX_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_SBFX_instr opts =
  let mn = SBFX in
  { id_msb_flag = MSB_MERGE; id_tin = ((sreg arm_decl) :: ((Coq_sword
  U8) :: ((Coq_sword U8) :: []))); id_in =
  ((coq_E arm_decl (S O)) :: ((coq_E arm_decl (S (S O))) :: ((coq_E arm_decl
                                                               (S (S (S O)))) :: [])));
  id_tout = ((sreg arm_decl) :: []); id_out = ((coq_E arm_decl O) :: []);
  id_semi = (Obj.magic bit_field_extract_semi (wsar arm_decl.reg_size));
  id_args_kinds = (((CAreg :: []) :: ((CAreg :: []) :: (((CAimm
  U8) :: []) :: (((CAimm U8) :: []) :: [])))) :: []); id_nargs = (S (S (S (S
  O)))); id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = [];
  id_pp_asm = (pp_arm_op mn opts) }

(** val arm_CMP_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_CMP_semi wn wm =
  let wmnot = wnot arm_decl.reg_size wm in
  let res =
    nzcv_of_aluop arm_decl.reg_size
      (GRing.add (GRing.ComRing.zmodType (word arm_decl.reg_size))
        (GRing.add (GRing.ComRing.zmodType (word arm_decl.reg_size)) wn wmnot)
        (GRing.one (GRing.ComRing.ringType (word arm_decl.reg_size))))
      (Z.add
        (Z.add (wunsigned arm_decl.reg_size wn)
          (wunsigned arm_decl.reg_size wmnot)) (Zpos Coq_xH))
      (Z.add
        (Z.add (wsigned arm_decl.reg_size wn)
          (wsigned arm_decl.reg_size wmnot)) (Zpos Coq_xH))
  in
  Ok res

(** val arm_CMP_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_CMP_instr opts =
  let mn = CMP in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl O) :: ((coq_E arm_decl (S O)) :: [])); id_tout =
    (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: [])))); id_out =
    ad_nzcv; id_semi = (Obj.magic arm_CMP_semi); id_args_kinds =
    (cat ak_reg_reg ak_reg_imm); id_nargs = (S (S O)); id_str_jas =
    (pp_s (string_of_arm_mnemonic mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  (match opts.has_shift with
   | Some sk ->
     mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
   | None -> x)

(** val arm_TST_semi : sem_tuple -> sem_tuple -> sem_tuple exec **)

let arm_TST_semi wn wm =
  let res = wand arm_decl.reg_size wn wm in
  Ok
  (Obj.magic ((Some (coq_NF_of_word arm_decl.reg_size res)), ((Some
    (coq_ZF_of_word arm_decl.reg_size res)), (Some false))))

(** val arm_TST_instr :
    arm_options -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_TST_instr opts =
  let mn = TST in
  let x = { id_msb_flag = MSB_MERGE; id_tin =
    ((sreg arm_decl) :: ((sreg arm_decl) :: [])); id_in =
    ((coq_E arm_decl O) :: ((coq_E arm_decl (S O)) :: [])); id_tout =
    (Coq_sbool :: (Coq_sbool :: (Coq_sbool :: []))); id_out = ad_nzc;
    id_semi = (Obj.magic arm_TST_semi); id_args_kinds =
    (cat ak_reg_reg ak_reg_imm); id_nargs = (S (S O)); id_str_jas =
    (pp_s (string_of_arm_mnemonic mn)); id_safe = []; id_pp_asm =
    (pp_arm_op mn opts) }
  in
  (match opts.has_shift with
   | Some sk ->
     mk_shifted sk x (mk_semi2_2_shifted (sreg arm_decl) sk x.id_semi)
   | None -> x)

(** val arm_extend_semi :
    wsize -> bool -> wsize -> GRing.ComRing.sort -> GRing.ComRing.sort exec **)

let arm_extend_semi ws sign ws' wn =
  let f = if sign then sign_extend else zero_extend in Ok (f ws' ws wn)

(** val arm_load_instr :
    arm_mnemonic -> arm_options -> (register, __, __, rflag, condt)
    instr_desc_t **)

let arm_load_instr mn opts =
  let ws = match wsize_of_load_mn mn with
           | Some ws' -> ws'
           | None -> U32 in
  { id_msb_flag = MSB_MERGE; id_tin = ((Coq_sword ws) :: []); id_in =
  ((coq_E arm_decl (S O)) :: []); id_tout = ((sreg arm_decl) :: []); id_out =
  ((coq_E arm_decl O) :: []); id_semi =
  (Obj.magic arm_extend_semi ws (isSome (wsize_of_sload_mn mn))
    arm_decl.reg_size); id_args_kinds = ak_reg_addr; id_nargs = (S (S O));
  id_str_jas = (pp_s (string_of_arm_mnemonic mn)); id_safe = []; id_pp_asm =
  (pp_arm_op mn opts) }

(** val arm_store_instr :
    arm_mnemonic -> arm_options -> (register, __, __, rflag, condt)
    instr_desc_t **)

let arm_store_instr mn opts =
  let ws = match wsize_of_store_mn mn with
           | Some ws' -> ws'
           | None -> U32 in
  { id_msb_flag = MSB_MERGE; id_tin = ((Coq_sword ws) :: []); id_in =
  ((coq_E arm_decl O) :: []); id_tout = ((Coq_sword ws) :: []); id_out =
  ((coq_E arm_decl (S O)) :: []); id_semi =
  (Obj.magic arm_extend_semi ws false ws); id_args_kinds = ak_reg_addr;
  id_nargs = (S (S O)); id_str_jas = (pp_s (string_of_arm_mnemonic mn));
  id_safe = []; id_pp_asm = (pp_arm_op mn opts) }

(** val mn_desc :
    arm_mnemonic -> arm_options -> (register, __, __, rflag, condt)
    instr_desc_t **)

let mn_desc mn opts =
  match mn with
  | ADD -> arm_ADD_instr opts
  | ADC -> arm_ADC_instr opts
  | MUL -> arm_MUL_instr opts
  | SDIV -> arm_SDIV_instr opts
  | SUB -> arm_SUB_instr opts
  | RSB -> arm_RSB_instr opts
  | UDIV -> arm_UDIV_instr opts
  | UMULL -> arm_UMULL_instr opts
  | AND -> arm_AND_instr opts
  | BIC -> arm_BIC_instr opts
  | EOR -> arm_EOR_instr opts
  | MVN -> arm_MVN_instr opts
  | ORR -> arm_ORR_instr opts
  | ASR -> arm_ASR_instr opts
  | LSL -> arm_LSL_instr opts
  | LSR -> arm_LSR_instr opts
  | ROR -> arm_ROR_instr opts
  | MOV -> arm_MOV_instr opts
  | MOVT -> arm_MOVT_instr opts
  | UBFX -> arm_UBFX_instr opts
  | UXTB -> arm_UXTB_instr opts
  | UXTH -> arm_UXTH_instr opts
  | SBFX -> arm_SBFX_instr opts
  | CMP -> arm_CMP_instr opts
  | TST -> arm_TST_instr opts
  | STR -> arm_store_instr STR opts
  | STRB -> arm_store_instr STRB opts
  | STRH -> arm_store_instr STRH opts
  | x -> arm_load_instr x opts

(** val arm_instr_desc :
    arm_op -> (register, __, __, rflag, condt) instr_desc_t **)

let arm_instr_desc = function
| ARM_op (mn, opts) ->
  let x = mn_desc mn opts in if opts.is_conditional then mk_cond x else x

(** val arm_prim_string : (char list * arm_op prim_constructor) list **)

let arm_prim_string =
  let mk_prim = fun mn sf ic hs ->
    let opts = { set_flags = sf; is_conditional = ic; has_shift = hs } in
    ARM_op (mn, opts)
  in
  map (fun mn -> ((string_of_arm_mnemonic mn), (PrimARM (mk_prim mn))))
    finTC_arm_mnemonic.cenum

(** val arm_op_decl : (register, __, __, rflag, condt, arm_op) asm_op_decl **)

let arm_op_decl =
  { _eqT = eqTC_arm_op; instr_desc_op = arm_instr_desc; prim_string =
    arm_prim_string }

type arm_prog = (register, __, __, rflag, condt, arm_op) asm_prog
