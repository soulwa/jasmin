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

val register_dec_eq : register -> register -> bool

val register_beq : register -> register -> bool

val register_eq_axiom : register Equality.axiom

val eqTC_register : register eqTypeC

val arm_register_eqType : Equality.coq_type

val registers : register list

val finTC_register : register finTypeC

val register_finType : Finite.coq_type

val string_of_register : register -> char list

val reg_toS : register coq_ToString

val register_ext_dec_eq : __ -> bool

val register_ext_beq : __ -> bool

val regx_eq_axiom : __ -> reflect

val regx_eqMixin : __ Equality.mixin_of

val regx_eqType : Equality.coq_type

val register_exts : __ list

val regx_choiceMixin : Equality.sort Choice.Choice.mixin_of

val regx_choiceType : Choice.Choice.coq_type

val regx_countMixin : Equality.sort Choice.Countable.mixin_of

val regx_countType : Choice.Countable.coq_type

val regx_finMixin : Finite.mixin_of

val regx_finType : Finite.coq_type

val string_of_register_ext : __ -> char list

val eqTC_register_ext : __ eqTypeC

val finC_register_ext : __ finTypeC

val regx_toS : __ coq_ToString

val xregister_dec_eq : __ -> bool

val xregister_beq : __ -> bool

val xregister_eq_axiom : __ -> reflect

val eqTC_xregister : __ eqTypeC

val xregister_eqType : Equality.coq_type

val xregisters : __ list

val finTC_xregister : __ finTypeC

val xregister_finType : Finite.coq_type

val string_of_xregister : __ -> char list

val xreg_toS : __ coq_ToString

type rflag =
| NF
| ZF
| CF
| VF

val rflag_dec_eq : rflag -> rflag -> bool

val rflag_beq : rflag -> rflag -> bool

val rflag_eq_axiom : rflag Equality.axiom

val eqTC_rflag : rflag eqTypeC

val rflag_eqType : Equality.coq_type

val rflags : rflag list

val finTC_rflag : rflag finTypeC

val rflag_finType : Finite.coq_type

val string_of_rflag : rflag -> char list

val rflag_toS : rflag coq_ToString

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

val condt_dec_eq : condt -> condt -> bool

val condt_beq : condt -> condt -> bool

val condt_eq_axiom : condt Equality.axiom

val eqTC_condt : condt eqTypeC

val condt_eqType : Equality.coq_type

val condts : condt list

val finTC_condt : condt finTypeC

val condt_finType : Finite.coq_type

val string_of_condt : condt -> char list

val shift_kind_dec_eq : shift_kind -> shift_kind -> bool

val shift_kind_beq : shift_kind -> shift_kind -> bool

val shift_kind_eq_axiom : shift_kind Equality.axiom

val eqTC_shift_kind : shift_kind eqTypeC

val shift_kind_eqType : Equality.coq_type

val shift_kinds : shift_kind list

val finTC_shift_kind : shift_kind finTypeC

val shift_kind_finType : Finite.coq_type

val string_of_shift_kind : shift_kind -> char list

val shift_kind_toS : shift_kind coq_ToString

val check_shift_amount : shift_kind -> coq_Z -> bool

val shift_op :
  shift_kind -> wsize -> GRing.ComRing.sort -> coq_Z -> GRing.ComRing.sort

val shift_of_sop2 : wsize -> sop2 -> shift_kind option

val arm_fc_of_cfc : combine_flags_core -> flag_combination

val arm_fcp : coq_FlagCombinationParams

val arm_decl : (register, __, __, rflag, condt) arch_decl

val arm_linux_call_conv : (register, __, __, rflag, condt) calling_convention
