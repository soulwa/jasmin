open Bool
open Arch_decl
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Ssrfun
open Strings
open Type
open Utils0
open Var0
open Wsize
open Xseq

(** val of_string : stype -> 'a1 coq_ToString -> char list -> 'a1 option **)

let of_string _ tS s =
  assoc string_eqType (Obj.magic tS.strings) (Obj.magic s)

(** val to_var : stype -> 'a1 coq_ToString -> 'a1 -> Var.var **)

let to_var t tS r =
  { Var.vtype = (rtype t tS); Var.vname = (Obj.magic tS.to_string r) }

(** val of_var : stype -> 'a1 coq_ToString -> Var.var -> 'a1 option **)

let of_var t tS v =
  if eq_op stype_eqType (Obj.magic Var.vtype v) (Obj.magic rtype t tS)
  then of_string t tS (Obj.magic Var.vname v)
  else None

(** val sopn_implicit_arg :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    Arch_decl.implicit_arg -> implicit_arg **)

let sopn_implicit_arg arch = function
| Arch_decl.IArflag r -> IArflag (to_var Coq_sbool arch.toS_f r)
| Arch_decl.IAreg r -> IArflag (to_var (Coq_sword arch.reg_size) arch.toS_r r)

(** val sopn_arg_desc :
    ('a1, 'a2, 'a3, 'a4, 'a5) arch_decl -> ('a1, 'a2, 'a3, 'a4, 'a5)
    Arch_decl.arg_desc -> arg_desc **)

let sopn_arg_desc arch = function
| Arch_decl.ADImplicit ia -> ADImplicit (sopn_implicit_arg arch ia)
| Arch_decl.ADExplicit (_, n, ox) ->
  ADExplicit (n,
    (Option.map (to_var (Coq_sword arch.reg_size) arch.toS_r) ox))

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) asm_extra = { 
_asm : ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm;
_extra : 'extra_op asmOp;
to_asm : (instr_info -> 'extra_op -> lval list -> pexpr list -> ((('reg,
         'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t * lval
         list) * pexpr list) cexec) }

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t =
  'extra_op

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extended_op =
| BaseOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op) asm_op_msb_t
| ExtOp of ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op) extra_op_t

(** val extended_op_beq :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op -> bool **)

let extended_op_beq asm_e o1 o2 =
  match o1 with
  | BaseOp o3 ->
    (match o2 with
     | BaseOp o4 ->
       eq_op
         (prod_eqType (option_eqType wsize_eqType)
           (ceqT_eqType asm_e._asm._asm_op_decl.Arch_decl._eqT))
         (Obj.magic o3) (Obj.magic o4)
     | ExtOp _ -> false)
  | ExtOp o3 ->
    (match o2 with
     | BaseOp _ -> false
     | ExtOp o4 ->
       eq_op (ceqT_eqType asm_e._extra._eqT) (Obj.magic o3) (Obj.magic o4))

(** val extended_op_eq_axiom :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op Equality.axiom **)

let extended_op_eq_axiom asm_e _top_assumption_ =
  let _evar_0_ = fun o1 __top_assumption_ ->
    let _evar_0_ = fun o2 ->
      reflect_inj
        (prod_eqType (option_eqType wsize_eqType)
          (ceqT_eqType asm_e._asm._asm_op_decl.Arch_decl._eqT))
        (Obj.magic (fun x -> BaseOp x)) o1 o2
        (eqP
          (prod_eqType (option_eqType wsize_eqType)
            (ceqT_eqType asm_e._asm._asm_op_decl.Arch_decl._eqT)) o1 o2)
    in
    let _evar_0_0 = fun _ -> ReflectF in
    (match __top_assumption_ with
     | BaseOp a -> Obj.magic _evar_0_ a
     | ExtOp e -> _evar_0_0 e)
  in
  let _evar_0_0 = fun o1 __top_assumption_ ->
    let _evar_0_0 = fun _ -> ReflectF in
    let _evar_0_1 = fun o2 ->
      reflect_inj (ceqT_eqType asm_e._extra._eqT) (fun x -> ExtOp x) o1 o2
        (eqP (ceqT_eqType asm_e._extra._eqT) o1 o2)
    in
    (match __top_assumption_ with
     | BaseOp a -> _evar_0_0 a
     | ExtOp e -> _evar_0_1 e)
  in
  (match _top_assumption_ with
   | BaseOp a -> Obj.magic _evar_0_ a
   | ExtOp e -> Obj.magic _evar_0_0 e)

(** val get_instr_desc :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op -> instruction_desc **)

let get_instr_desc asm_e = function
| BaseOp o0 ->
  let id = instr_desc asm_e._asm._arch_decl asm_e._asm._asm_op_decl o0 in
  { str = id.id_str_jas; tin = id.id_tin; i_in =
  (map (sopn_arg_desc asm_e._asm._arch_decl) id.id_in); tout = id.id_tout;
  i_out = (map (sopn_arg_desc asm_e._asm._arch_decl) id.id_out); semi =
  id.id_semi; i_safe = id.id_safe }
| ExtOp o0 -> asm_e._extra.asm_op_instr o0

(** val sopn_prim_constructor :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (wsize option -> 'a6 ->
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op) -> 'a6
    Arch_decl.prim_constructor -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
    extended_op prim_constructor **)

let sopn_prim_constructor _ f = function
| Arch_decl.PrimP (x1, x2) -> PrimP (x1, (fun ws1 ws2 -> f ws1 (x2 ws2)))
| Arch_decl.PrimM x -> PrimM (fun ws -> f ws x)
| Arch_decl.PrimV x -> PrimV (fun ws1 _ v ws2 -> f ws1 (x v ws2))
| PrimSV x -> PrimV (fun ws1 s v ws2 -> f ws1 (x s v ws2))
| Arch_decl.PrimX x -> PrimX (fun ws1 ws2 ws3 -> f ws1 (x ws2 ws3))
| Arch_decl.PrimVV x ->
  PrimVV (fun ws1 v1 ws2 v2 ws3 -> f ws1 (x v1 ws2 v2 ws3))
| Arch_decl.PrimARM x -> PrimARM (fun sf ic hs -> f None (x sf ic hs))

(** val sopn_prim_string_base :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (char list * 'a6
    Arch_decl.prim_constructor) list -> (char list * ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op prim_constructor) list **)

let sopn_prim_string_base asm_e o =
  let to_ex = fun ws o0 -> BaseOp (ws, o0) in
  map (fun pat ->
    let (s, p) = pat in (s, (sopn_prim_constructor asm_e to_ex p))) o

(** val sopn_prim_string_extra :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (char list * 'a7
    prim_constructor) list -> (char list * ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op prim_constructor) list **)

let sopn_prim_string_extra _ o =
  let to_ex = fun o0 -> ExtOp o0 in
  map (fun pat -> let (s, p) = pat in (s, (map_prim_constructor to_ex p))) o

(** val get_prime_op :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (char list * ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7) extended_op prim_constructor) list **)

let get_prime_op asm_e =
  cat
    (sopn_prim_string_base asm_e
      asm_e._asm._asm_op_decl.Arch_decl.prim_string)
    (sopn_prim_string_extra asm_e asm_e._extra.prim_string)

(** val eqTC_extended_op :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op eqTypeC **)

let eqTC_extended_op asm_e =
  { beq = (extended_op_beq asm_e); ceqP = (extended_op_eq_axiom asm_e) }

(** val asm_opI :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op asmOp **)

let asm_opI asm_e =
  { _eqT = (eqTC_extended_op asm_e); asm_op_instr = (get_instr_desc asm_e);
    prim_string = (get_prime_op asm_e) }
