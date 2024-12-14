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

(** val str : instruction_desc -> unit -> char list **)

let str i =
  i.str

(** val tin : instruction_desc -> stype list **)

let tin i =
  i.tin

(** val i_in : instruction_desc -> arg_desc list **)

let i_in i =
  i.i_in

(** val tout : instruction_desc -> stype list **)

let tout i =
  i.tout

(** val i_out : instruction_desc -> arg_desc list **)

let i_out i =
  i.i_out

(** val semi : instruction_desc -> sem_tuple exec sem_prod **)

let semi i =
  i.semi

(** val i_safe : instruction_desc -> safe_cond list **)

let i_safe i =
  i.i_safe

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

(** val _eqT : 'a1 asmOp -> 'a1 eqTypeC **)

let _eqT asmOp0 =
  asmOp0._eqT

(** val asm_op_instr : 'a1 asmOp -> 'a1 -> instruction_desc **)

let asm_op_instr asmOp0 =
  asmOp0.asm_op_instr

(** val prim_string : 'a1 asmOp -> (char list * 'a1 prim_constructor) list **)

let prim_string asmOp0 =
  asmOp0.prim_string

type 'asm_op asm_op_t = 'asm_op

type 'asm_op sopn =
| Ocopy of wsize * positive
| Onop
| Omulu of wsize
| Oaddcarry of wsize
| Osubcarry of wsize
| Oasm of 'asm_op asm_op_t

(** val sopn_beq : 'a1 asmOp -> 'a1 sopn -> 'a1 sopn -> bool **)

let sopn_beq asmop o1 o2 =
  match o1 with
  | Ocopy (ws1, p1) ->
    (match o2 with
     | Ocopy (ws2, p2) ->
       (&&) (eq_op wsize_eqType (Obj.magic ws1) (Obj.magic ws2))
         (eq_op pos_eqType (Obj.magic p1) (Obj.magic p2))
     | _ -> false)
  | Onop -> (match o2 with
             | Onop -> true
             | _ -> false)
  | Omulu ws1 ->
    (match o2 with
     | Omulu ws2 -> eq_op wsize_eqType (Obj.magic ws1) (Obj.magic ws2)
     | _ -> false)
  | Oaddcarry ws1 ->
    (match o2 with
     | Oaddcarry ws2 -> eq_op wsize_eqType (Obj.magic ws1) (Obj.magic ws2)
     | _ -> false)
  | Osubcarry ws1 ->
    (match o2 with
     | Osubcarry ws2 -> eq_op wsize_eqType (Obj.magic ws1) (Obj.magic ws2)
     | _ -> false)
  | Oasm o3 ->
    (match o2 with
     | Oasm o4 -> eq_op (ceqT_eqType asmop._eqT) (Obj.magic o3) (Obj.magic o4)
     | _ -> false)

(** val sopn_eq_axiom : 'a1 asmOp -> 'a1 sopn Equality.axiom **)

let sopn_eq_axiom asmop __top_assumption_ =
  let _evar_0_ = fun ws1 p1 __top_assumption_0 ->
    let _evar_0_ = fun ws2 p2 ->
      iffP ((&&) (eq_op wsize_eqType ws1 ws2) (eq_op pos_eqType p1 p2))
        (andP (eq_op wsize_eqType ws1 ws2) (eq_op pos_eqType p1 p2))
    in
    let _evar_0_0 = ReflectF in
    let _evar_0_1 = fun _ -> ReflectF in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | Ocopy (w, p) -> Obj.magic _evar_0_ w p
     | Onop -> _evar_0_0
     | Omulu w -> _evar_0_1 w
     | Oaddcarry w -> _evar_0_2 w
     | Osubcarry w -> _evar_0_3 w
     | Oasm a -> _evar_0_4 a)
  in
  let _evar_0_0 = fun __top_assumption_0 ->
    let _evar_0_0 = fun _ _ -> ReflectF in
    let _evar_0_1 = ReflectT in
    let _evar_0_2 = fun _ -> ReflectF in
    let _evar_0_3 = fun _ -> ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | Ocopy (w, p) -> _evar_0_0 w p
     | Onop -> _evar_0_1
     | Omulu w -> _evar_0_2 w
     | Oaddcarry w -> _evar_0_3 w
     | Osubcarry w -> _evar_0_4 w
     | Oasm a -> _evar_0_5 a)
  in
  let _evar_0_1 = fun ws1 __top_assumption_0 ->
    let _evar_0_1 = fun _ _ -> ReflectF in
    let _evar_0_2 = ReflectF in
    let _evar_0_3 = fun ws2 ->
      reflect_inj wsize_eqType (Obj.magic (fun x -> Omulu x)) ws1 ws2
        (eqP wsize_eqType ws1 ws2)
    in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun _ -> ReflectF in
    let _evar_0_6 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | Ocopy (w, p) -> _evar_0_1 w p
     | Onop -> _evar_0_2
     | Omulu w -> Obj.magic _evar_0_3 w
     | Oaddcarry w -> _evar_0_4 w
     | Osubcarry w -> _evar_0_5 w
     | Oasm a -> _evar_0_6 a)
  in
  let _evar_0_2 = fun ws1 __top_assumption_0 ->
    let _evar_0_2 = fun _ _ -> ReflectF in
    let _evar_0_3 = ReflectF in
    let _evar_0_4 = fun _ -> ReflectF in
    let _evar_0_5 = fun ws2 ->
      reflect_inj wsize_eqType (Obj.magic (fun x -> Oaddcarry x)) ws1 ws2
        (eqP wsize_eqType ws1 ws2)
    in
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | Ocopy (w, p) -> _evar_0_2 w p
     | Onop -> _evar_0_3
     | Omulu w -> _evar_0_4 w
     | Oaddcarry w -> Obj.magic _evar_0_5 w
     | Osubcarry w -> _evar_0_6 w
     | Oasm a -> _evar_0_7 a)
  in
  let _evar_0_3 = fun ws1 __top_assumption_0 ->
    let _evar_0_3 = fun _ _ -> ReflectF in
    let _evar_0_4 = ReflectF in
    let _evar_0_5 = fun _ -> ReflectF in
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun ws2 ->
      reflect_inj wsize_eqType (Obj.magic (fun x -> Osubcarry x)) ws1 ws2
        (eqP wsize_eqType ws1 ws2)
    in
    let _evar_0_8 = fun _ -> ReflectF in
    (match __top_assumption_0 with
     | Ocopy (w, p) -> _evar_0_3 w p
     | Onop -> _evar_0_4
     | Omulu w -> _evar_0_5 w
     | Oaddcarry w -> _evar_0_6 w
     | Osubcarry w -> Obj.magic _evar_0_7 w
     | Oasm a -> _evar_0_8 a)
  in
  let _evar_0_4 = fun o1 __top_assumption_0 ->
    let _evar_0_4 = fun _ _ -> ReflectF in
    let _evar_0_5 = ReflectF in
    let _evar_0_6 = fun _ -> ReflectF in
    let _evar_0_7 = fun _ -> ReflectF in
    let _evar_0_8 = fun _ -> ReflectF in
    let _evar_0_9 = fun o2 ->
      reflect_inj (ceqT_eqType asmop._eqT) (fun x -> Oasm x) o1 o2
        (eqP (ceqT_eqType asmop._eqT) o1 o2)
    in
    (match __top_assumption_0 with
     | Ocopy (w, p) -> _evar_0_4 w p
     | Onop -> _evar_0_5
     | Omulu w -> _evar_0_6 w
     | Oaddcarry w -> _evar_0_7 w
     | Osubcarry w -> _evar_0_8 w
     | Oasm a -> _evar_0_9 a)
  in
  (match __top_assumption_ with
   | Ocopy (w, p) -> Obj.magic _evar_0_ w p
   | Onop -> _evar_0_0
   | Omulu w -> Obj.magic _evar_0_1 w
   | Oaddcarry w -> Obj.magic _evar_0_2 w
   | Osubcarry w -> Obj.magic _evar_0_3 w
   | Oasm a -> Obj.magic _evar_0_4 a)

(** val sopn_eqMixin : 'a1 asmOp -> 'a1 sopn Equality.mixin_of **)

let sopn_eqMixin asmop =
  { Equality.op = (sopn_beq asmop); Equality.mixin_of__1 =
    (sopn_eq_axiom asmop) }

(** val sopn_eqType : 'a1 asmOp -> Equality.coq_type **)

let sopn_eqType asmop =
  Obj.magic sopn_eqMixin asmop

(** val coq_Ocopy_instr : wsize -> positive -> instruction_desc **)

let coq_Ocopy_instr ws p =
  let sz = Z.to_pos (arr_size ws p) in
  { str = (pp_sz ('c'::('o'::('p'::('y'::[])))) ws); tin = ((Coq_sarr
  sz) :: []); i_in = ((ADExplicit ((S O), None)) :: []); tout = ((Coq_sarr
  sz) :: []); i_out = ((ADExplicit (O, None)) :: []); semi =
  (Obj.magic WArray.copy ws p); i_safe = ((AllInit (ws, p, O)) :: []) }

(** val coq_Onop_instr : instruction_desc **)

let coq_Onop_instr =
  { str = (pp_s ('N'::('O'::('P'::[])))); tin = []; i_in = []; tout = [];
    i_out = []; semi = (Obj.magic (Ok ())); i_safe = [] }

(** val coq_Omulu_instr : wsize -> instruction_desc **)

let coq_Omulu_instr sz =
  { str = (pp_sz ('m'::('u'::('l'::('u'::[])))) sz); tin = ((Coq_sword
    sz) :: ((Coq_sword sz) :: [])); i_in = ((ADExplicit (O,
    None)) :: ((ADExplicit ((S O), None)) :: [])); tout = ((Coq_sword
    sz) :: ((Coq_sword sz) :: [])); i_out = ((ADExplicit ((S (S O)),
    None)) :: ((ADExplicit ((S (S (S O))), None)) :: [])); semi =
    (Obj.magic (fun x y -> Ok (wumul sz x y))); i_safe = [] }

(** val coq_Oaddcarry_instr : wsize -> instruction_desc **)

let coq_Oaddcarry_instr sz =
  { str = (pp_sz ('a'::('d'::('c'::[]))) sz); tin = ((Coq_sword
    sz) :: ((Coq_sword sz) :: (Coq_sbool :: []))); i_in = ((ADExplicit (O,
    None)) :: ((ADExplicit ((S O), None)) :: ((ADExplicit ((S (S O)),
    None)) :: []))); tout = (Coq_sbool :: ((Coq_sword sz) :: [])); i_out =
    ((ADExplicit ((S (S (S O))), None)) :: ((ADExplicit ((S (S (S (S O)))),
    None)) :: [])); semi =
    (Obj.magic (fun x y c ->
      let p = waddcarry sz x y c in Ok ((Some (fst p)), (snd p)))); i_safe =
    [] }

(** val coq_Osubcarry_instr : wsize -> instruction_desc **)

let coq_Osubcarry_instr sz =
  { str = (pp_sz ('s'::('b'::('b'::[]))) sz); tin = ((Coq_sword
    sz) :: ((Coq_sword sz) :: (Coq_sbool :: []))); i_in = ((ADExplicit (O,
    None)) :: ((ADExplicit ((S O), None)) :: ((ADExplicit ((S (S O)),
    None)) :: []))); tout = (Coq_sbool :: ((Coq_sword sz) :: [])); i_out =
    ((ADExplicit ((S (S (S O))), None)) :: ((ADExplicit ((S (S (S (S O)))),
    None)) :: [])); semi =
    (Obj.magic (fun x y c ->
      let p = wsubcarry sz x y c in Ok ((Some (fst p)), (snd p)))); i_safe =
    [] }

(** val get_instr_desc : 'a1 asmOp -> 'a1 sopn -> instruction_desc **)

let get_instr_desc asmop = function
| Ocopy (ws, p) -> coq_Ocopy_instr ws p
| Onop -> coq_Onop_instr
| Omulu sz -> coq_Omulu_instr sz
| Oaddcarry sz -> coq_Oaddcarry_instr sz
| Osubcarry sz -> coq_Osubcarry_instr sz
| Oasm o0 -> asmop.asm_op_instr o0

(** val string_of_sopn : 'a1 asmOp -> 'a1 sopn -> char list **)

let string_of_sopn asmop o =
  (get_instr_desc asmop o).str ()

(** val sopn_tin : 'a1 asmOp -> 'a1 sopn -> stype list **)

let sopn_tin asmop o =
  (get_instr_desc asmop o).tin

(** val sopn_tout : 'a1 asmOp -> 'a1 sopn -> stype list **)

let sopn_tout asmop o =
  (get_instr_desc asmop o).tout

(** val sopn_sem : 'a1 asmOp -> 'a1 sopn -> sem_tuple exec sem_prod **)

let sopn_sem asmop o =
  (get_instr_desc asmop o).semi

(** val eqC_sopn : 'a1 asmOp -> 'a1 sopn eqTypeC **)

let eqC_sopn asmop =
  { beq = (sopn_beq asmop); ceqP = (sopn_eq_axiom asmop) }

(** val map_prim_constructor :
    ('a1 -> 'a2) -> 'a1 prim_constructor -> 'a2 prim_constructor **)

let map_prim_constructor f = function
| PrimP (x1, x2) -> PrimP (x1, (fun ws1 ws2 -> f (x2 ws1 ws2)))
| PrimM x -> PrimM (fun ws -> f (x ws))
| PrimV x -> PrimV (fun ws1 s v ws2 -> f (x ws1 s v ws2))
| PrimX x -> PrimX (fun ws1 ws2 ws3 -> f (x ws1 ws2 ws3))
| PrimVV x -> PrimVV (fun ws1 v1 ws2 v2 ws3 -> f (x ws1 v1 ws2 v2 ws3))
| PrimARM x -> PrimARM (fun sf ic hs -> f (x sf ic hs))

(** val sopn_prim_string :
    coq_PointerData -> 'a1 asmOp -> (char list * 'a1 sopn prim_constructor)
    list **)

let sopn_prim_string pd asmop =
  cat ((('c'::('o'::('p'::('y'::[])))), (PrimP ((coq_Uptr pd), (fun _ sz ->
    Ocopy (sz, Coq_xH))))) :: ((('m'::('u'::('l'::('u'::[])))), (PrimP
    ((coq_Uptr pd), (fun _ sz -> Omulu sz)))) :: ((('a'::('d'::('c'::[]))),
    (PrimP ((coq_Uptr pd), (fun _ sz -> Oaddcarry
    sz)))) :: ((('s'::('b'::('b'::[]))), (PrimP ((coq_Uptr pd), (fun _ sz ->
    Osubcarry sz)))) :: []))))
    (map (fun pat ->
      let (s, p) = pat in (s, (map_prim_constructor (fun x -> Oasm x) p)))
      asmop.prim_string)

(** val asmOp_sopn : coq_PointerData -> 'a1 asmOp -> 'a1 sopn asmOp **)

let asmOp_sopn pd asmop =
  { _eqT = (eqC_sopn asmop); asm_op_instr = (get_instr_desc asmop);
    prim_string = (sopn_prim_string pd asmop) }
