open Bool
open Datatypes
open Arch_decl
open Arch_extra
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Ssralg
open Ssrbool
open Type
open Utils0
open Word0
open Wsize
open X86
open X86_decl
open X86_instr_decl

type x86_extra_op =
| Oset0 of wsize
| Oconcat128
| Ox86MOVZX32

(** val x86_extra_op_beq : x86_extra_op -> x86_extra_op -> bool **)

let x86_extra_op_beq x y =
  match x with
  | Oset0 x0 -> (match y with
                 | Oset0 x1 -> wsize_beq x0 x1
                 | _ -> false)
  | Oconcat128 -> (match y with
                   | Oconcat128 -> true
                   | _ -> false)
  | Ox86MOVZX32 -> (match y with
                    | Ox86MOVZX32 -> true
                    | _ -> false)

(** val x86_extra_op_eq_dec : x86_extra_op -> x86_extra_op -> bool **)

let x86_extra_op_eq_dec x y =
  let b = x86_extra_op_beq x y in if b then true else false

(** val x86_extra_op_eq_axiom : x86_extra_op Equality.axiom **)

let x86_extra_op_eq_axiom x y =
  iffP (x86_extra_op_beq x y)
    (if x86_extra_op_beq x y then ReflectT else ReflectF)

(** val x86_extra_op_eqMixin : x86_extra_op Equality.mixin_of **)

let x86_extra_op_eqMixin =
  { Equality.op = x86_extra_op_beq; Equality.mixin_of__1 =
    x86_extra_op_eq_axiom }

(** val x86_extra_op_eqType : Equality.coq_type **)

let x86_extra_op_eqType =
  Obj.magic x86_extra_op_eqMixin

(** val coq_Oset0_instr : wsize -> instruction_desc **)

let coq_Oset0_instr sz =
  if cmp_le wsize_cmp sz U64
  then { str = (pp_sz ('s'::('e'::('t'::('0'::[])))) sz); tin = []; i_in =
         []; tout = (b5w_ty sz); i_out =
         (cat (map (sopn_arg_desc x86_decl) implicit_flags) ((ADExplicit (O,
           None)) :: [])); semi =
         (let vf = Some false in
          let vt = Some true in
          Obj.magic (Ok (vf, (vf, (vf, (vt, (vt,
            (GRing.zero (GRing.ComRing.zmodType (word sz)))))))))); i_safe =
         [] }
  else { str = (pp_sz ('s'::('e'::('t'::('0'::[])))) sz); tin = []; i_in =
         []; tout = (w_ty sz); i_out = ((ADExplicit (O, None)) :: []); semi =
         (Obj.magic (Ok (GRing.zero (GRing.ComRing.zmodType (word sz)))));
         i_safe = [] }

(** val coq_Oconcat128_instr : instruction_desc **)

let coq_Oconcat128_instr =
  { str =
    (pp_s
      ('c'::('o'::('n'::('c'::('a'::('t'::('_'::('2'::('u'::('1'::('2'::('8'::[])))))))))))));
    tin = ((Coq_sword U128) :: ((Coq_sword U128) :: [])); i_in = ((ADExplicit
    ((S O), None)) :: ((ADExplicit ((S (S O)), None)) :: [])); tout =
    ((Coq_sword U256) :: []); i_out = ((ADExplicit (O, None)) :: []); semi =
    (Obj.magic (fun h l -> Ok (make_vec U128 U256 (l :: (h :: [])))));
    i_safe = [] }

(** val coq_Ox86MOVZX32_instr : instruction_desc **)

let coq_Ox86MOVZX32_instr =
  { str = (pp_s ('M'::('O'::('V'::('Z'::('X'::('3'::('2'::[])))))))); tin =
    ((Coq_sword U32) :: []); i_in = ((ADExplicit ((S O), None)) :: []);
    tout = ((Coq_sword U64) :: []); i_out = ((ADExplicit (O, None)) :: []);
    semi = (Obj.magic (fun x -> Ok (zero_extend U64 U32 x))); i_safe = [] }

(** val get_instr_desc : x86_extra_op -> instruction_desc **)

let get_instr_desc = function
| Oset0 ws -> coq_Oset0_instr ws
| Oconcat128 -> coq_Oconcat128_instr
| Ox86MOVZX32 -> coq_Ox86MOVZX32_instr

(** val prim_string : (char list * x86_extra_op prim_constructor) list **)

let prim_string =
  (('s'::('e'::('t'::('0'::[])))), (PrimP (U64, (fun _ sz -> Oset0
    sz)))) :: ((('c'::('o'::('n'::('c'::('a'::('t'::('_'::('2'::('u'::('1'::('2'::('8'::[])))))))))))),
    (PrimM (fun _ -> Oconcat128))) :: [])

module E =
 struct
  (** val pass_name : char list **)

  let pass_name =
    'a'::('s'::('m'::('g'::('e'::('n'::[])))))

  (** val error : instr_info -> char list -> pp_error_loc **)

  let error ii msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = None; pel_ii = (Some
      ii); pel_vi = None; pel_pass = (Some pass_name); pel_internal = true }
 end

(** val assemble_extra :
    instr_info -> x86_extra_op -> lval list -> pexpr list -> (((register,
    register_ext, xmm_register, rflag, condt, x86_op) asm_op_msb_t * lval
    list) * pexpr list) cexec **)

let assemble_extra ii o outx inx =
  match o with
  | Oset0 sz ->
    let op0 = if cmp_le wsize_cmp sz U64 then XOR sz else VPXOR sz in
    (match rev outx with
     | [] ->
       let s =
         E.error ii
           ('s'::('e'::('t'::('0'::(' '::(':'::(' '::('d'::('e'::('s'::('t'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))
       in
       Error s
     | y :: _ ->
       (match y with
        | Lnone (_, _) ->
          let s =
            E.error ii
              ('s'::('e'::('t'::('0'::(' '::(':'::(' '::('d'::('e'::('s'::('t'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))
          in
          Error s
        | Lvar x ->
          Ok (((None, op0), outx), ((coq_Plvar x) :: ((coq_Plvar x) :: [])))
        | _ ->
          let s =
            E.error ii
              ('s'::('e'::('t'::('0'::(' '::(':'::(' '::('d'::('e'::('s'::('t'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))
          in
          Error s))
  | Oconcat128 ->
    (match inx with
     | [] ->
       let s =
         E.error ii
           ('O'::('c'::('o'::('n'::('c'::('a'::('t'::(':'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))))))))
       in
       Error s
     | h :: l0 ->
       (match l0 with
        | [] ->
          let s =
            E.error ii
              ('O'::('c'::('o'::('n'::('c'::('a'::('t'::(':'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))))))))
          in
          Error s
        | l :: l1 ->
          (match l with
           | Pconst _ ->
             let s =
               E.error ii
                 ('O'::('c'::('o'::('n'::('c'::('a'::('t'::(':'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))))))))
             in
             Error s
           | Pbool _ ->
             let s =
               E.error ii
                 ('O'::('c'::('o'::('n'::('c'::('a'::('t'::(':'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))))))))
             in
             Error s
           | Parr_init _ ->
             let s =
               E.error ii
                 ('O'::('c'::('o'::('n'::('c'::('a'::('t'::(':'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))))))))
             in
             Error s
           | Pvar _ ->
             (match l1 with
              | [] ->
                let x =
                  l :: (h :: ((wconst U8
                                (GRing.one (GRing.ComRing.ringType (word U8)))) :: []))
                in
                Ok (((None, VINSERTI128), outx), x)
              | _ :: _ ->
                let s =
                  E.error ii
                    ('O'::('c'::('o'::('n'::('c'::('a'::('t'::(':'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))))))))
                in
                Error s)
           | _ ->
             let s =
               E.error ii
                 ('O'::('c'::('o'::('n'::('c'::('a'::('t'::(':'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::('f'::('a'::('l'::('s'::('e'::[])))))))))))))))))))))
             in
             Error s)))
  | Ox86MOVZX32 ->
    (match outx with
     | [] ->
       let s =
         E.error ii
           ('O'::('x'::('8'::('6'::('M'::('O'::('V'::('Z'::('X'::('3'::('2'::(':'::(' '::('d'::('e'::('s'::('t'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))
       in
       Error s
     | l :: l0 ->
       (match l with
        | Lnone (_, _) ->
          let s =
            E.error ii
              ('O'::('x'::('8'::('6'::('M'::('O'::('V'::('Z'::('X'::('3'::('2'::(':'::(' '::('d'::('e'::('s'::('t'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))
          in
          Error s
        | Lvar _ ->
          (match l0 with
           | [] -> Ok (((None, (MOV U32)), outx), inx)
           | _ :: _ ->
             let s =
               E.error ii
                 ('O'::('x'::('8'::('6'::('M'::('O'::('V'::('Z'::('X'::('3'::('2'::(':'::(' '::('d'::('e'::('s'::('t'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))
             in
             Error s)
        | _ ->
          let s =
            E.error ii
              ('O'::('x'::('8'::('6'::('M'::('O'::('V'::('Z'::('X'::('3'::('2'::(':'::(' '::('d'::('e'::('s'::('t'::('i'::('n'::('a'::('t'::('i'::('o'::('n'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))
          in
          Error s))

(** val eqC_x86_extra_op : x86_extra_op eqTypeC **)

let eqC_x86_extra_op =
  { beq = x86_extra_op_beq; ceqP = x86_extra_op_eq_axiom }

(** val x86_extra_op_decl : x86_extra_op asmOp **)

let x86_extra_op_decl =
  { _eqT = eqC_x86_extra_op; asm_op_instr = get_instr_desc;
    Sopn.prim_string = prim_string }

(** val x86_extra :
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) asm_extra **)

let x86_extra =
  { _asm = x86; _extra = x86_extra_op_decl; to_asm = assemble_extra }

type x86_extended_op =
  (register, register_ext, xmm_register, rflag, condt, x86_op, x86_extra_op)
  extended_op

(** val coq_Ox86 : x86_op -> x86_extended_op sopn **)

let coq_Ox86 o =
  Oasm (BaseOp (None, o))
