open BinInt
open BinNums
open List0
open Arch_decl
open Arch_extra
open Arch_params
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Linearization
open Seq
open Sopn
open Stack_alloc
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize
open X86_decl
open X86_extra
open X86_instr_decl
open X86_lowering

(** val lea_ptr :
    lval -> pexpr -> assgn_tag -> coq_Z -> (register, register_ext,
    xmm_register, rflag, condt, x86_op, x86_extra_op) extended_op instr_r **)

let lea_ptr x y tag ofs =
  Copn ((x :: []), tag, (coq_Ox86 (LEA (coq_Uptr (arch_pd x86_decl)))),
    ((add (arch_pd x86_decl) y (cast_const (arch_pd x86_decl) ofs)) :: []))

type mov_kind =
| MK_LEA
| MK_MOV

(** val mk_mov : vptr_kind -> mov_kind **)

let mk_mov = function
| VKglob _ -> MK_LEA
| VKptr p ->
  (match p with
   | Pdirect (_, _, _, _, v0) ->
     (match v0 with
      | Slocal -> MK_MOV
      | Sglob -> MK_LEA)
   | _ -> MK_MOV)

(** val x86_mov_ofs :
    (Var.var -> bool) -> lval -> assgn_tag -> vptr_kind -> pexpr -> coq_Z ->
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op instr_r option **)

let x86_mov_ofs is_regx x tag vpk y ofs =
  let addr =
    match mk_mov vpk with
    | MK_LEA -> lea_ptr x y tag ofs
    | MK_MOV ->
      if eq_op coq_Z_eqType (Obj.magic ofs) (Obj.magic Z0)
      then mov_ws is_regx (coq_Uptr (arch_pd x86_decl)) x y tag
      else lea_ptr x y tag ofs
  in
  Some addr

(** val x86_saparams :
    (Var.var -> bool) -> (register, register_ext, xmm_register, rflag, condt,
    x86_op, x86_extra_op) extended_op stack_alloc_params **)

let x86_saparams =
  x86_mov_ofs

(** val x86_allocate_stack_frame :
    var_i -> coq_Z -> (lval list * x86_extended_op sopn) * pexpr list **)

let x86_allocate_stack_frame rspi sz =
  let rspg = { gv = rspi; gs = Slocal } in
  let p = Papp2 ((Osub (Op_w (coq_Uptr (arch_pd x86_decl)))), (Pvar rspg),
    (cast_const (arch_pd x86_decl) sz))
  in
  ((((Lvar rspi) :: []), (coq_Ox86 (LEA (coq_Uptr (arch_pd x86_decl))))),
  (p :: []))

(** val x86_free_stack_frame :
    var_i -> coq_Z -> (lval list * x86_extended_op sopn) * pexpr list **)

let x86_free_stack_frame rspi sz =
  let rspg = { gv = rspi; gs = Slocal } in
  let p = Papp2 ((Oadd (Op_w (coq_Uptr (arch_pd x86_decl)))), (Pvar rspg),
    (cast_const (arch_pd x86_decl) sz))
  in
  ((((Lvar rspi) :: []), (coq_Ox86 (LEA (coq_Uptr (arch_pd x86_decl))))),
  (p :: []))

(** val x86_ensure_rsp_alignment :
    var_i -> wsize -> (lval list * x86_extended_op sopn) * pexpr list **)

let x86_ensure_rsp_alignment rspi al =
  let to_lvar = fun x -> Lvar { v_var = (to_var Coq_sbool x86_rflag_toS x);
    v_info = dummy_var_info }
  in
  let eflags = List0.map to_lvar (OF :: (CF :: (SF :: (PF :: (ZF :: []))))) in
  let p0 = Pvar { gv = rspi; gs = Slocal } in
  let p1 = Papp1 ((Oword_of_int (coq_Uptr (arch_pd x86_decl))), (Pconst
    (Z.opp (wsize_size al))))
  in
  (((cat eflags ((Lvar rspi) :: [])),
  (coq_Ox86 (AND (coq_Uptr (arch_pd x86_decl))))), (p0 :: (p1 :: [])))

(** val x86_lassign :
    lval -> wsize -> pexpr -> ((lval list * x86_extended_op sopn) * pexpr
    list) option **)

let x86_lassign x ws e =
  let op = if cmp_le wsize_cmp ws U64 then MOV ws else VMOVDQU ws in
  Some (((x :: []), (coq_Ox86 op)), (e :: []))

(** val x86_liparams :
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op linearization_params **)

let x86_liparams =
  { lip_tmp = (Obj.magic ('R'::('A'::('X'::[])))); lip_allocate_stack_frame =
    x86_allocate_stack_frame; lip_free_stack_frame = x86_free_stack_frame;
    lip_ensure_rsp_alignment = x86_ensure_rsp_alignment; lip_lassign =
    x86_lassign }

(** val x86_loparams :
    ((register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op, fresh_vars, lowering_options) lowering_params **)

let x86_loparams =
  { lop_lower_i = lower_i; lop_fvars_correct = fvars_correct }

(** val not_condt : condt -> condt **)

let not_condt = function
| O_ct -> NO_ct
| NO_ct -> O_ct
| B_ct -> NB_ct
| NB_ct -> B_ct
| E_ct -> NE_ct
| NE_ct -> E_ct
| BE_ct -> NBE_ct
| NBE_ct -> BE_ct
| S_ct -> NS_ct
| NS_ct -> S_ct
| P_ct -> NP_ct
| NP_ct -> P_ct
| L_ct -> NL_ct
| NL_ct -> L_ct
| LE_ct -> NLE_ct
| NLE_ct -> LE_ct

(** val or_condt : instr_info -> pexpr -> condt -> condt -> condt cexec **)

let or_condt ii e c1 c2 =
  match c1 with
  | B_ct ->
    (match c2 with
     | E_ct -> Ok BE_ct
     | _ ->
       Error
         (Asm_gen.E.berror ii e
           ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('O'::('R'::(')'::[]))))))))))))))))))))))))
  | E_ct ->
    (match c2 with
     | B_ct -> Ok BE_ct
     | L_ct -> Ok LE_ct
     | _ ->
       Error
         (Asm_gen.E.berror ii e
           ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('O'::('R'::(')'::[]))))))))))))))))))))))))
  | L_ct ->
    (match c2 with
     | E_ct -> Ok LE_ct
     | _ ->
       Error
         (Asm_gen.E.berror ii e
           ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('O'::('R'::(')'::[]))))))))))))))))))))))))
  | _ ->
    Error
      (Asm_gen.E.berror ii e
        ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('O'::('R'::(')'::[])))))))))))))))))))))))

(** val and_condt :
    instr_info -> pexpr -> condt -> condt -> (pp_error_loc, condt) result **)

let and_condt ii e c1 c2 =
  match c1 with
  | NB_ct ->
    (match c2 with
     | NE_ct -> Ok NBE_ct
     | _ ->
       Error
         (Asm_gen.E.berror ii e
           ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('A'::('N'::('D'::(')'::[])))))))))))))))))))))))))
  | NE_ct ->
    (match c2 with
     | NB_ct -> Ok NBE_ct
     | NL_ct -> Ok NLE_ct
     | _ ->
       Error
         (Asm_gen.E.berror ii e
           ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('A'::('N'::('D'::(')'::[])))))))))))))))))))))))))
  | NL_ct ->
    (match c2 with
     | NE_ct -> Ok NLE_ct
     | _ ->
       Error
         (Asm_gen.E.berror ii e
           ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('A'::('N'::('D'::(')'::[])))))))))))))))))))))))))
  | _ ->
    Error
      (Asm_gen.E.berror ii e
        ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('A'::('N'::('D'::(')'::[]))))))))))))))))))))))))

(** val of_var_e_bool : instr_info -> var_i -> rflag cexec **)

let of_var_e_bool ii v =
  match of_var Coq_sbool x86_rflag_toS v.v_var with
  | Some r -> Ok r
  | None -> Error (Asm_gen.E.invalid_flag ii v)

(** val assemble_cond_r : instr_info -> pexpr -> condt cexec **)

let rec assemble_cond_r ii e = match e with
| Pvar v ->
  (match of_var_e_bool ii v.gv with
   | Ok x ->
     (match x with
      | CF -> Ok B_ct
      | PF -> Ok P_ct
      | ZF -> Ok E_ct
      | SF -> Ok S_ct
      | OF -> Ok O_ct
      | DF ->
        Error
          (Asm_gen.E.berror ii e
            ('C'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('r'::('a'::('n'::('c'::('h'::(' '::('o'::('n'::(' '::('D'::('F'::[])))))))))))))))))))))
   | Error s -> Error s)
| Papp1 (s, e0) ->
  (match s with
   | Onot ->
     (match assemble_cond_r ii e0 with
      | Ok x -> Ok (not_condt x)
      | Error s0 -> Error s0)
   | _ ->
     Error
       (Asm_gen.E.berror ii e
         ('d'::('o'::('n'::('\''::('t'::(' '::('k'::('n'::('o'::('w'::('n'::(' '::('h'::('o'::('w'::(' '::('t'::('o'::(' '::('c'::('o'::('m'::('p'::('i'::('l'::('e'::(' '::('t'::('h'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))))))
| Papp2 (s, e1, e2) ->
  (match s with
   | Obeq ->
     (match e1 with
      | Pvar x1 ->
        (match e2 with
         | Pvar x2 ->
           (match of_var_e_bool ii x1.gv with
            | Ok x ->
              (match of_var_e_bool ii x2.gv with
               | Ok x0 ->
                 if (||)
                      ((&&) (eq_op rflag_eqType (Obj.magic x) (Obj.magic SF))
                        (eq_op rflag_eqType (Obj.magic x0) (Obj.magic OF)))
                      ((&&) (eq_op rflag_eqType (Obj.magic x) (Obj.magic OF))
                        (eq_op rflag_eqType (Obj.magic x0) (Obj.magic SF)))
                 then Ok NL_ct
                 else Error
                        (Asm_gen.E.berror ii e
                          ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('N'::('L'::(')'::[])))))))))))))))))))))))
               | Error s0 -> Error s0)
            | Error s0 -> Error s0)
         | _ ->
           Error
             (Asm_gen.E.berror ii e
               ('d'::('o'::('n'::('\''::('t'::(' '::('k'::('n'::('o'::('w'::('n'::(' '::('h'::('o'::('w'::(' '::('t'::('o'::(' '::('c'::('o'::('m'::('p'::('i'::('l'::('e'::(' '::('t'::('h'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))))))
      | _ ->
        Error
          (Asm_gen.E.berror ii e
            ('d'::('o'::('n'::('\''::('t'::(' '::('k'::('n'::('o'::('w'::('n'::(' '::('h'::('o'::('w'::(' '::('t'::('o'::(' '::('c'::('o'::('m'::('p'::('i'::('l'::('e'::(' '::('t'::('h'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))))))
   | Oand ->
     (match assemble_cond_r ii e1 with
      | Ok x ->
        (match assemble_cond_r ii e2 with
         | Ok x0 -> and_condt ii e x x0
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | Oor ->
     (match assemble_cond_r ii e1 with
      | Ok x ->
        (match assemble_cond_r ii e2 with
         | Ok x0 -> or_condt ii e x x0
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | _ ->
     Error
       (Asm_gen.E.berror ii e
         ('d'::('o'::('n'::('\''::('t'::(' '::('k'::('n'::('o'::('w'::('n'::(' '::('h'::('o'::('w'::(' '::('t'::('o'::(' '::('c'::('o'::('m'::('p'::('i'::('l'::('e'::(' '::('t'::('h'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))))))
| _ ->
  Error
    (Asm_gen.E.berror ii e
      ('d'::('o'::('n'::('\''::('t'::(' '::('k'::('n'::('o'::('w'::('n'::(' '::('h'::('o'::('w'::(' '::('t'::('o'::(' '::('c'::('o'::('m'::('p'::('i'::('l'::('e'::(' '::('t'::('h'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))))))

(** val assemble_cond : instr_info -> pexpr -> condt cexec **)

let assemble_cond =
  assemble_cond_r

(** val x86_agparams :
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) asm_gen_params **)

let x86_agparams =
  assemble_cond

(** val x86_is_move_op :
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op) extended_op asm_op_t -> bool **)

let x86_is_move_op = function
| BaseOp a ->
  let (o0, x) = a in
  (match o0 with
   | Some _ -> false
   | None -> (match x with
              | MOV _ -> true
              | VMOVDQU _ -> true
              | _ -> false))
| ExtOp _ -> false

(** val x86_params :
    (register, register_ext, xmm_register, rflag, condt, x86_op,
    x86_extra_op, fresh_vars, lowering_options) architecture_params **)

let x86_params =
  { ap_sap = x86_saparams; ap_lip = x86_liparams; ap_lop = x86_loparams;
    ap_agp = x86_agparams; ap_is_move_op = x86_is_move_op }
