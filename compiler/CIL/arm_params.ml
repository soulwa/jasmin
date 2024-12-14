open BinInt
open BinNums
open Datatypes
open Arch_decl
open Arch_extra
open Arch_params
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Arm_lowering
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Linearization
open Lowering
open Sopn
open Ssrbool
open Stack_alloc
open Type
open Utils0
open Word0
open Wsize

type __ = Obj.t

(** val addi :
    lval -> assgn_tag -> pexpr -> coq_Z -> arm_extended_op instr_r **)

let addi x tag y ofs =
  let eofs = Papp1 ((Oword_of_int arm_decl.reg_size), (Pconst ofs)) in
  Copn ((x :: []), tag, (coq_Oarm (ARM_op (ADD, default_opts))),
  (y :: (eofs :: [])))

(** val arm_mov_ofs :
    lval -> assgn_tag -> vptr_kind -> pexpr -> coq_Z -> (register, __, __,
    rflag, condt, arm_op, __) extended_op instr_r option **)

let arm_mov_ofs x tag _ y ofs =
  Some (addi x tag y ofs)

(** val arm_saparams :
    (register, __, __, rflag, condt, arm_op, __) extended_op
    stack_alloc_params **)

let arm_saparams =
  arm_mov_ofs

(** val arm_allocate_stack_frame :
    var_i -> coq_Z -> (lval list * arm_extended_op sopn) * pexpr list **)

let arm_allocate_stack_frame rspi sz =
  let rspg = { gv = rspi; gs = Slocal } in
  let esz = Papp1 ((Oword_of_int arm_decl.reg_size), (Pconst sz)) in
  ((((Lvar rspi) :: []), (coq_Oarm (ARM_op (SUB, default_opts)))), ((Pvar
  rspg) :: (esz :: [])))

(** val arm_free_stack_frame :
    var_i -> coq_Z -> (lval list * arm_extended_op sopn) * pexpr list **)

let arm_free_stack_frame rspi sz =
  let rspg = { gv = rspi; gs = Slocal } in
  let esz = Papp1 ((Oword_of_int arm_decl.reg_size), (Pconst sz)) in
  ((((Lvar rspi) :: []), (coq_Oarm (ARM_op (ADD, default_opts)))), ((Pvar
  rspg) :: (esz :: [])))

(** val arm_ensure_rsp_alignment :
    var_i -> wsize -> (lval list * arm_extended_op sopn) * pexpr list **)

let arm_ensure_rsp_alignment rspi al =
  let p0 = Pvar { gv = rspi; gs = Slocal } in
  let p1 = Papp1 ((Oword_of_int arm_decl.reg_size), (Pconst
    (Z.opp (wsize_size al))))
  in
  ((((Lvar rspi) :: []), (coq_Oarm (ARM_op (AND, default_opts)))),
  (p0 :: (p1 :: [])))

(** val arm_lassign :
    lval -> wsize -> pexpr -> ((lval list * (register, __, __, rflag, condt,
    arm_op, __) extended_op sopn) * pexpr list) option **)

let arm_lassign lv ws e =
  let args =
    match lv with
    | Lnone (_, _) -> None
    | Lvar _ ->
      (match ws with
       | U8 -> None
       | U16 -> None
       | U32 ->
         (match e with
          | Pconst _ -> None
          | Pbool _ -> None
          | Parr_init _ -> None
          | Pvar _ -> Some (MOV, e)
          | Pload (_, _, _) -> Some (LDR, e)
          | _ -> None)
       | _ -> None)
    | Lmem (_, _, _) ->
      (match store_mn_of_wsize ws with
       | Some mn -> Some (mn, e)
       | None -> None)
    | _ -> None
  in
  (match args with
   | Some p ->
     let (mn, e') = p in
     Some (((lv :: []), (coq_Oarm (ARM_op (mn, default_opts)))), (e' :: []))
   | None -> None)

(** val arm_liparams :
    (register, __, __, rflag, condt, arm_op, __) extended_op
    linearization_params **)

let arm_liparams =
  { lip_tmp = (Obj.magic ('r'::('1'::('2'::[])))); lip_allocate_stack_frame =
    arm_allocate_stack_frame; lip_free_stack_frame = arm_free_stack_frame;
    lip_ensure_rsp_alignment = arm_ensure_rsp_alignment; lip_lassign =
    arm_lassign }

(** val arm_fvars_correct :
    fresh_vars -> Equality.coq_type -> progT -> (register, __, __, rflag,
    condt, arm_op, __) extended_op fun_decl list -> bool **)

let arm_fvars_correct fv eft pT fds =
  fvars_correct (asm_opI arm_extra) eft pT (all_fresh_vars fv) (fvars fv) fds

(** val arm_loparams :
    ((register, __, __, rflag, condt, arm_op, __) extended_op, fresh_vars,
    lowering_options) lowering_params **)

let arm_loparams =
  { lop_lower_i = (fun _ _ -> lower_i); lop_fvars_correct =
    arm_fvars_correct }

(** val condt_of_rflag : rflag -> condt **)

let condt_of_rflag = function
| NF -> MI_ct
| ZF -> EQ_ct
| CF -> CS_ct
| VF -> VS_ct

(** val condt_not : condt -> condt **)

let condt_not = function
| EQ_ct -> NE_ct
| NE_ct -> EQ_ct
| CS_ct -> CC_ct
| CC_ct -> CS_ct
| MI_ct -> PL_ct
| PL_ct -> MI_ct
| VS_ct -> VC_ct
| VC_ct -> VS_ct
| HI_ct -> LS_ct
| LS_ct -> HI_ct
| GE_ct -> LT_ct
| LT_ct -> GE_ct
| GT_ct -> LE_ct
| LE_ct -> GT_ct

(** val condt_and : condt -> condt -> condt option **)

let condt_and c0 c1 =
  match c0 with
  | NE_ct ->
    (match c1 with
     | CS_ct -> Some HI_ct
     | GE_ct -> Some GT_ct
     | _ -> None)
  | CS_ct -> (match c1 with
              | NE_ct -> Some HI_ct
              | _ -> None)
  | GE_ct -> (match c1 with
              | NE_ct -> Some GT_ct
              | _ -> None)
  | _ -> None

(** val condt_or : condt -> condt -> condt option **)

let condt_or c0 c1 =
  match c0 with
  | EQ_ct ->
    (match c1 with
     | CC_ct -> Some LS_ct
     | LT_ct -> Some LE_ct
     | _ -> None)
  | CC_ct -> (match c1 with
              | EQ_ct -> Some LS_ct
              | _ -> None)
  | LT_ct -> (match c1 with
              | EQ_ct -> Some LE_ct
              | _ -> None)
  | _ -> None

(** val is_rflags_GE : rflag -> rflag -> bool **)

let is_rflags_GE r0 r1 =
  match r0 with
  | NF -> (match r1 with
           | VF -> true
           | _ -> false)
  | VF -> (match r1 with
           | NF -> true
           | _ -> false)
  | _ -> false

(** val assemble_cond : instr_info -> pexpr -> condt cexec **)

let rec assemble_cond ii e = match e with
| Pvar v ->
  (match of_var_e Coq_sbool rflag_toS ii v.gv with
   | Ok x -> Ok (condt_of_rflag x)
   | Error s -> Error s)
| Papp1 (s, e0) ->
  (match s with
   | Onot ->
     (match assemble_cond ii e0 with
      | Ok x -> Ok (condt_not x)
      | Error s0 -> Error s0)
   | _ ->
     Error
       (Asm_gen.E.berror ii e
         ('C'::('a'::('n'::('\''::('t'::(' '::('a'::('s'::('s'::('e'::('m'::('b'::('l'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('.'::[])))))))))))))))))))))))))))
| Papp2 (s, e0, e1) ->
  (match s with
   | Obeq ->
     (match e0 with
      | Pvar x0 ->
        (match e1 with
         | Pvar x1 ->
           (match of_var_e Coq_sbool rflag_toS ii x0.gv with
            | Ok x ->
              (match of_var_e Coq_sbool rflag_toS ii x1.gv with
               | Ok x2 ->
                 if is_rflags_GE x x2
                 then Ok GE_ct
                 else Error
                        (Asm_gen.E.berror ii e
                          ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('E'::('Q'::(')'::('.'::[]))))))))))))))))))))))))
               | Error s0 -> Error s0)
            | Error s0 -> Error s0)
         | _ ->
           Error
             (Asm_gen.E.berror ii e
               ('C'::('a'::('n'::('\''::('t'::(' '::('a'::('s'::('s'::('e'::('m'::('b'::('l'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('.'::[])))))))))))))))))))))))))))
      | _ ->
        Error
          (Asm_gen.E.berror ii e
            ('C'::('a'::('n'::('\''::('t'::(' '::('a'::('s'::('s'::('e'::('m'::('b'::('l'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('.'::[])))))))))))))))))))))))))))
   | Oand ->
     (match assemble_cond ii e0 with
      | Ok x ->
        (match assemble_cond ii e1 with
         | Ok x0 ->
           (match condt_and x x0 with
            | Some ct -> Ok ct
            | None ->
              Error
                (Asm_gen.E.berror ii e
                  ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('A'::('N'::('D'::(')'::[])))))))))))))))))))))))))
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | Oor ->
     (match assemble_cond ii e0 with
      | Ok x ->
        (match assemble_cond ii e1 with
         | Ok x0 ->
           (match condt_or x x0 with
            | Some ct -> Ok ct
            | None ->
              Error
                (Asm_gen.E.berror ii e
                  ('I'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('('::('O'::('R'::(')'::[]))))))))))))))))))))))))
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | _ ->
     Error
       (Asm_gen.E.berror ii e
         ('C'::('a'::('n'::('\''::('t'::(' '::('a'::('s'::('s'::('e'::('m'::('b'::('l'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('.'::[])))))))))))))))))))))))))))
| _ ->
  Error
    (Asm_gen.E.berror ii e
      ('C'::('a'::('n'::('\''::('t'::(' '::('a'::('s'::('s'::('e'::('m'::('b'::('l'::('e'::(' '::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::('.'::[]))))))))))))))))))))))))))

(** val arm_agparams :
    (register, __, __, rflag, condt, arm_op, __) asm_gen_params **)

let arm_agparams =
  assemble_cond

(** val arm_is_move_op :
    (register, __, __, rflag, condt, arm_op, __) extended_op asm_op_t -> bool **)

let arm_is_move_op = function
| BaseOp a ->
  let (o0, a0) = a in
  (match o0 with
   | Some _ -> false
   | None ->
     let ARM_op (a1, opts) = a0 in
     (match a1 with
      | MOV ->
        (&&) (negb opts.set_flags)
          ((&&) (negb opts.is_conditional)
            (negb (Ssrbool.isSome opts.has_shift)))
      | _ -> false))
| ExtOp _ -> false

(** val arm_params :
    (register, __, __, rflag, condt, arm_op, __, fresh_vars,
    lowering_options) architecture_params **)

let arm_params =
  { ap_sap = (fun _ -> arm_saparams); ap_lip = arm_liparams; ap_lop =
    arm_loparams; ap_agp = arm_agparams; ap_is_move_op = arm_is_move_op }
