open BinInt
open BinNums
open BinPos
open Datatypes
open Compiler_util
open Constant_prop
open Eqtype
open Expr
open Label
open Linear
open Memory_model
open Seq
open Sopn
open Ssralg
open Ssrint
open Ssrnat
open Type
open Utils0
open Var0
open Word0
open Word_ssrZ
open Wsize

module E =
 struct
  (** val pass_name : char list **)

  let pass_name =
    'l'::('i'::('n'::('e'::('a'::('r'::('i'::('z'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))

  (** val my_error : pp_error -> pp_error_loc **)

  let my_error msg =
    { pel_msg = msg; pel_fn = None; pel_fi = None; pel_ii = None; pel_vi =
      None; pel_pass = (Some pass_name); pel_internal = false }

  (** val gen_error :
      bool -> instr_info option -> char list -> pp_error_loc **)

  let gen_error internal ii msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = None; pel_ii = ii;
      pel_vi = None; pel_pass = (Some pass_name); pel_internal = internal }

  (** val ii_error : instr_info -> char list -> pp_error_loc **)

  let ii_error ii msg =
    gen_error false (Some ii) msg

  (** val error : char list -> pp_error_loc **)

  let error msg =
    gen_error false None msg

  (** val internal_error : char list -> pp_error_loc **)

  let internal_error msg =
    gen_error true None msg
 end

type 'asm_op linearization_params = { lip_tmp : Equality.sort;
                                      lip_allocate_stack_frame : (var_i ->
                                                                 coq_Z ->
                                                                 (lval
                                                                 list * 'asm_op
                                                                 sopn) * pexpr
                                                                 list);
                                      lip_free_stack_frame : (var_i -> coq_Z
                                                             -> (lval
                                                             list * 'asm_op
                                                             sopn) * pexpr
                                                             list);
                                      lip_ensure_rsp_alignment : (var_i ->
                                                                 wsize ->
                                                                 (lval
                                                                 list * 'asm_op
                                                                 sopn) * pexpr
                                                                 list);
                                      lip_lassign : (lval -> wsize -> pexpr
                                                    -> ((lval list * 'asm_op
                                                    sopn) * pexpr list)
                                                    option) }

(** val lassign :
    'a1 asmOp -> 'a1 linearization_params -> lval -> wsize -> pexpr -> 'a1
    linstr_r option **)

let lassign _ liparams lv ws e =
  match liparams.lip_lassign lv ws e with
  | Some p ->
    let (p0, es) = p in let (lvs, op) = p0 in Some (Lopn (lvs, op, es))
  | None -> None

(** val lmove :
    'a1 asmOp -> 'a1 linearization_params -> var_i -> wsize -> gvar -> 'a1
    linstr_r option **)

let lmove asmop liparams rd ws r0 =
  lassign asmop liparams (Lvar rd) ws (Pvar r0)

(** val lload :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> var_i ->
    wsize -> var_i -> coq_Z -> 'a1 linstr_r option **)

let lload pd asmop liparams rd ws r0 ofs =
  lassign asmop liparams (Lvar rd) ws (Pload (ws, r0, (cast_const pd ofs)))

(** val lstore :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> var_i ->
    coq_Z -> wsize -> gvar -> 'a1 linstr_r option **)

let lstore pd asmop liparams rd ofs ws r0 =
  lassign asmop liparams (Lmem (ws, rd, (cast_const pd ofs))) ws (Pvar r0)

(** val mkli_dummy : 'a1 asmOp -> 'a1 linstr_r -> 'a1 linstr **)

let mkli_dummy _ lir =
  { li_ii = dummy_instr_info; li_i = lir }

(** val dummy_linstr : 'a1 asmOp -> 'a1 linstr **)

let dummy_linstr asmop =
  mkli_dummy asmop Lalign

(** val of_olinstr_r :
    'a1 asmOp -> instr_info -> 'a1 linstr_r option -> 'a1 linstr **)

let of_olinstr_r asmop ii = function
| Some lir -> { li_ii = ii; li_i = lir }
| None -> dummy_linstr asmop

(** val stack_frame_allocation_size : stk_fun_extra -> coq_Z **)

let stack_frame_allocation_size e =
  round_ws e.sf_align (Z.add e.sf_stk_sz e.sf_stk_extra_sz)

(** val check_c :
    'a1 asmOp -> ('a1 instr -> unit cexec) -> 'a1 instr list -> unit cexec **)

let rec check_c asmop check_i0 = function
| [] -> Ok ()
| i :: c0 ->
  (match check_c asmop check_i0 c0 with
   | Ok _ -> check_i0 i
   | Error s -> Error s)

(** val check_i :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (instr_info -> Var.var option) -> funname -> wsize -> 'a1 instr -> unit
    cexec **)

let rec check_i pd asmop liparams p extra_free_registers this stack_align = function
| MkI (ii, ir) ->
  (match ir with
   | Cassgn (x, _, ty, e) ->
     (match ty with
      | Coq_sword ws ->
        if isSome (lassign asmop liparams x ws e)
        then Ok ()
        else Error
               (E.ii_error ii
                 ('a'::('s'::('s'::('i'::('g'::('n'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))
      | _ ->
        Error
          (E.ii_error ii
            ('a'::('s'::('s'::('i'::('g'::('n'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('w'::('o'::('r'::('d'::[])))))))))))))))))))
   | Cif (_, c1, c2) ->
     (match check_c asmop
              (check_i pd asmop liparams p extra_free_registers this
                stack_align) c1 with
      | Ok _ ->
        check_c asmop
          (check_i pd asmop liparams p extra_free_registers this stack_align)
          c2
      | Error s -> Error s)
   | Cfor (_, _, _) ->
     Error
       (E.ii_error ii
         ('f'::('o'::('r'::(' '::('f'::('o'::('u'::('n'::('d'::(' '::('i'::('n'::(' '::('l'::('i'::('n'::('e'::('a'::('r'::[]))))))))))))))))))))
   | Cwhile (_, c, e, c') ->
     if is_false e
     then check_c asmop
            (check_i pd asmop liparams p extra_free_registers this
              stack_align) c
     else (match check_c asmop
                   (check_i pd asmop liparams p extra_free_registers this
                     stack_align) c with
           | Ok _ ->
             check_c asmop
               (check_i pd asmop liparams p extra_free_registers this
                 stack_align) c'
           | Error s -> Error s)
   | Ccall (_, _, fn, _) ->
     if negb (eq_op pos_eqType (Obj.magic fn) (Obj.magic this))
     then (match get_fundef p.p_funcs fn with
           | Some fd ->
             let e = fd.f_extra in
             if match (Obj.magic e).sf_return_address with
                | RAnone -> false
                | RAreg _ -> true
                | RAstack ofs ->
                  (match extra_free_registers ii with
                   | Some ra ->
                     let rag = { gv = { v_var = ra; v_info =
                       dummy_var_info }; gs = Slocal }
                     in
                     isSome
                       (lstore pd asmop liparams { v_var = { Var.vtype =
                         (Coq_sword (coq_Uptr pd)); Var.vname =
                         (Obj.magic p).p_extra.sp_rsp }; v_info =
                         dummy_var_info } ofs (coq_Uptr pd) rag)
                   | None -> false)
             then if cmp_le wsize_cmp (Obj.magic e).sf_align stack_align
                  then Ok ()
                  else let s =
                         E.ii_error ii
                           ('c'::('a'::('l'::('l'::('e'::('r'::(' '::('n'::('e'::('e'::('d'::(' '::('a'::('l'::('i'::('g'::('n'::('m'::('e'::('n'::('t'::(' '::('g'::('r'::('e'::('a'::('t'::('e'::('r'::(' '::('t'::('h'::('a'::('n'::(' '::('c'::('a'::('l'::('l'::('e'::('e'::[])))))))))))))))))))))))))))))))))))))))))
                       in
                       Error s
             else let s =
                    E.ii_error ii
                      ('('::('o'::('n'::('e'::('_'::('v'::('a'::('r'::('m'::('a'::('p'::(')'::(' '::('n'::('o'::('w'::('h'::('e'::('r'::('e'::(' '::('t'::('o'::(' '::('s'::('t'::('o'::('r'::('e'::(' '::('t'::('h'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('a'::('d'::('d'::('r'::('e'::('s'::('s'::[]))))))))))))))))))))))))))))))))))))))))))))))))
                  in
                  Error s
           | None ->
             Error
               (E.ii_error ii
                 ('c'::('a'::('l'::('l'::(' '::('t'::('o'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))
     else let s =
            E.ii_error ii
              ('c'::('a'::('l'::('l'::(' '::('t'::('o'::(' '::('s'::('e'::('l'::('f'::[]))))))))))))
          in
          Error s
   | _ -> Ok ())

(** val all_disjoint_aligned_between :
    coq_PointerData -> coq_Z -> coq_Z -> wsize -> 'a1 list -> ('a1 ->
    (coq_Z * wsize) cexec) -> unit cexec **)

let all_disjoint_aligned_between pd lo hi al m slot =
  match foldM (fun a base ->
          match slot a with
          | Ok x ->
            let (ofs, ws) = x in
            if Z.leb base ofs
            then if cmp_le wsize_cmp ws al
                 then if is_align (GRing.ComRing.eqType (word (coq_Uptr pd)))
                           (coq_Pointer pd) (wrepr (coq_Uptr pd) ofs) ws
                      then Ok (Z.add ofs (wsize_size ws))
                      else let s =
                             E.error
                               ('t'::('o'::('-'::('s'::('a'::('v'::('e'::(':'::(' '::('b'::('a'::('d'::(' '::('s'::('l'::('o'::('t'::(' '::('a'::('l'::('i'::('g'::('n'::('e'::('m'::('e'::('n'::('t'::[]))))))))))))))))))))))))))))
                           in
                           Error s
                 else let s =
                        E.error
                          ('t'::('o'::('-'::('s'::('a'::('v'::('e'::(':'::(' '::('b'::('a'::('d'::(' '::('f'::('r'::('a'::('m'::('e'::(' '::('a'::('l'::('i'::('g'::('n'::('e'::('m'::('e'::('n'::('t'::[])))))))))))))))))))))))))))))
                      in
                      Error s
            else let s =
                   E.my_error
                     (pp_hov ((PPEstring
                       ('t'::('o'::('-'::('s'::('a'::('v'::('e'::(':'::(' '::('o'::('v'::('e'::('r'::('l'::('a'::('p'::[]))))))))))))))))) :: ((PPEexpr
                       (Pconst base)) :: ((PPEexpr (Pconst ofs)) :: []))))
                 in
                 Error s
          | Error s -> Error s) lo m with
  | Ok x ->
    if Z.leb x hi
    then Ok ()
    else Error
           (E.error
             ('t'::('o'::('-'::('s'::('a'::('v'::('e'::(':'::(' '::('o'::('v'::('e'::('r'::('f'::('l'::('o'::('w'::(' '::('i'::('n'::(' '::('t'::('h'::('e'::(' '::('s'::('t'::('a'::('c'::('k'::(' '::('f'::('r'::('a'::('m'::('e'::[])))))))))))))))))))))))))))))))))))))
  | Error s -> Error s

(** val check_to_save_slot :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (Var.var * coq_Z) -> (coq_Z * wsize) cexec **)

let check_to_save_slot pd asmop liparams p = function
| (x, ofs) ->
  (match is_word_type (Var.vtype x) with
   | Some ws ->
     let xi = { v_var = x; v_info = dummy_var_info } in
     let xg = { gv = xi; gs = Slocal } in
     if (&&)
          (isSome
            (lload pd asmop liparams xi ws { v_var = { Var.vtype = (Coq_sword
              (coq_Uptr pd)); Var.vname = (Obj.magic p).p_extra.sp_rsp };
              v_info = dummy_var_info } ofs))
          (isSome
            (lstore pd asmop liparams { v_var = { Var.vtype = (Coq_sword
              (coq_Uptr pd)); Var.vname = (Obj.magic p).p_extra.sp_rsp };
              v_info = dummy_var_info } ofs ws xg))
     then Ok (ofs, ws)
     else let s =
            E.error
              ('t'::('o'::('-'::('s'::('a'::('v'::('e'::(':'::(' '::('c'::('a'::('n'::('\''::('t'::(' '::('p'::('u'::('s'::('h'::('/'::('p'::('o'::('p'::(' '::('t'::('o'::(' '::('s'::('t'::('a'::('c'::('k'::[]))))))))))))))))))))))))))))))))
          in
          Error s
   | None ->
     Error
       (E.error
         ('t'::('o'::('-'::('s'::('a'::('v'::('e'::(':'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('w'::('o'::('r'::('d'::[])))))))))))))))))))))

(** val check_to_save :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    stk_fun_extra -> unit cexec **)

let check_to_save pd asmop liparams p e =
  match e.sf_return_address with
  | RAnone ->
    let stk_size = Z.add e.sf_stk_sz e.sf_stk_extra_sz in
    if match e.sf_save_stack with
       | SavedStackStk ofs ->
         Z.leb (Z.add ofs (wsize_size (coq_Uptr pd))) stk_size
       | _ -> true
    then all_disjoint_aligned_between pd e.sf_stk_sz
           (match e.sf_save_stack with
            | SavedStackStk ofs -> ofs
            | _ -> Z.add e.sf_stk_sz e.sf_stk_extra_sz) e.sf_align
           e.sf_to_save (check_to_save_slot pd asmop liparams p)
    else let s =
           E.error
             ('s'::('t'::('a'::('c'::('k'::(' '::('s'::('i'::('z'::('e'::(' '::('t'::('o'::(' '::('s'::('m'::('a'::('l'::('l'::[])))))))))))))))))))
         in
         Error s
  | _ -> Ok ()

(** val linear_c :
    'a1 asmOp -> ('a1 instr -> label -> 'a1 lcmd -> label * 'a1 lcmd) -> 'a1
    instr list -> label -> 'a1 lcmd -> label * 'a1 lcmd **)

let rec linear_c asmop linear_i0 c lbl lc =
  match c with
  | [] -> (lbl, lc)
  | i :: c0 ->
    let (lbl0, lc0) = linear_c asmop linear_i0 c0 lbl lc in
    linear_i0 i lbl0 lc0

(** val next_lbl : positive -> positive **)

let next_lbl lbl =
  Pos.add lbl Coq_xH

(** val add_align :
    'a1 asmOp -> instr_info -> align -> 'a1 lcmd -> 'a1 linstr list **)

let add_align _ ii a lc =
  match a with
  | Align -> { li_ii = ii; li_i = Lalign } :: lc
  | NoAlign -> lc

(** val align :
    'a1 asmOp -> instr_info -> align -> (label * 'a1 lcmd) -> label * 'a1 lcmd **)

let align asmop ii a p =
  ((fst p), (add_align asmop ii a (snd p)))

(** val check_fd :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (instr_info -> Var.var option) -> funname -> 'a1 sfundef ->
    (pp_error_loc, unit) result **)

let check_fd pd asmop liparams p extra_free_registers =
  let check_stack_ofs = fun e ofs ws ->
    (&&) (Z.leb e.sf_stk_sz ofs)
      ((&&)
        (Z.leb (Z.add ofs (wsize_size ws))
          (Z.add e.sf_stk_sz e.sf_stk_extra_sz))
        ((&&) (cmp_le wsize_cmp ws e.sf_align)
          (is_align (GRing.ComRing.eqType (word (coq_Uptr pd)))
            (coq_Pointer pd) (wrepr (coq_Uptr pd) ofs) ws)))
  in
  (fun fn fd ->
  let e = fd.f_extra in
  let stack_align = (Obj.magic e).sf_align in
  (match check_c asmop
           (check_i pd asmop liparams p extra_free_registers fn stack_align)
           fd.f_body with
   | Ok _ ->
     (match check_to_save pd asmop liparams p (Obj.magic e) with
      | Ok _ ->
        if (&&) (Z.leb Z0 (Obj.magic e).sf_stk_sz)
             ((&&) (Z.leb Z0 (Obj.magic e).sf_stk_extra_sz)
               (Z.ltb (stack_frame_allocation_size (Obj.magic e))
                 (wbase (coq_Uptr pd))))
        then if match (Obj.magic e).sf_return_address with
                | RAnone -> true
                | RAreg ra ->
                  eq_op stype_eqType (Obj.magic Var.vtype ra)
                    (Obj.magic (Coq_sword (coq_Uptr pd)))
                | RAstack ofs -> Obj.magic check_stack_ofs e ofs (coq_Uptr pd)
             then let ok_save_stack =
                    match (Obj.magic e).sf_save_stack with
                    | SavedStackNone ->
                      (&&)
                        (eq_op
                          (seq_eqType
                            (prod_eqType Var.var_eqType coq_Z_eqType))
                          (Obj.magic (Obj.magic e).sf_to_save) (Obj.magic []))
                        ((&&)
                          (eq_op wsize_eqType (Obj.magic stack_align)
                            (Obj.magic U8))
                          ((&&)
                            (eq_op coq_Z_eqType
                              (Obj.magic (Obj.magic e).sf_stk_sz)
                              (Obj.magic int_to_Z (Posz O)))
                            (eq_op coq_Z_eqType
                              (Obj.magic (Obj.magic e).sf_stk_extra_sz)
                              (Obj.magic int_to_Z (Posz O)))))
                    | SavedStackReg x ->
                      let xi = { v_var = x; v_info = dummy_var_info } in
                      let xg = { gv = xi; gs = Slocal } in
                      (&&)
                        (eq_op stype_eqType (Obj.magic Var.vtype x)
                          (Obj.magic (Coq_sword (coq_Uptr pd))))
                        ((&&)
                          (eq_op
                            (seq_eqType
                              (prod_eqType Var.var_eqType coq_Z_eqType))
                            (Obj.magic (Obj.magic e).sf_to_save)
                            (Obj.magic []))
                          ((&&)
                            (isSome
                              (lmove asmop liparams { v_var = { Var.vtype =
                                (Coq_sword (coq_Uptr pd)); Var.vname =
                                (Obj.magic p).p_extra.sp_rsp }; v_info =
                                dummy_var_info } (coq_Uptr pd) xg))
                            (isSome
                              (lmove asmop liparams xi (coq_Uptr pd) { gv =
                                { v_var = { Var.vtype = (Coq_sword
                                (coq_Uptr pd)); Var.vname =
                                (Obj.magic p).p_extra.sp_rsp }; v_info =
                                dummy_var_info }; gs = Slocal }))))
                    | SavedStackStk ofs ->
                      (&&) (Obj.magic check_stack_ofs e ofs (coq_Uptr pd))
                        ((&&)
                          (negb
                            (SvExtra.Sv.mem
                              (Obj.magic { Var.vtype = (Coq_sword
                                (coq_Uptr pd)); Var.vname =
                                liparams.lip_tmp })
                              (SvExtra.sv_of_list (Obj.magic fst)
                                (Obj.magic e).sf_to_save)))
                          ((&&)
                            (isSome
                              (lload pd asmop liparams { v_var =
                                { Var.vtype = (Coq_sword (coq_Uptr pd));
                                Var.vname = (Obj.magic p).p_extra.sp_rsp };
                                v_info = dummy_var_info } (coq_Uptr pd)
                                { v_var = { Var.vtype = (Coq_sword
                                (coq_Uptr pd)); Var.vname =
                                (Obj.magic p).p_extra.sp_rsp }; v_info =
                                dummy_var_info } ofs))
                            ((&&)
                              (isSome
                                (lmove asmop liparams { v_var = { Var.vtype =
                                  (Coq_sword (coq_Uptr pd)); Var.vname =
                                  liparams.lip_tmp }; v_info =
                                  dummy_var_info } (coq_Uptr pd) { gv =
                                  { v_var = { Var.vtype = (Coq_sword
                                  (coq_Uptr pd)); Var.vname =
                                  (Obj.magic p).p_extra.sp_rsp }; v_info =
                                  dummy_var_info }; gs = Slocal }))
                              (isSome
                                (lstore pd asmop liparams { v_var =
                                  { Var.vtype = (Coq_sword (coq_Uptr pd));
                                  Var.vname = (Obj.magic p).p_extra.sp_rsp };
                                  v_info = dummy_var_info } ofs (coq_Uptr pd)
                                  { gv = { v_var = { Var.vtype = (Coq_sword
                                  (coq_Uptr pd)); Var.vname =
                                  liparams.lip_tmp }; v_info =
                                  dummy_var_info }; gs = Slocal })))))
                  in
                  if (||)
                       (negb
                         (eq_op return_address_location_eqType
                           (Obj.magic (Obj.magic e).sf_return_address)
                           (Obj.magic RAnone))) ok_save_stack
                  then Ok ()
                  else let s =
                         E.error
                           ('b'::('a'::('d'::(' '::('s'::('a'::('v'::('e'::('-'::('s'::('t'::('a'::('c'::('k'::[]))))))))))))))
                       in
                       Error s
             else let s =
                    E.error
                      ('b'::('a'::('d'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::('-'::('a'::('d'::('d'::('r'::('e'::('s'::('s'::[]))))))))))))))))))
                  in
                  Error s
        else let s =
               E.error
                 ('b'::('a'::('d'::(' '::('s'::('t'::('a'::('c'::('k'::(' '::('s'::('i'::('z'::('e'::[]))))))))))))))
             in
             Error s
      | Error s -> Error s)
   | Error s -> Error s))

(** val check_prog :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (instr_info -> Var.var option) -> (pp_error_loc, unit) result **)

let check_prog pd asmop liparams p extra_free_registers =
  match map_cfprog_name_gen (fun x -> x.f_info)
          (check_fd pd asmop liparams p extra_free_registers) p.p_funcs with
  | Ok _ -> Ok ()
  | Error s -> Error s

(** val allocate_stack_frame :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    bool -> instr_info -> coq_Z -> 'a1 lcmd **)

let allocate_stack_frame pd _ liparams p free ii sz =
  if eq_op coq_Z_eqType (Obj.magic sz) (Obj.magic Z0)
  then []
  else let args =
         if free
         then liparams.lip_free_stack_frame { v_var = { Var.vtype =
                (Coq_sword (coq_Uptr pd)); Var.vname =
                (Obj.magic p).p_extra.sp_rsp }; v_info = dummy_var_info } sz
         else liparams.lip_allocate_stack_frame { v_var = { Var.vtype =
                (Coq_sword (coq_Uptr pd)); Var.vname =
                (Obj.magic p).p_extra.sp_rsp }; v_info = dummy_var_info } sz
       in
       { li_ii = ii; li_i = (Lopn ((fst (fst args)), (snd (fst args)),
       (snd args))) } :: []

(** val ensure_rsp_alignment :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    instr_info -> wsize -> 'a1 linstr **)

let ensure_rsp_alignment pd _ liparams p ii al =
  let args =
    liparams.lip_ensure_rsp_alignment { v_var = { Var.vtype = (Coq_sword
      (coq_Uptr pd)); Var.vname = (Obj.magic p).p_extra.sp_rsp }; v_info =
      dummy_var_info } al
  in
  { li_ii = ii; li_i = (Lopn ((fst (fst args)), (snd (fst args)),
  (snd args))) }

(** val push_to_save :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    instr_info -> (Var.var * coq_Z) list -> 'a1 lcmd **)

let push_to_save pd asmop liparams p ii to_save =
  let mkli = fun pat ->
    let (x, ofs) = pat in
    (match is_word_type (Var.vtype x) with
     | Some ws ->
       let xi = { v_var = x; v_info = dummy_var_info } in
       let xg = { gv = xi; gs = Slocal } in
       of_olinstr_r asmop ii
         (lstore pd asmop liparams { v_var = { Var.vtype = (Coq_sword
           (coq_Uptr pd)); Var.vname = (Obj.magic p).p_extra.sp_rsp };
           v_info = dummy_var_info } ofs ws xg)
     | None -> dummy_linstr asmop)
  in
  map mkli to_save

(** val pop_to_save :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    instr_info -> (Var.var * coq_Z) list -> 'a1 lcmd **)

let pop_to_save pd asmop liparams p ii to_save =
  let mkli = fun pat ->
    let (x, ofs) = pat in
    (match is_word_type (Var.vtype x) with
     | Some ws ->
       let xi = { v_var = x; v_info = dummy_var_info } in
       of_olinstr_r asmop ii
         (lload pd asmop liparams xi ws { v_var = { Var.vtype = (Coq_sword
           (coq_Uptr pd)); Var.vname = (Obj.magic p).p_extra.sp_rsp };
           v_info = dummy_var_info } ofs)
     | None -> dummy_linstr asmop)
  in
  map mkli to_save

(** val linear_i :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (instr_info -> Var.var option) -> funname -> 'a1 instr -> label -> 'a1
    lcmd -> label * 'a1 lcmd **)

let rec linear_i pd asmop liparams p extra_free_registers fn i lbl lc =
  let MkI (ii, ir) = i in
  (match ir with
   | Cassgn (x, _, ty, e) ->
     let lc' =
       match ty with
       | Coq_sword sz ->
         (of_olinstr_r asmop ii (lassign asmop liparams x sz e)) :: lc
       | _ -> lc
     in
     (lbl, lc')
   | Copn (xs, _, o, es) ->
     (lbl, ({ li_ii = ii; li_i = (Lopn (xs, o, es)) } :: lc))
   | Csyscall (_, o, _) -> (lbl, ({ li_ii = ii; li_i = (Lsyscall o) } :: lc))
   | Cif (e, c1, c2) ->
     (match c1 with
      | [] ->
        let lbl0 = next_lbl lbl in
        let (lbl1, lc0) =
          linear_c asmop
            (linear_i pd asmop liparams p extra_free_registers fn) c2 lbl0
            ({ li_ii = ii; li_i = (Llabel lbl) } :: lc)
        in
        (lbl1, ({ li_ii = ii; li_i = (Lcond (e, lbl)) } :: lc0))
      | _ :: _ ->
        (match c2 with
         | [] ->
           let lbl0 = next_lbl lbl in
           let (lbl1, lc0) =
             linear_c asmop
               (linear_i pd asmop liparams p extra_free_registers fn) c1 lbl0
               ({ li_ii = ii; li_i = (Llabel lbl) } :: lc)
           in
           (lbl1, ({ li_ii = ii; li_i = (Lcond ((snot e), lbl)) } :: lc0))
         | _ :: _ ->
           let l2 = next_lbl lbl in
           let lbl0 = next_lbl l2 in
           let (lbl1, lc0) =
             let (lbl1, lc0) =
               let (lbl1, lc0) =
                 linear_c asmop
                   (linear_i pd asmop liparams p extra_free_registers fn) c1
                   lbl0 ({ li_ii = ii; li_i = (Llabel l2) } :: lc)
               in
               let lc1 = { li_ii = ii; li_i = (Llabel lbl) } :: lc0 in
               (lbl1, ({ li_ii = ii; li_i = (Lgoto (fn, l2)) } :: lc1))
             in
             linear_c asmop
               (linear_i pd asmop liparams p extra_free_registers fn) c2 lbl1
               lc0
           in
           (lbl1, ({ li_ii = ii; li_i = (Lcond (e, lbl)) } :: lc0))))
   | Cfor (_, _, _) -> (lbl, lc)
   | Cwhile (a, c, e, c') ->
     (match is_bool e with
      | Some b ->
        if b
        then let lbl0 = next_lbl lbl in
             align asmop ii a
               (let (lbl1, lc0) =
                  let (lbl1, lc0) =
                    linear_c asmop
                      (linear_i pd asmop liparams p extra_free_registers fn)
                      c' lbl0 ({ li_ii = ii; li_i = (Lgoto (fn, lbl)) } :: lc)
                  in
                  linear_c asmop
                    (linear_i pd asmop liparams p extra_free_registers fn) c
                    lbl1 lc0
                in
                (lbl1, ({ li_ii = ii; li_i = (Llabel lbl) } :: lc0)))
        else linear_c asmop
               (linear_i pd asmop liparams p extra_free_registers fn) c lbl lc
      | None ->
        (match c' with
         | [] ->
           let lbl0 = next_lbl lbl in
           align asmop ii a
             (let (lbl1, lc0) =
                linear_c asmop
                  (linear_i pd asmop liparams p extra_free_registers fn) c
                  lbl0 ({ li_ii = ii; li_i = (Lcond (e, lbl)) } :: lc)
              in
              (lbl1, ({ li_ii = ii; li_i = (Llabel lbl) } :: lc0)))
         | _ :: _ ->
           let l2 = next_lbl lbl in
           let lbl0 = next_lbl l2 in
           let (lbl1, lc0) =
             align asmop ii a
               (let (lbl1, lc0) =
                  let (lbl1, lc0) =
                    linear_c asmop
                      (linear_i pd asmop liparams p extra_free_registers fn)
                      c lbl0 ({ li_ii = ii; li_i = (Lcond (e, l2)) } :: lc)
                  in
                  let lc1 = { li_ii = ii; li_i = (Llabel lbl) } :: lc0 in
                  linear_c asmop
                    (linear_i pd asmop liparams p extra_free_registers fn) c'
                    lbl1 lc1
                in
                (lbl1, ({ li_ii = ii; li_i = (Llabel l2) } :: lc0)))
           in
           (lbl1, ({ li_ii = ii; li_i = (Lgoto (fn, lbl)) } :: lc0))))
   | Ccall (_, _, fn', _) ->
     (match get_fundef p.p_funcs fn' with
      | Some fd ->
        let e = fd.f_extra in
        let ra = (Obj.magic e).sf_return_address in
        if eq_op return_address_location_eqType (Obj.magic ra)
             (Obj.magic RAnone)
        then (lbl, lc)
        else let sz = stack_frame_allocation_size (Obj.magic e) in
             let before = allocate_stack_frame pd asmop liparams p false ii sz
             in
             let after = allocate_stack_frame pd asmop liparams p true ii sz
             in
             let lbl0 = next_lbl lbl in
             let lcall = (fn',
               (if eq_op pos_eqType (Obj.magic fn') (Obj.magic fn)
                then lbl
                else Coq_xH))
             in
             (match (Obj.magic e).sf_return_address with
              | RAnone -> (lbl0, lc)
              | RAreg ra0 ->
                (lbl0,
                  (cat before ({ li_ii = ii; li_i = (LstoreLabel (ra0,
                    lbl)) } :: ({ li_ii = ii; li_i = (Lgoto
                    lcall) } :: ({ li_ii = ii; li_i = (Llabel
                    lbl) } :: (cat after lc))))))
              | RAstack z ->
                (match extra_free_registers ii with
                 | Some ra0 ->
                   let glob_ra = { gv = { v_var = ra0; v_info =
                     dummy_var_info }; gs = Slocal }
                   in
                   (lbl0,
                   (cat before ({ li_ii = ii; li_i = (LstoreLabel (ra0,
                     lbl)) } :: ((of_olinstr_r asmop ii
                                   (lstore pd asmop liparams { v_var =
                                     { Var.vtype = (Coq_sword (coq_Uptr pd));
                                     Var.vname =
                                     (Obj.magic p).p_extra.sp_rsp }; v_info =
                                     dummy_var_info } z (coq_Uptr pd) glob_ra)) :: ({ li_ii =
                     ii; li_i = (Lgoto lcall) } :: ({ li_ii = ii; li_i =
                     (Llabel lbl) } :: (cat after lc)))))))
                 | None -> (lbl0, lc)))
      | None -> (lbl, lc)))

(** val linear_body :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (instr_info -> Var.var option) -> funname -> stk_fun_extra -> 'a1 instr
    list -> label * 'a1 lcmd **)

let linear_body pd asmop liparams p extra_free_registers fn e body =
  let (p0, lbl) =
    match e.sf_return_address with
    | RAnone ->
      (match e.sf_save_stack with
       | SavedStackNone -> (([], []), Coq_xH)
       | SavedStackReg x ->
         let r = { v_var = x; v_info = dummy_var_info } in
         ((((of_olinstr_r asmop dummy_instr_info
              (lmove asmop liparams { v_var = { Var.vtype = (Coq_sword
                (coq_Uptr pd)); Var.vname = (Obj.magic p).p_extra.sp_rsp };
                v_info = dummy_var_info } (coq_Uptr pd) { gv = r; gs =
                Slocal })) :: []),
         ((of_olinstr_r asmop dummy_instr_info
            (lmove asmop liparams r (coq_Uptr pd) { gv = { v_var =
              { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
              (Obj.magic p).p_extra.sp_rsp }; v_info = dummy_var_info }; gs =
              Slocal })) :: (cat
                              (allocate_stack_frame pd asmop liparams p false
                                dummy_instr_info
                                (Z.add e.sf_stk_sz e.sf_stk_extra_sz))
                              ((ensure_rsp_alignment pd asmop liparams p
                                 dummy_instr_info e.sf_align) :: [])))),
         Coq_xH)
       | SavedStackStk ofs ->
         (((cat
             (pop_to_save pd asmop liparams p dummy_instr_info e.sf_to_save)
             ((of_olinstr_r asmop dummy_instr_info
                (lload pd asmop liparams { v_var = { Var.vtype = (Coq_sword
                  (coq_Uptr pd)); Var.vname = (Obj.magic p).p_extra.sp_rsp };
                  v_info = dummy_var_info } (coq_Uptr pd) { v_var =
                  { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
                  (Obj.magic p).p_extra.sp_rsp }; v_info = dummy_var_info }
                  ofs)) :: [])),
           ((of_olinstr_r asmop dummy_instr_info
              (lmove asmop liparams { v_var = { Var.vtype = (Coq_sword
                (coq_Uptr pd)); Var.vname = liparams.lip_tmp }; v_info =
                dummy_var_info } (coq_Uptr pd) { gv = { v_var = { Var.vtype =
                (Coq_sword (coq_Uptr pd)); Var.vname =
                (Obj.magic p).p_extra.sp_rsp }; v_info = dummy_var_info };
                gs = Slocal })) :: (cat
                                     (allocate_stack_frame pd asmop liparams
                                       p false dummy_instr_info
                                       (Z.add e.sf_stk_sz e.sf_stk_extra_sz))
                                     ((ensure_rsp_alignment pd asmop liparams
                                        p dummy_instr_info e.sf_align) :: (
                                     (of_olinstr_r asmop dummy_instr_info
                                       (lstore pd asmop liparams { v_var =
                                         { Var.vtype = (Coq_sword
                                         (coq_Uptr pd)); Var.vname =
                                         (Obj.magic p).p_extra.sp_rsp };
                                         v_info = dummy_var_info } ofs
                                         (coq_Uptr pd) { gv = { v_var =
                                         { Var.vtype = (Coq_sword
                                         (coq_Uptr pd)); Var.vname =
                                         liparams.lip_tmp }; v_info =
                                         dummy_var_info }; gs = Slocal })) :: 
                                     (push_to_save pd asmop liparams p
                                       dummy_instr_info e.sf_to_save)))))),
           Coq_xH))
    | RAreg r ->
      ((({ li_ii = dummy_instr_info; li_i = (Ligoto (Pvar { gv = { v_var = r;
        v_info = dummy_var_info }; gs = Slocal })) } :: []), ({ li_ii =
        dummy_instr_info; li_i = (Llabel Coq_xH) } :: [])), (Coq_xO Coq_xH))
    | RAstack z ->
      ((({ li_ii = dummy_instr_info; li_i = (Ligoto (Pload ((coq_Uptr pd),
        { v_var = { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
        (Obj.magic p).p_extra.sp_rsp }; v_info = dummy_var_info },
        (cast_const pd z)))) } :: []), ({ li_ii = dummy_instr_info; li_i =
        (Llabel Coq_xH) } :: [])), (Coq_xO Coq_xH))
  in
  let (tail, head) = p0 in
  let fd' =
    linear_c asmop (linear_i pd asmop liparams p extra_free_registers fn)
      body lbl tail
  in
  ((fst fd'), (cat head (snd fd')))

(** val linear_fd :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (instr_info -> Var.var option) -> funname -> 'a1 sfundef -> label * 'a1
    lfundef **)

let linear_fd pd asmop liparams p extra_free_registers fn fd =
  let e = fd.f_extra in
  let is_export =
    eq_op return_address_location_eqType
      (Obj.magic (Obj.magic e).sf_return_address) (Obj.magic RAnone)
  in
  let res = if is_export then fd.f_res else [] in
  let body =
    linear_body pd asmop liparams p extra_free_registers fn (Obj.magic e)
      fd.f_body
  in
  ((fst body), { lfd_info = fd.f_info; lfd_align = (Obj.magic e).sf_align;
  lfd_tyin = fd.f_tyin; lfd_arg = fd.f_params; lfd_body = (snd body);
  lfd_tyout = fd.f_tyout; lfd_res = res; lfd_export = is_export;
  lfd_callee_saved =
  (if is_export then map fst (Obj.magic e).sf_to_save else []);
  lfd_total_stack = (Obj.magic e).sf_stk_max })

(** val linear_prog :
    coq_PointerData -> 'a1 asmOp -> 'a1 linearization_params -> 'a1 sprog ->
    (instr_info -> Var.var option) -> 'a1 lprog cexec **)

let linear_prog pd asmop liparams p extra_free_registers =
  match check_prog pd asmop liparams p extra_free_registers with
  | Ok _ ->
    if eq_op nat_eqType (Obj.magic size p.p_globs) (Obj.magic O)
    then let funcs =
           fmap (fun nb_lbl pat ->
             let (f, fd) = pat in
             let fd0 = linear_fd pd asmop liparams p extra_free_registers f fd
             in
             ((Pos.add nb_lbl (fst fd0)), (f, (snd fd0)))) Coq_xH p.p_funcs
         in
         if Z.leb (Zpos (fst funcs)) (wbase (coq_Uptr pd))
         then Ok { lp_rip = (Obj.magic p).p_extra.sp_rip; lp_rsp =
                (Obj.magic p).p_extra.sp_rsp; lp_globs =
                (Obj.magic p).p_extra.sp_globs; lp_funcs = (snd funcs) }
         else let s =
                E.internal_error
                  ('t'::('o'::('o'::(' '::('m'::('a'::('n'::('y'::(' '::('l'::('a'::('b'::('e'::('l'::('s'::[])))))))))))))))
              in
              Error s
    else let s =
           E.internal_error
             ('i'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('p'::('_'::('g'::('l'::('o'::('b'::('s'::[])))))))))))))))
         in
         Error s
  | Error s -> Error s
