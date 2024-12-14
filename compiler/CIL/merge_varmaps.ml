open Datatypes
open Compiler_util
open Eqtype
open Expr
open Gen_map
open One_varmap
open Seq
open Sopn
open Ssrfun
open Type
open Utils0
open Var0
open Wsize

module E =
 struct
  (** val pass_name : char list **)

  let pass_name =
    'o'::('n'::('e'::('-'::('v'::('a'::('r'::('m'::('a'::('p'::(' '::('c'::('h'::('e'::('c'::('k'::('e'::('r'::[])))))))))))))))))

  (** val gen_error :
      bool -> instr_info option -> pp_error -> pp_error_loc **)

  let gen_error internal ii msg =
    { pel_msg = msg; pel_fn = None; pel_fi = None; pel_ii = ii; pel_vi =
      None; pel_pass = (Some pass_name); pel_internal = internal }

  (** val internal_error : instr_info -> char list -> pp_error_loc **)

  let internal_error ii msg =
    gen_error true (Some ii) (PPEstring msg)

  (** val error : instr_info -> char list -> pp_error_loc **)

  let error ii msg =
    gen_error false (Some ii) (PPEstring msg)

  (** val ii_loop_iterator : instr_info -> pp_error_loc **)

  let ii_loop_iterator =
    ii_loop_iterator pass_name
 end

(** val add_extra_free_registers :
    (instr_info -> Var.var option) -> instr_info -> SvExtra.Sv.t ->
    SvExtra.Sv.t **)

let add_extra_free_registers extra_free_registers ii d =
  match extra_free_registers ii with
  | Some r -> SvExtra.Sv.add (Obj.magic r) d
  | None -> d

(** val writefun_ra :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog -> Var.var
    -> (funname -> SvExtra.Sv.t) -> funname -> SvExtra.Sv.t **)

let writefun_ra _ asmop ovm_i p var_tmp writefun fn =
  let ra =
    match get_fundef p.p_funcs fn with
    | Some fd ->
      SvExtra.Sv.union (ra_vm ovm_i (Obj.magic fd).f_extra var_tmp)
        (saved_stack_vm asmop (Obj.magic fd))
    | None -> SvExtra.Sv.empty
  in
  SvExtra.Sv.union (writefun fn) ra

(** val write_I_rec :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (funname -> SvExtra.Sv.t) ->
    SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t **)

let write_I_rec pd asmop ovm_i p extra_free_registers var_tmp writefun =
  let rec write_i_rec s = function
  | Cassgn (x, _, _, _) -> vrv_rec s x
  | Copn (xs, _, _, _) -> vrvs_rec s xs
  | Csyscall (_, o, _) ->
    vrvs_rec (SvExtra.Sv.union s (syscall_kill ovm_i))
      (to_lvals (ovm_i.syscall_sig o).scs_vout)
  | Cif (_, c1, c2) -> foldl write_I_rec0 (foldl write_I_rec0 s c2) c1
  | Cfor (x, _, c) ->
    foldl write_I_rec0 (SvExtra.Sv.add (Obj.magic x.v_var) s) c
  | Cwhile (_, c, _, c') -> foldl write_I_rec0 (foldl write_I_rec0 s c') c
  | Ccall (_, _, fn, _) ->
    SvExtra.Sv.union s (writefun_ra pd asmop ovm_i p var_tmp writefun fn)
  and write_I_rec0 s = function
  | MkI (ii, i0) ->
    add_extra_free_registers extra_free_registers ii (write_i_rec s i0)
  in write_I_rec0

(** val write_c_rec :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (funname -> SvExtra.Sv.t) ->
    SvExtra.Sv.t -> 'a1 instr list -> SvExtra.Sv.t **)

let write_c_rec pd asmop ovm_i p extra_free_registers var_tmp writefun =
  foldl (write_I_rec pd asmop ovm_i p extra_free_registers var_tmp writefun)

(** val write_c :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (funname -> SvExtra.Sv.t) ->
    'a1 instr list -> SvExtra.Sv.t **)

let write_c pd asmop ovm_i p extra_free_registers var_tmp writefun =
  write_c_rec pd asmop ovm_i p extra_free_registers var_tmp writefun
    SvExtra.Sv.empty

(** val write_fd :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (funname -> SvExtra.Sv.t) ->
    'a1 sfundef -> SvExtra.Sv.t **)

let write_fd pd asmop ovm_i p extra_free_registers var_tmp writefun fd =
  write_c pd asmop ovm_i p extra_free_registers var_tmp writefun fd.f_body

(** val get_wmap : SvExtra.Sv.t Mp.t -> funname -> SvExtra.Sv.t **)

let get_wmap wmap fn =
  Option.default SvExtra.Sv.empty (Mp.get wmap (Obj.magic fn))

(** val mk_wmap :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> SvExtra.Sv.t Mp.t **)

let mk_wmap pd asmop ovm_i p extra_free_registers var_tmp =
  foldr (fun pat wmap ->
    let (f, fd) = pat in
    let w =
      write_fd pd asmop ovm_i p extra_free_registers var_tmp (get_wmap wmap)
        fd
    in
    Mp.set wmap (Obj.magic f) w) Mp.empty p.p_funcs

(** val check_wmap :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> SvExtra.Sv.t Mp.t -> bool **)

let check_wmap pd asmop ovm_i p extra_free_registers var_tmp wmap =
  all (fun pat ->
    let (f, fd) = pat in
    SvExtra.Sv.subset
      (write_fd pd asmop ovm_i p extra_free_registers var_tmp (get_wmap wmap)
        fd) (get_wmap wmap f)) p.p_funcs

(** val check_fv :
    instr_info -> SvExtra.Sv.t -> SvExtra.Sv.t -> (pp_error_loc, unit) result **)

let check_fv ii d r =
  if SvExtra.disjoint d r
  then Ok ()
  else Error
         (E.error ii
           ('m'::('o'::('d'::('i'::('f'::('i'::('e'::('d'::(' '::('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::[]))))))))))))))))))))

(** val check_e :
    instr_info -> SvExtra.Sv.t -> pexpr -> (pp_error_loc, unit) result **)

let check_e ii d e =
  check_fv ii d (read_e e)

(** val check_es :
    instr_info -> SvExtra.Sv.t -> pexpr list -> (pp_error_loc, unit) result **)

let check_es ii d es =
  foldM (fun e _ -> check_e ii d e) () es

(** val check_c :
    'a1 asmOp -> (SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t cexec) ->
    SvExtra.Sv.t -> 'a1 instr list -> (pp_error_loc, SvExtra.Sv.t) result **)

let rec check_c asmop check_i0 d = function
| [] -> Ok d
| i :: c' ->
  (match check_i0 d i with
   | Ok x -> check_c asmop check_i0 x c'
   | Error s -> Error s)

(** val wloop :
    'a1 asmOp -> (SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t cexec) ->
    instr_info -> 'a1 instr list -> SvExtra.Sv.t -> 'a1 instr list -> nat ->
    SvExtra.Sv.t -> (pp_error_loc, SvExtra.Sv.t) result **)

let rec wloop asmop check_i0 ii c1 efv c2 n d =
  match n with
  | O -> Error (E.ii_loop_iterator ii)
  | S n' ->
    (match check_c asmop check_i0 d c1 with
     | Ok x ->
       (match check_fv ii x efv with
        | Ok _ ->
          (match check_c asmop check_i0 x c2 with
           | Ok x0 ->
             if SvExtra.Sv.subset x0 d
             then Ok x
             else wloop asmop check_i0 ii c1 efv c2 n' (SvExtra.Sv.union x0 d)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)

(** val check_lv :
    instr_info -> SvExtra.Sv.t -> lval -> (pp_error_loc, SvExtra.Sv.t) result **)

let check_lv ii d x =
  match check_fv ii d (read_rv x) with
  | Ok _ -> Ok (SvExtra.Sv.diff d (vrv x))
  | Error s -> Error s

(** val check_lvs :
    instr_info -> SvExtra.Sv.t -> lval list -> (pp_error_loc, SvExtra.Sv.t)
    result **)

let check_lvs ii d xs =
  foldM (fun x d0 -> check_lv ii d0 x) d xs

(** val check_i :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (funname -> SvExtra.Sv.t) ->
    wsize -> SvExtra.Sv.t -> 'a1 instr -> SvExtra.Sv.t cexec **)

let check_i pd asmop ovm_i p extra_free_registers var_tmp writefun =
  let rec check_i0 sz d = function
  | MkI (ii, ir) ->
    (match match extra_free_registers ii with
           | Some r ->
             if eq_op stype_eqType (Obj.magic Var.vtype r)
                  (Obj.magic (Coq_sword (coq_Uptr pd)))
             then (match ir with
                   | Cwhile (_, _, _, _) ->
                     Error
                       (E.internal_error ii
                         ('l'::('o'::('o'::('p'::('s'::(' '::('n'::('e'::('e'::('d'::(' '::('n'::('o'::(' '::('e'::('x'::('t'::('r'::('a'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[])))))))))))))))))))))))))))))
                   | _ -> Ok ())
             else let s =
                    E.internal_error ii
                      ('b'::('a'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('f'::('o'::('r'::(' '::('e'::('x'::('t'::('r'::('a'::(' '::('f'::('r'::('e'::('e'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))
                  in
                  Error s
           | None -> Ok () with
     | Ok _ ->
       check_ir sz ii (add_extra_free_registers extra_free_registers ii d) ir
     | Error s -> Error s)
  and check_ir sz ii d = function
  | Cassgn (x, _, _, e) ->
    (match check_e ii d e with
     | Ok _ -> check_lv ii d x
     | Error s -> Error s)
  | Copn (xs, _, _, es) ->
    (match check_es ii d es with
     | Ok _ -> check_lvs ii d xs
     | Error s -> Error s)
  | Csyscall (xs, o, es) ->
    let osig = ovm_i.syscall_sig o in
    let o_params = osig.scs_vin in
    let o_res = osig.scs_vout in
    (match check_es ii d es with
     | Ok _ ->
       if all2 (fun e a ->
            match e with
            | Pvar g ->
              let { gv = v; gs = gs0 } = g in
              (match gs0 with
               | Slocal ->
                 eq_op Var.var_eqType (Obj.magic v.v_var) (Obj.magic a)
               | Sglob -> false)
            | _ -> false) es o_params
       then if all2 (fun x r ->
                 match x with
                 | Lvar v ->
                   eq_op Var.var_eqType (Obj.magic v.v_var) (Obj.magic r)
                 | _ -> false) xs o_res
            then let w = syscall_kill ovm_i in
                 Ok
                 (SvExtra.Sv.diff (SvExtra.Sv.union d w)
                   (vrvs (to_lvals (ovm_i.syscall_sig o).scs_vout)))
            else let s =
                   E.internal_error ii
                     ('b'::('a'::('d'::(' '::('s'::('y'::('s'::('c'::('a'::('l'::('l'::(' '::('d'::('e'::('s'::('t'::('s'::[])))))))))))))))))
                 in
                 Error s
       else let s =
              E.internal_error ii
                ('b'::('a'::('d'::(' '::('s'::('y'::('s'::('c'::('a'::('l'::('l'::(' '::('a'::('r'::('g'::('s'::[]))))))))))))))))
            in
            Error s
     | Error s -> Error s)
  | Cif (b, c1, c2) ->
    (match check_e ii d b with
     | Ok _ ->
       (match check_c asmop (check_i0 sz) d c1 with
        | Ok x ->
          (match check_c asmop (check_i0 sz) d c2 with
           | Ok x0 -> Ok (SvExtra.Sv.union x x0)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Cfor (_, _, _) ->
    Error
      (E.internal_error ii
        ('f'::('o'::('r'::(' '::('l'::('o'::('o'::('p'::(' '::('s'::('h'::('o'::('u'::('l'::('d'::(' '::('b'::('e'::(' '::('u'::('n'::('r'::('o'::('l'::('l'::('e'::('d'::[]))))))))))))))))))))))))))))
  | Cwhile (_, c, e, c') ->
    if is_false e
    then check_c asmop (check_i0 sz) d c
    else wloop asmop (check_i0 sz) ii c (read_e e) c' Loop.nb d
  | Ccall (_, xs, fn, es) ->
    (match get_fundef p.p_funcs fn with
     | Some fd ->
       (match check_es ii d es with
        | Ok _ ->
          if cmp_le wsize_cmp (Obj.magic fd).f_extra.sf_align sz
          then if match (Obj.magic fd).f_extra.sf_return_address with
                  | RAstack _ ->
                    negb
                      (eq_op (option_eqType Var.var_eqType)
                        (Obj.magic extra_free_registers ii) (Obj.magic None))
                  | _ -> true
               then if all2 (fun e a ->
                         match e with
                         | Pvar g ->
                           let { gv = v; gs = gs0 } = g in
                           (match gs0 with
                            | Slocal ->
                              eq_op Var.var_eqType (Obj.magic v.v_var)
                                (Obj.magic a.v_var)
                            | Sglob -> false)
                         | _ -> false) es fd.f_params
                    then if all2 (fun x r ->
                              match x with
                              | Lvar v ->
                                eq_op Var.var_eqType (Obj.magic v.v_var)
                                  (Obj.magic r.v_var)
                              | _ -> false) xs fd.f_res
                         then let w =
                                writefun_ra pd asmop ovm_i p var_tmp writefun
                                  fn
                              in
                              Ok
                              (SvExtra.Sv.diff (SvExtra.Sv.union d w)
                                (SvExtra.sv_of_list
                                  (Obj.magic (fun v -> v.v_var)) fd.f_res))
                         else let s =
                                E.internal_error ii
                                  ('b'::('a'::('d'::(' '::('c'::('a'::('l'::('l'::(' '::('d'::('e'::('s'::('t'::('s'::[]))))))))))))))
                              in
                              Error s
                    else let s =
                           E.internal_error ii
                             ('b'::('a'::('d'::(' '::('c'::('a'::('l'::('l'::(' '::('a'::('r'::('g'::('s'::[])))))))))))))
                         in
                         Error s
               else let s =
                      E.internal_error ii
                        ('n'::('o'::(' '::('e'::('x'::('t'::('r'::('a'::(' '::('f'::('r'::('e'::('e'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::(' '::('t'::('o'::(' '::('c'::('o'::('m'::('p'::('u'::('t'::('e'::(' '::('t'::('h'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('a'::('d'::('d'::('r'::('e'::('s'::('s'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))
                    in
                    Error s
          else let s =
                 E.internal_error ii
                   ('a'::('l'::('i'::('g'::('n'::('m'::('e'::('n'::('t'::(' '::('c'::('o'::('n'::('s'::('t'::('r'::('a'::('i'::('n'::('t'::('s'::(' '::('e'::('r'::('r'::('o'::('r'::[])))))))))))))))))))))))))))
               in
               Error s
        | Error s -> Error s)
     | None ->
       Error
         (E.internal_error ii
           ('c'::('a'::('l'::('l'::(' '::('t'::('o'::(' '::('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))
  in check_i0

(** val check_fd :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (funname -> SvExtra.Sv.t) ->
    funname -> 'a1 sfundef -> (pp_error_loc, unit) result **)

let check_fd pd asmop ovm_i p extra_free_registers var_tmp =
  let magic_variables0 = magic_variables pd asmop p in
  (fun writefun ->
  let check_preserved_register = fun w j name r ->
    if eq_op stype_eqType (Obj.magic Var.vtype r)
         (Obj.magic (Coq_sword (coq_Uptr pd)))
    then if negb (SvExtra.Sv.mem (Obj.magic r) w)
         then if negb (SvExtra.Sv.mem (Obj.magic r) j)
              then Ok ()
              else Error
                     (E.gen_error true None
                       (pp_box ((PPEstring
                         ('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('d'::('e'::('p'::('e'::('n'::('d'::('s'::(' '::('o'::('n'::(' '::('i'::('t'::('s'::[])))))))))))))))))))))))))))) :: ((PPEstring
                         name) :: ((PPEvar r) :: [])))))
         else let s =
                E.gen_error true None
                  (pp_box ((PPEstring
                    ('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('w'::('r'::('i'::('t'::('e'::('s'::(' '::('i'::('t'::('s'::[])))))))))))))))))))))))) :: ((PPEstring
                    name) :: ((PPEvar r) :: []))))
              in
              Error s
    else let s =
           E.gen_error true None
             (pp_box ((PPEstring
               ('b'::('a'::('d'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::(' '::('t'::('y'::('p'::('e'::(' '::('f'::('o'::('r'::[])))))))))))))))))))))) :: ((PPEstring
               name) :: ((PPEvar r) :: []))))
         in
         Error s
  in
  (fun fn fd ->
  let params = SvExtra.sv_of_list (Obj.magic (fun v -> v.v_var)) fd.f_params
  in
  let dI =
    SvExtra.Sv.inter params (ra_undef asmop ovm_i (Obj.magic fd) var_tmp)
  in
  (match check_c asmop
           (check_i pd asmop ovm_i p extra_free_registers var_tmp writefun
             (Obj.magic fd).f_extra.sf_align) dI fd.f_body with
   | Ok x ->
     let res = SvExtra.sv_of_list (Obj.magic (fun v -> v.v_var)) fd.f_res in
     let w' = writefun_ra pd asmop ovm_i p var_tmp writefun fn in
     if SvExtra.disjoint x res
     then if SvExtra.disjoint params magic_variables0
          then if negb
                    (SvExtra.Sv.mem
                      (let v_var0 = { Var.vtype = (Coq_sword (coq_Uptr pd));
                         Var.vname = (Obj.magic p).p_extra.sp_rsp }
                       in
                       Obj.magic v_var0) res)
               then if SvExtra.disjoint w' magic_variables0
                    then let w = writefun fn in
                         let j = SvExtra.Sv.union magic_variables0 params in
                         let e = fd.f_extra in
                         (match match (Obj.magic e).sf_save_stack with
                                | SavedStackReg r ->
                                  check_preserved_register w j
                                    ('s'::('a'::('v'::('e'::('d'::(' '::('s'::('t'::('a'::('c'::('k'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[])))))))))))))))))))
                                    r
                                | _ -> Ok () with
                          | Ok _ ->
                            (match (Obj.magic e).sf_return_address with
                             | RAnone ->
                               let to_save =
                                 SvExtra.sv_of_list (Obj.magic fst)
                                   (Obj.magic fd).f_extra.sf_to_save
                               in
                               if SvExtra.disjoint to_save res
                               then if SvExtra.Sv.subset
                                         (SvExtra.Sv.inter ovm_i.callee_saved
                                           w') to_save
                                    then if all (fun x0 ->
                                              match Var.vtype x0.v_var with
                                              | Coq_sword _ -> true
                                              | _ -> false) fd.f_params
                                         then Ok ()
                                         else Error
                                                (E.gen_error true None
                                                  (PPEstring
                                                  ('t'::('h'::('e'::(' '::('e'::('x'::('p'::('o'::('r'::('t'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('h'::('a'::('s'::(' '::('n'::('o'::('n'::('-'::('w'::('o'::('r'::('d'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[]))))))))))))))))))))))))))))))))))))))))))))
                                    else let s =
                                           E.gen_error true None (PPEstring
                                             ('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('k'::('i'::('l'::('l'::('s'::(' '::('s'::('o'::('m'::('e'::(' '::('c'::('a'::('l'::('l'::('e'::('e'::('-'::('s'::('a'::('v'::('e'::('d'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::('s'::[])))))))))))))))))))))))))))))))))))))))))))))))
                                         in
                                         Error s
                               else let s =
                                      E.gen_error true None (PPEstring
                                        ('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::('s'::(' '::('a'::(' '::('c'::('a'::('l'::('l'::('e'::('e'::('-'::('s'::('a'::('v'::('e'::('d'::(' '::('r'::('e'::('g'::('i'::('s'::('t'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))))))))))
                                    in
                                    Error s
                             | RAreg ra ->
                               check_preserved_register w j
                                 ('r'::('e'::('t'::('u'::('r'::('n'::(' '::('a'::('d'::('d'::('r'::('e'::('s'::('s'::[]))))))))))))))
                                 ra
                             | RAstack _ -> Ok ())
                          | Error s -> Error s)
                    else let s =
                           E.gen_error true None (PPEstring
                             ('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('w'::('r'::('i'::('t'::('e'::('s'::(' '::('t'::('o'::(' '::('R'::('S'::('P'::(' '::('o'::('r'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('-'::('d'::('a'::('t'::('a'::[]))))))))))))))))))))))))))))))))))))))))))
                         in
                         Error s
               else let s =
                      E.gen_error true None (PPEstring
                        ('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::('s'::(' '::('R'::('S'::('P'::[])))))))))))))))))))))))))
                    in
                    Error s
          else let s =
                 E.gen_error true None (PPEstring
                   ('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('h'::('a'::('s'::(' '::('R'::('S'::('P'::(' '::('o'::('r'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('-'::('d'::('a'::('t'::('a'::(' '::('a'::('s'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))))))))))))))
               in
               Error s
     else let s =
            E.gen_error true None (PPEstring
              ('n'::('o'::('t'::(' '::('a'::('b'::('l'::('e'::(' '::('t'::('o'::(' '::('e'::('n'::('s'::('u'::('r'::('e'::(' '::('e'::('q'::('u'::('a'::('l'::('i'::('t'::('y'::(' '::('o'::('f'::(' '::('t'::('h'::('e'::(' '::('r'::('e'::('s'::('u'::('l'::('t'::[]))))))))))))))))))))))))))))))))))))))))))
          in
          Error s
   | Error s -> Error s)))

(** val check_prog :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (funname -> SvExtra.Sv.t) ->
    (pp_error_loc, (funname * unit) list) result **)

let check_prog pd asmop ovm_i p extra_free_registers var_tmp writefun =
  map_cfprog_name_gen (fun x -> x.f_info)
    (check_fd pd asmop ovm_i p extra_free_registers var_tmp writefun)
    p.p_funcs

(** val check :
    coq_PointerData -> 'a1 asmOp -> one_varmap_info -> 'a1 sprog ->
    (instr_info -> Var.var option) -> Var.var -> (pp_error_loc, unit) result **)

let check pd asmop ovm_i p extra_free_registers var_tmp =
  let magic_variables0 = magic_variables pd asmop p in
  let wmap = mk_wmap pd asmop ovm_i p extra_free_registers var_tmp in
  if check_wmap pd asmop ovm_i p extra_free_registers var_tmp wmap
  then if negb
            (eq_op Ident.Ident.ident (Obj.magic p).p_extra.sp_rip
              (Obj.magic p).p_extra.sp_rsp)
       then if negb (SvExtra.Sv.mem (Obj.magic var_tmp) magic_variables0)
            then (match check_prog pd asmop ovm_i p extra_free_registers
                          var_tmp (get_wmap wmap) with
                  | Ok _ -> Ok ()
                  | Error s -> Error s)
            else let s =
                   E.gen_error true None (PPEstring
                     ('R'::('A'::('X'::(' '::('c'::('l'::('a'::('s'::('h'::('e'::('s'::(' '::('w'::('i'::('t'::('h'::(' '::('R'::('S'::('P'::(' '::('o'::('r'::(' '::('R'::('I'::('P'::[]))))))))))))))))))))))))))))
                 in
                 Error s
       else let s =
              E.gen_error true None (PPEstring
                ('r'::('i'::('p'::(' '::('a'::('n'::('d'::(' '::('r'::('s'::('p'::(' '::('c'::('l'::('a'::('s'::('h'::[]))))))))))))))))))
            in
            Error s
  else let s =
         E.gen_error true None (PPEstring
           ('i'::('n'::('v'::('a'::('l'::('i'::('d'::(' '::('w'::('m'::('a'::('p'::[])))))))))))))
       in
       Error s
