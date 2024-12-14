open BinInt
open BinNums
open Bool
open Datatypes
open Compiler_util
open Eqtype
open Expr
open Gen_map
open Seq
open Sopn
open Type
open Utils0
open Var0
open Warray_
open Word_ssrZ
open Wsize

type __ = Obj.t

module E =
 struct
  (** val pass : char list **)

  let pass =
    'a'::('r'::('r'::('a'::('y'::(' '::('e'::('x'::('p'::('a'::('n'::('s'::('i'::('o'::('n'::[]))))))))))))))

  (** val reg_error : var_i -> char list -> pp_error_loc **)

  let reg_error x msg =
    { pel_msg =
      (pp_box ((PPEstring
        ('c'::('a'::('n'::('n'::('o'::('t'::(' '::('e'::('x'::('p'::('a'::('n'::('d'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::[]))))))))))))))))))))))) :: ((PPEvar
        x.v_var) :: ((PPEstring msg) :: [])))); pel_fn = None; pel_fi = None;
      pel_ii = None; pel_vi = (Some x.v_info); pel_pass = (Some pass);
      pel_internal = false }

  (** val reg_ferror : fun_info -> char list -> pp_error_loc **)

  let reg_ferror fi msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = (Some fi); pel_ii =
      None; pel_vi = None; pel_pass = (Some pass); pel_internal = false }

  (** val reg_ierror : var_i -> char list -> pp_error_loc **)

  let reg_ierror x msg =
    { pel_msg =
      (pp_box ((PPEstring
        msg) :: ((pp_nobox ((PPEstring ('('::[])) :: ((PPEvar
                   x.v_var) :: ((PPEstring (')'::[])) :: [])))) :: [])));
      pel_fn = None; pel_fi = None; pel_ii = None; pel_vi = (Some x.v_info);
      pel_pass = (Some pass); pel_internal = true }

  (** val reg_ierror_no_var : char list -> pp_error_loc **)

  let reg_ierror_no_var =
    pp_internal_error_s pass
 end

module CmpIndex =
 struct
  (** val t : Equality.coq_type **)

  let t =
    Equality.clone coq_Z_eqType (Obj.magic coq_Z_eqMixin) (fun x -> x)

  (** val cmp : Equality.sort -> Equality.sort -> comparison **)

  let cmp =
    Obj.magic Z.compare
 end

type varr_info = { vi_v : Var.var; vi_s : wsize; vi_n : Equality.sort list }

type expand_info = { vars : Var.var list; arrs : varr_info list }

module Mi = Mmake(CmpIndex)

type array_info = { ai_ty : wsize; ai_elems : Var.var Mi.t }

type t = { svars : SvExtra.Sv.t; sarrs : array_info Mvar.t }

(** val of_list : Var.var list -> SvExtra.Sv.t **)

let of_list l =
  foldl (fun s x -> SvExtra.Sv.add (Obj.magic x) s) SvExtra.Sv.empty l

(** val init_elems :
    stype -> Equality.sort -> ((SvExtra.Sv.t * Var.var Mi.t) * coq_Z) ->
    (pp_error_loc, (SvExtra.Sv.t * Var.var Mi.Map.t) * coq_Z) result **)

let init_elems ty id = function
| (p, i) ->
  let (sv, mi) = p in
  let xi = { Var.vtype = ty; Var.vname = id } in
  if negb (SvExtra.Sv.mem (Obj.magic xi) sv)
  then Ok (((SvExtra.Sv.add (Obj.magic xi) sv),
         (Mi.set mi (Obj.magic i) xi)), (Z.add i (Zpos Coq_xH)))
  else let s =
         E.reg_ierror_no_var
           ('i'::('n'::('i'::('t'::('_'::('e'::('l'::('e'::('m'::('s'::[]))))))))))
       in
       Error s

(** val init_array_info :
    varr_info -> (SvExtra.Sv.t * array_info Mvar.t) -> (pp_error_loc,
    SvExtra.Sv.t * array_info Mvar.Map.t) result **)

let init_array_info x = function
| (sv, m) ->
  let ty = Coq_sword x.vi_s in
  if negb (SvExtra.Sv.mem (Obj.magic x.vi_v) sv)
  then (match foldM (init_elems ty) ((sv, Mi.empty), Z0) x.vi_n with
        | Ok x0 ->
          let (y, _) = x0 in
          let (sv0, mi) = y in
          Ok (sv0,
          (Mvar.set m (Obj.magic x.vi_v) { ai_ty = x.vi_s; ai_elems = mi }))
        | Error s -> Error s)
  else let s =
         E.reg_ierror_no_var
           ('i'::('n'::('i'::('t'::('_'::('a'::('r'::('r'::('a'::('y'::('_'::('i'::('n'::('f'::('o'::[])))))))))))))))
       in
       Error s

(** val init_map : expand_info -> (pp_error_loc, t) result **)

let init_map fi =
  let svars0 = of_list fi.vars in
  (match foldM init_array_info (svars0, Mvar.empty) fi.arrs with
   | Ok x -> Ok { svars = svars0; sarrs = (snd x) }
   | Error s -> Error s)

(** val check_gvar : t -> gvar -> bool **)

let check_gvar m x =
  (||) (negb (is_lvar x)) (SvExtra.Sv.mem (Obj.magic x.gv.v_var) m.svars)

(** val expand_e : t -> pexpr -> pexpr cexec **)

let rec expand_e m e = match e with
| Pvar x ->
  if check_gvar m x
  then Ok e
  else let s =
         E.reg_error x.gv
           ('('::('t'::('h'::('e'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('m'::('a'::('n'::('i'::('p'::('u'::('l'::('a'::('t'::('e'::('d'::(' '::('a'::('l'::('o'::('n'::('e'::(','::(' '::('y'::('o'::('u'::(' '::('n'::('e'::('e'::('d'::(' '::('t'::('o'::(' '::('a'::('c'::('c'::('e'::('s'::('s'::(' '::('i'::('t'::('s'::(' '::('c'::('e'::('l'::('l'::('s'::(' '::('i'::('n'::('s'::('t'::('e'::('a'::('d'::(')'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       in
       Error s
| Pget (aa, ws, x, e1) ->
  if check_gvar m x
  then (match expand_e m e1 with
        | Ok x0 -> Ok (Pget (aa, ws, x, x0))
        | Error s -> Error s)
  else let x0 = x.gv in
       (match Mvar.get m.sarrs (Obj.magic x0.v_var) with
        | Some ai ->
          (match is_const e1 with
           | Some i ->
             if eq_op wsize_eqType (Obj.magic ai.ai_ty) (Obj.magic ws)
             then if eq_op arr_access_eqType (Obj.magic aa)
                       (Obj.magic AAscale)
                  then (match Mi.get ai.ai_elems (Obj.magic i) with
                        | Some v ->
                          Ok (Pvar
                            (mk_lvar { v_var = v; v_info = x0.v_info }))
                        | None ->
                          Error
                            (E.reg_ierror x0
                              ('t'::('h'::('e'::(' '::('n'::('e'::('w'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::('w'::('a'::('s'::(' '::('n'::('o'::('t'::(' '::('g'::('i'::('v'::('e'::('n'::[]))))))))))))))))))))))))))))))))
                  else let s =
                         E.reg_error x0
                           ('('::('t'::('h'::('e'::(' '::('d'::('e'::('f'::('a'::('u'::('l'::('t'::(' '::('s'::('c'::('a'::('l'::('e'::(' '::('m'::('u'::('s'::('t'::(' '::('b'::('e'::(' '::('u'::('s'::('e'::('d'::(')'::[]))))))))))))))))))))))))))))))))
                       in
                       Error s
             else let s =
                    E.reg_error x0
                      ('('::('t'::('h'::('e'::(' '::('d'::('e'::('f'::('a'::('u'::('l'::('t'::(' '::('s'::('c'::('a'::('l'::('e'::(' '::('m'::('u'::('s'::('t'::(' '::('b'::('e'::(' '::('u'::('s'::('e'::('d'::(')'::[]))))))))))))))))))))))))))))))))
                  in
                  Error s
           | None ->
             Error
               (E.reg_error x0
                 ('('::('t'::('h'::('e'::(' '::('i'::('n'::('d'::('e'::('x'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::(')'::[])))))))))))))))))))))))))))))))
        | None ->
          Error
            (E.reg_error x0
              ('('::('t'::('h'::('e'::(' '::('i'::('n'::('d'::('e'::('x'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::(')'::[])))))))))))))))))))))))))))))))
| Psub (aa, ws, len, x, e1) ->
  if check_gvar m x
  then (match expand_e m e1 with
        | Ok x0 -> Ok (Psub (aa, ws, len, x, x0))
        | Error s -> Error s)
  else let s =
         E.reg_error x.gv
           ('('::('s'::('u'::('b'::('-'::('r'::('e'::('g'::(' '::('a'::('r'::('r'::('a'::('y'::('s'::(' '::('a'::('r'::('e'::(' '::('n'::('o'::('t'::(' '::('a'::('l'::('l'::('o'::('w'::('e'::('d'::(')'::[]))))))))))))))))))))))))))))))))
       in
       Error s
| Pload (ws, x, e1) ->
  if SvExtra.Sv.mem (Obj.magic x.v_var) m.svars
  then (match expand_e m e1 with
        | Ok x0 -> Ok (Pload (ws, x, x0))
        | Error s -> Error s)
  else let s =
         E.reg_ierror x
           ('r'::('e'::('g'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('i'::('n'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('a'::('c'::('c'::('e'::('s'::('s'::[]))))))))))))))))))))))))))
       in
       Error s
| Papp1 (o, e1) ->
  (match expand_e m e1 with
   | Ok x -> Ok (Papp1 (o, x))
   | Error s -> Error s)
| Papp2 (o, e1, e2) ->
  (match expand_e m e1 with
   | Ok x ->
     (match expand_e m e2 with
      | Ok x0 -> Ok (Papp2 (o, x, x0))
      | Error s -> Error s)
   | Error s -> Error s)
| PappN (o, es) ->
  (match mapM (expand_e m) es with
   | Ok x -> Ok (PappN (o, x))
   | Error s -> Error s)
| Pif (ty, e1, e2, e3) ->
  (match expand_e m e1 with
   | Ok x ->
     (match expand_e m e2 with
      | Ok x0 ->
        (match expand_e m e3 with
         | Ok x1 -> Ok (Pif (ty, x, x0, x1))
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)
| _ -> Ok e

(** val expand_lv : t -> lval -> (pp_error_loc, lval) result **)

let expand_lv m x = match x with
| Lnone (_, _) -> Ok x
| Lvar x0 ->
  if SvExtra.Sv.mem (Obj.magic x0.v_var) m.svars
  then Ok (Lvar x0)
  else let s =
         E.reg_error x0
           ('('::('t'::('h'::('e'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('m'::('a'::('n'::('i'::('p'::('u'::('l'::('a'::('t'::('e'::('d'::(' '::('a'::('l'::('o'::('n'::('e'::(','::(' '::('y'::('o'::('u'::(' '::('n'::('e'::('e'::('d'::(' '::('t'::('o'::(' '::('a'::('c'::('c'::('e'::('s'::('s'::(' '::('i'::('t'::('s'::(' '::('c'::('e'::('l'::('l'::('s'::(' '::('i'::('n'::('s'::('t'::('e'::('a'::('d'::(')'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       in
       Error s
| Lmem (ws, x0, e) ->
  if SvExtra.Sv.mem (Obj.magic x0.v_var) m.svars
  then (match expand_e m e with
        | Ok x1 -> Ok (Lmem (ws, x0, x1))
        | Error s -> Error s)
  else let s =
         E.reg_ierror x0
           ('r'::('e'::('g'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('i'::('n'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('a'::('c'::('c'::('e'::('s'::('s'::[]))))))))))))))))))))))))))
       in
       Error s
| Laset (aa, ws, x0, e) ->
  if SvExtra.Sv.mem (Obj.magic x0.v_var) m.svars
  then (match expand_e m e with
        | Ok x1 -> Ok (Laset (aa, ws, x0, x1))
        | Error s -> Error s)
  else (match Mvar.get m.sarrs (Obj.magic x0.v_var) with
        | Some ai ->
          (match is_const e with
           | Some i ->
             if eq_op wsize_eqType (Obj.magic ai.ai_ty) (Obj.magic ws)
             then if eq_op arr_access_eqType (Obj.magic aa)
                       (Obj.magic AAscale)
                  then (match Mi.get ai.ai_elems (Obj.magic i) with
                        | Some v ->
                          Ok (Lvar { v_var = v; v_info = x0.v_info })
                        | None ->
                          Error
                            (E.reg_ierror x0
                              ('t'::('h'::('e'::(' '::('n'::('e'::('w'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::('w'::('a'::('s'::(' '::('n'::('o'::('t'::(' '::('g'::('i'::('v'::('e'::('n'::[]))))))))))))))))))))))))))))))))
                  else let s =
                         E.reg_error x0
                           ('('::('t'::('h'::('e'::(' '::('d'::('e'::('f'::('a'::('u'::('l'::('t'::(' '::('s'::('c'::('a'::('l'::('e'::(' '::('m'::('u'::('s'::('t'::(' '::('b'::('e'::(' '::('u'::('s'::('e'::('d'::(')'::[]))))))))))))))))))))))))))))))))
                       in
                       Error s
             else let s =
                    E.reg_error x0
                      ('('::('t'::('h'::('e'::(' '::('d'::('e'::('f'::('a'::('u'::('l'::('t'::(' '::('s'::('c'::('a'::('l'::('e'::(' '::('m'::('u'::('s'::('t'::(' '::('b'::('e'::(' '::('u'::('s'::('e'::('d'::(')'::[]))))))))))))))))))))))))))))))))
                  in
                  Error s
           | None ->
             Error
               (E.reg_error x0
                 ('('::('t'::('h'::('e'::(' '::('i'::('n'::('d'::('e'::('x'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::(')'::[])))))))))))))))))))))))))))))))
        | None ->
          Error
            (E.reg_error x0
              ('('::('t'::('h'::('e'::(' '::('i'::('n'::('d'::('e'::('x'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('o'::('n'::('s'::('t'::('a'::('n'::('t'::(')'::[])))))))))))))))))))))))))))))))
| Lasub (aa, ws, len, x0, e) ->
  if SvExtra.Sv.mem (Obj.magic x0.v_var) m.svars
  then (match expand_e m e with
        | Ok x1 -> Ok (Lasub (aa, ws, len, x0, x1))
        | Error s -> Error s)
  else let s =
         E.reg_error x0
           ('('::('s'::('u'::('b'::('-'::('r'::('e'::('g'::(' '::('a'::('r'::('r'::('a'::('y'::('s'::(' '::('a'::('r'::('e'::(' '::('n'::('o'::('t'::(' '::('a'::('l'::('l'::('o'::('w'::('e'::('d'::(')'::[]))))))))))))))))))))))))))))))))
       in
       Error s

(** val expand_es : t -> pexpr list -> (pp_error_loc, pexpr list) result **)

let expand_es m =
  mapM (expand_e m)

(** val expand_lvs : t -> lval list -> (pp_error_loc, lval list) result **)

let expand_lvs m =
  mapM (expand_lv m)

(** val expand_i : 'a1 asmOp -> t -> 'a1 instr -> 'a1 instr cexec **)

let rec expand_i asmop m = function
| MkI (ii, ir) ->
  (match ir with
   | Cassgn (x, tag, ty, e) ->
     (match add_iinfo ii (expand_lv m x) with
      | Ok x0 ->
        (match add_iinfo ii (expand_e m e) with
         | Ok x1 -> Ok (MkI (ii, (Cassgn (x0, tag, ty, x1))))
         | Error s -> Error s)
      | Error s -> Error s)
   | Copn (xs, tag, o, es) ->
     (match add_iinfo ii (expand_lvs m xs) with
      | Ok x ->
        (match add_iinfo ii (expand_es m es) with
         | Ok x0 -> Ok (MkI (ii, (Copn (x, tag, o, x0))))
         | Error s -> Error s)
      | Error s -> Error s)
   | Csyscall (xs, o, es) ->
     (match add_iinfo ii (expand_lvs m xs) with
      | Ok x ->
        (match add_iinfo ii (expand_es m es) with
         | Ok x0 -> Ok (MkI (ii, (Csyscall (x, o, x0))))
         | Error s -> Error s)
      | Error s -> Error s)
   | Cif (b, c1, c2) ->
     (match add_iinfo ii (expand_e m b) with
      | Ok x ->
        (match mapM (expand_i asmop m) c1 with
         | Ok x0 ->
           (match mapM (expand_i asmop m) c2 with
            | Ok x1 -> Ok (MkI (ii, (Cif (x, x0, x1))))
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Cfor (x, r, c) ->
     let (p, e2) = r in
     let (dir, e1) = p in
     (match add_iinfo ii
              (if SvExtra.Sv.mem (Obj.magic x.v_var) m.svars
               then Ok ()
               else Error
                      (E.reg_ierror x
                        ('r'::('e'::('g'::(' '::('a'::('r'::('r'::('a'::('y'::(' '::('a'::('s'::(' '::('a'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::('o'::('f'::(' '::('a'::(' '::('f'::('o'::('r'::(' '::('l'::('o'::('o'::('p'::[]))))))))))))))))))))))))))))))))))))))) with
      | Ok _ ->
        (match add_iinfo ii (expand_e m e1) with
         | Ok x0 ->
           (match add_iinfo ii (expand_e m e2) with
            | Ok x1 ->
              (match mapM (expand_i asmop m) c with
               | Ok x2 -> Ok (MkI (ii, (Cfor (x, ((dir, x0), x1), x2))))
               | Error s -> Error s)
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Cwhile (a, c, e, c') ->
     (match add_iinfo ii (expand_e m e) with
      | Ok x ->
        (match mapM (expand_i asmop m) c with
         | Ok x0 ->
           (match mapM (expand_i asmop m) c' with
            | Ok x1 -> Ok (MkI (ii, (Cwhile (a, x0, x, x1))))
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Ccall (ini, xs, fn, es) ->
     (match add_iinfo ii (expand_lvs m xs) with
      | Ok x ->
        (match add_iinfo ii (expand_es m es) with
         | Ok x0 -> Ok (MkI (ii, (Ccall (ini, x, fn, x0))))
         | Error s -> Error s)
      | Error s -> Error s))

(** val expand_fd :
    'a1 asmOp -> (funname -> 'a1 ufundef -> expand_info) -> funname -> 'a1
    ufundef -> (pp_error_loc, ('a1, Equality.sort) _fundef) result **)

let expand_fd asmop fi f fd =
  match init_map (fi f fd) with
  | Ok x ->
    let { f_info = fi0; f_tyin = tyin; f_params = params; f_body = c;
      f_tyout = tyout; f_res = res; f_extra = ef } = fd
    in
    if all (fun x0 -> SvExtra.Sv.mem (Obj.magic x0.v_var) x.svars) params
    then if all (fun x0 -> SvExtra.Sv.mem (Obj.magic x0.v_var) x.svars) res
         then (match mapM (expand_i asmop x) c with
               | Ok x0 ->
                 Ok { f_info = fi0; f_tyin = tyin; f_params = params;
                   f_body = x0; f_tyout = tyout; f_res = res; f_extra = ef }
               | Error s -> Error s)
         else let s =
                E.reg_ferror fi0
                  ('r'::('e'::('g'::(' '::('a'::('r'::('r'::('a'::('y'::('s'::(' '::('a'::('r'::('e'::(' '::('n'::('o'::('t'::(' '::('a'::('l'::('l'::('o'::('w'::('e'::('d'::(' '::('i'::('n'::(' '::('t'::('h'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('o'::('f'::(' '::('n'::('o'::('n'::(' '::('i'::('n'::('l'::('i'::('n'::('e'::('d'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
              in
              Error s
    else let s =
           E.reg_ferror fi0
             ('r'::('e'::('g'::(' '::('a'::('r'::('r'::('a'::('y'::('s'::(' '::('a'::('r'::('e'::(' '::('n'::('o'::('t'::(' '::('a'::('l'::('l'::('o'::('w'::('e'::('d'::(' '::('i'::('n'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('s'::(' '::('o'::('f'::(' '::('n'::('o'::('n'::(' '::('i'::('n'::('l'::('i'::('n'::('e'::('d'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         in
         Error s
  | Error s -> Error s

(** val expand_prog :
    'a1 asmOp -> (funname -> 'a1 ufundef -> expand_info) -> 'a1 uprog -> 'a1
    uprog cexec **)

let expand_prog asmop fi p =
  match map_cfprog_name_gen (fun x -> x.f_info) (expand_fd asmop fi) p.p_funcs with
  | Ok x -> Ok { p_funcs = x; p_globs = p.p_globs; p_extra = p.p_extra }
  | Error s -> Error s
