open Datatypes
open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Ssrfun
open Syscall
open Type
open Utils0
open Var0
open Warray_
open Word_ssrZ
open Wsize

module E =
 struct
  (** val pass_name : char list **)

  let pass_name =
    'a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::[])))))))))

  (** val gen_error :
      bool -> instr_info option -> char list -> pp_error_loc **)

  let gen_error internal ii msg =
    { pel_msg = (PPEstring msg); pel_fn = None; pel_fi = None; pel_ii = ii;
      pel_vi = None; pel_pass = (Some pass_name); pel_internal = internal }

  (** val error : char list -> pp_error_loc **)

  let error msg =
    gen_error true None msg

  (** val loop_iterator : pp_error_loc **)

  let loop_iterator =
    loop_iterator pass_name

  (** val fold2 : pp_error_loc **)

  let fold2 =
    error ('f'::('o'::('l'::('d'::('2'::[])))))
 end

(** val wextend_type : Equality.sort -> Equality.sort -> bool **)

let wextend_type t1 t2 =
  (||) (eq_op stype_eqType t1 t2)
    (match Obj.magic t1 with
     | Coq_sword s1 ->
       (match Obj.magic t2 with
        | Coq_sword s2 -> cmp_le wsize_cmp s1 s2
        | _ -> false)
     | _ -> false)

module M =
 struct
  module Mv =
   struct
    (** val oget : SvExtra.Sv.t Mvar.t -> Equality.sort -> SvExtra.Sv.t **)

    let oget mid0 id =
      Option.default SvExtra.Sv.empty (Mvar.get mid0 id)

    type t_ = { mvar : Var.var Mvar.t; mid : SvExtra.Sv.t Mvar.t }

    (** val mvar : t_ -> Var.var Mvar.t **)

    let mvar t0 =
      t0.mvar

    (** val mid : t_ -> SvExtra.Sv.t Mvar.t **)

    let mid t0 =
      t0.mid

    type t = t_

    (** val get : t -> Var.var -> Var.var option **)

    let get m x =
      Mvar.get m.mvar (Obj.magic x)

    (** val rm_id : t -> Equality.sort -> Var.var Mvar.t **)

    let rm_id m id =
      SvExtra.Sv.fold (fun x m0 -> Mvar.remove m0 x) (oget m.mid id) m.mvar

    (** val ms_upd :
        SvExtra.Sv.t Mvar.t -> (SvExtra.Sv.t -> SvExtra.Sv.t) ->
        Equality.sort -> SvExtra.Sv.t Mvar.Map.t **)

    let ms_upd m f id =
      Mvar.set m id (f (oget m id))

    (** val rm_x : t -> Equality.sort -> SvExtra.Sv.t Mvar.Map.t **)

    let rm_x m x =
      match Mvar.get m.mvar x with
      | Some id -> ms_upd m.mid (SvExtra.Sv.remove x) (Obj.magic id)
      | None -> m.mid

    (** val remove : t -> Equality.sort -> t_ **)

    let remove m id =
      { mvar = (rm_id m id); mid = (Mvar.remove m.mid id) }

    (** val set : t -> Equality.sort -> Equality.sort -> t_ **)

    let set m x id =
      { mvar = (Mvar.set (rm_id m id) x (Obj.magic id)); mid =
        (Mvar.set (rm_x m x) id (SvExtra.Sv.singleton x)) }

    (** val add : t_ -> Equality.sort -> Var.var -> t_ **)

    let add m x id =
      { mvar = (Mvar.set m.mvar x id); mid =
        (ms_upd (rm_x m x) (fun s -> SvExtra.Sv.add x s) (Obj.magic id)) }

    (** val empty : t_ **)

    let empty =
      { mvar = Mvar.empty; mid = Mvar.empty }
   end

  (** val bool_dec : bool -> bool **)

  let bool_dec = function
  | true -> true
  | false -> false

  (** val v_wextendty : Var.var -> Var.var -> bool **)

  let v_wextendty x y =
    wextend_type (Obj.magic Var.vtype x) (Obj.magic Var.vtype y)

  (** val v_wextendtyP : Var.var -> Var.var -> bool **)

  let v_wextendtyP x y =
    bool_dec (v_wextendty x y)

  type t_ = { mv : Mv.t; mset : SvExtra.Sv.t }

  (** val mv : t_ -> Mv.t **)

  let mv t0 =
    t0.mv

  (** val mset : t_ -> SvExtra.Sv.t **)

  let mset t0 =
    t0.mset

  type t = t_

  (** val get : t -> Var.var -> Var.var option **)

  let get m x =
    Mv.get m.mv x

  (** val set : t_ -> Var.var -> Var.var -> t_ **)

  let set m x id =
    { mv = (Mv.set m.mv (Obj.magic x) (Obj.magic id)); mset =
      (SvExtra.Sv.add (Obj.magic x) m.mset) }

  (** val add : t_ -> Var.var -> Var.var -> t_ **)

  let add m x id =
    { mv = (Mv.add m.mv (Obj.magic x) id); mset =
      (SvExtra.Sv.add (Obj.magic x) m.mset) }

  (** val addc : t_ -> Var.var -> Var.var -> t_ **)

  let addc m x id =
    if v_wextendtyP x id then add m x id else m

  (** val empty_s : SvExtra.Sv.t -> t_ **)

  let empty_s s =
    { mv = Mv.empty; mset = s }

  (** val empty : t_ **)

  let empty =
    empty_s SvExtra.Sv.empty

  (** val merge_aux : t_ -> t_ -> Equality.sort Mvar.t **)

  let merge_aux m1 m2 =
    Mvar.map2 (fun x ox ox' ->
      match ox with
      | Some idx ->
        (match ox' with
         | Some idx' ->
           if eq_op Var.var_eqType (Obj.magic idx) (Obj.magic idx')
           then Some (Obj.magic idx)
           else None
         | None ->
           if negb (SvExtra.Sv.mem x m2.mset)
           then Some (Obj.magic idx)
           else None)
      | None ->
        (match ox' with
         | Some idx ->
           if negb (SvExtra.Sv.mem x m1.mset)
           then Some (Obj.magic idx)
           else None
         | None -> None)) m1.mv.Mv.mvar m2.mv.Mv.mvar

  (** val merge : t_ -> t_ -> t_ **)

  let merge m1 m2 =
    let mv0 = merge_aux m1 m2 in
    Mvar.fold (fun x idx m -> addc m (Obj.magic x) (Obj.magic idx)) mv0
      (empty_s (SvExtra.Sv.union m1.mset m2.mset))

  (** val remove : t_ -> Equality.sort -> t_ **)

  let remove m id =
    { mv = (Mv.remove m.mv id); mset = m.mset }

  (** val incl : t_ -> t_ -> bool **)

  let incl m1 m2 =
    (&&) (SvExtra.Sv.subset m2.mset m1.mset)
      (let mv1 = m1.mv.Mv.mvar in
       let mv2 = m2.mv.Mv.mvar in
       SvExtra.Sv.for_all (fun x ->
         match Mvar.get mv1 x with
         | Some idx ->
           eq_op (option_eqType Var.var_eqType) (Obj.magic Mvar.get mv2 x)
             (Obj.magic (Some idx))
         | None -> true) m2.mset)
 end

(** val alloc_error : char list -> pp_error_loc **)

let alloc_error =
  pp_internal_error_s
    ('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::[]))))))))))

(** val cerr_varalloc : Var.var -> Var.var -> char list -> pp_error_loc **)

let cerr_varalloc xi1 xi2 s =
  pp_internal_error
    ('V'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))
    (pp_box ((PPEvar xi1) :: ((PPEstring ('a'::('n'::('d'::[])))) :: ((PPEvar
      xi2) :: ((PPEstring (':'::[])) :: ((PPEstring s) :: []))))))

(** val check_v : var_i -> var_i -> M.t -> M.t cexec **)

let check_v xi1 xi2 m =
  let x1 = xi1.v_var in
  let x2 = xi2.v_var in
  if M.v_wextendtyP x1 x2
  then (match M.get m x1 with
        | Some x2' ->
          if eq_op Var.var_eqType (Obj.magic x2) (Obj.magic x2')
          then Ok m
          else Error
                 (cerr_varalloc xi1.v_var xi2.v_var
                   ('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))
        | None ->
          if SvExtra.Sv.mem (Obj.magic x1) m.M.mset
          then Error
                 (cerr_varalloc xi1.v_var xi2.v_var
                   ('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::(' '::('a'::('l'::('r'::('e'::('a'::('d'::('y'::(' '::('s'::('e'::('t'::[])))))))))))))))))))))
          else Ok (M.set m x1 x2))
  else Error
         (cerr_varalloc xi1.v_var xi2.v_var
           ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))

(** val error_e : pp_error_loc **)

let error_e =
  pp_internal_error_s
    ('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::[]))))))))))
    ('e'::('x'::('p'::('r'::('e'::('s'::('s'::('i'::('o'::('n'::(' '::('a'::('r'::('e'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::[]))))))))))))))))))))))))

(** val check_gv : gvar -> gvar -> M.t -> M.t cexec **)

let check_gv x1 x2 m =
  if eq_op v_scope_eqType (Obj.magic x1.gs) (Obj.magic x2.gs)
  then if is_lvar x1
       then check_v x1.gv x2.gv m
       else if eq_op Var.var_eqType (Obj.magic x1.gv.v_var)
                 (Obj.magic x2.gv.v_var)
            then Ok m
            else Error error_e
  else Error error_e

(** val check_e : pexpr -> pexpr -> M.t -> M.t cexec **)

let rec check_e e1 e2 m =
  match e1 with
  | Pconst n1 ->
    (match e2 with
     | Pconst n2 ->
       if eq_op coq_Z_eqType (Obj.magic n1) (Obj.magic n2)
       then Ok m
       else Error error_e
     | _ -> Error error_e)
  | Pbool b1 ->
    (match e2 with
     | Pbool b2 ->
       if eq_op bool_eqType (Obj.magic b1) (Obj.magic b2)
       then Ok m
       else Error error_e
     | _ -> Error error_e)
  | Parr_init n1 ->
    (match e2 with
     | Parr_init n2 ->
       if eq_op pos_eqType (Obj.magic n1) (Obj.magic n2)
       then Ok m
       else Error error_e
     | _ -> Error error_e)
  | Pvar x1 ->
    (match e2 with
     | Pvar x2 -> check_gv x1 x2 m
     | _ -> Error error_e)
  | Pget (aa1, w1, x1, e3) ->
    (match e2 with
     | Pget (aa2, w2, x2, e4) ->
       if (&&) (eq_op arr_access_eqType (Obj.magic aa1) (Obj.magic aa2))
            (eq_op wsize_eqType (Obj.magic w1) (Obj.magic w2))
       then (match check_gv x1 x2 m with
             | Ok x -> check_e e3 e4 x
             | Error s -> Error s)
       else Error error_e
     | _ -> Error error_e)
  | Psub (aa1, w1, len1, x1, e3) ->
    (match e2 with
     | Psub (aa2, w2, len2, x2, e4) ->
       if (&&)
            ((&&) (eq_op arr_access_eqType (Obj.magic aa1) (Obj.magic aa2))
              (eq_op wsize_eqType (Obj.magic w1) (Obj.magic w2)))
            (eq_op pos_eqType (Obj.magic len1) (Obj.magic len2))
       then (match check_gv x1 x2 m with
             | Ok x -> check_e e3 e4 x
             | Error s -> Error s)
       else Error error_e
     | _ -> Error error_e)
  | Pload (w1, x1, e3) ->
    (match e2 with
     | Pload (w2, x2, e4) ->
       if eq_op wsize_eqType (Obj.magic w1) (Obj.magic w2)
       then (match check_v x1 x2 m with
             | Ok x -> check_e e3 e4 x
             | Error s -> Error s)
       else Error error_e
     | _ -> Error error_e)
  | Papp1 (o1, e3) ->
    (match e2 with
     | Papp1 (o2, e4) ->
       if eq_op sop1_eqType (Obj.magic o1) (Obj.magic o2)
       then check_e e3 e4 m
       else Error error_e
     | _ -> Error error_e)
  | Papp2 (o1, e11, e12) ->
    (match e2 with
     | Papp2 (o2, e21, e22) ->
       if eq_op sop2_eqType (Obj.magic o1) (Obj.magic o2)
       then (match check_e e11 e21 m with
             | Ok x -> check_e e12 e22 x
             | Error s -> Error s)
       else Error error_e
     | _ -> Error error_e)
  | PappN (o1, es1) ->
    (match e2 with
     | PappN (o2, es2) ->
       if eq_op opN_eqType (Obj.magic o1) (Obj.magic o2)
       then fold2
              (alloc_error
                ('c'::('h'::('e'::('c'::('k'::('_'::('e'::(' '::('('::('a'::('p'::('p'::('N'::(')'::[])))))))))))))))
              check_e es1 es2 m
       else Error error_e
     | _ -> Error error_e)
  | Pif (t0, e, e3, e4) ->
    (match e2 with
     | Pif (t', e', e1', e2') ->
       if eq_op stype_eqType (Obj.magic t0) (Obj.magic t')
       then (match match check_e e e' m with
                   | Ok x -> check_e e3 e1' x
                   | Error s -> Error s with
             | Ok x -> check_e e4 e2' x
             | Error s -> Error s)
       else Error error_e
     | _ -> Error error_e)

(** val check_var_aux : Var.var -> Var.var -> M.t_ -> M.t cexec **)

let check_var_aux x1 x2 m =
  Ok (M.set m x1 x2)

(** val check_varc : var_i -> var_i -> M.t_ -> M.t cexec **)

let check_varc xi1 xi2 m =
  let x1 = xi1.v_var in
  let x2 = xi2.v_var in
  if M.v_wextendtyP x1 x2
  then check_var_aux x1 x2 m
  else Error
         (cerr_varalloc xi1.v_var xi2.v_var
           ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))

(** val is_Pvar : (stype * pexpr) option -> (stype * var_i) option **)

let is_Pvar = function
| Some p ->
  let (ty, p0) = p in
  (match p0 with
   | Pvar x -> if is_lvar x then Some (ty, x.gv) else None
   | _ -> None)
| None -> None

(** val error_lv : pp_error_loc **)

let error_lv =
  pp_internal_error_s
    ('a'::('l'::('l'::('o'::('c'::('a'::('t'::('i'::('o'::('n'::[]))))))))))
    ('l'::('v'::('a'::('l'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::[]))))))))))))))

(** val check_lval :
    (stype * pexpr) option -> lval -> lval -> M.t -> M.t cexec **)

let check_lval e2 x1 x2 m =
  match x1 with
  | Lnone (_, t1) ->
    (match x2 with
     | Lnone (_, t2) ->
       if wextend_type (Obj.magic t1) (Obj.magic t2)
       then Ok m
       else Error error_lv
     | Lvar x ->
       if wextend_type (Obj.magic t1) (Obj.magic Var.vtype x.v_var)
       then Ok (M.remove m (Obj.magic x.v_var))
       else Error error_lv
     | _ -> Error error_lv)
  | Lvar x3 ->
    (match x2 with
     | Lvar x4 ->
       (match is_Pvar e2 with
        | Some p ->
          let (ty, x2') = p in
          if M.v_wextendtyP x3.v_var x4.v_var
          then if (&&)
                    ((&&)
                      (eq_op stype_eqType (Obj.magic Var.vtype x3.v_var)
                        (Obj.magic ty))
                      (eq_op stype_eqType (Obj.magic Var.vtype x3.v_var)
                        (Obj.magic Var.vtype x4.v_var)))
                    (eq_op Var.var_eqType (Obj.magic x4.v_var)
                      (Obj.magic x2'.v_var))
               then Ok (M.add m x3.v_var x4.v_var)
               else check_var_aux x3.v_var x4.v_var m
          else Error
                 (cerr_varalloc x3.v_var x4.v_var
                   ('t'::('y'::('p'::('e'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))
        | None -> check_varc x3 x4 m)
     | _ -> Error error_lv)
  | Lmem (w1, x3, e1) ->
    (match x2 with
     | Lmem (w2, x4, e3) ->
       if eq_op wsize_eqType (Obj.magic w1) (Obj.magic w2)
       then (match check_v x3 x4 m with
             | Ok x -> check_e e1 e3 x
             | Error s -> Error s)
       else Error error_lv
     | _ -> Error error_lv)
  | Laset (aa1, w1, x3, e1) ->
    (match x2 with
     | Laset (aa2, w2, x4, e3) ->
       if (&&) (eq_op arr_access_eqType (Obj.magic aa1) (Obj.magic aa2))
            (eq_op wsize_eqType (Obj.magic w1) (Obj.magic w2))
       then (match match check_v x3 x4 m with
                   | Ok x -> check_e e1 e3 x
                   | Error s -> Error s with
             | Ok x -> check_varc x3 x4 x
             | Error s -> Error s)
       else Error error_lv
     | _ -> Error error_lv)
  | Lasub (aa1, w1, len1, x3, e1) ->
    (match x2 with
     | Lasub (aa2, w2, len2, x4, e3) ->
       if (&&)
            ((&&) (eq_op arr_access_eqType (Obj.magic aa1) (Obj.magic aa2))
              (eq_op wsize_eqType (Obj.magic w1) (Obj.magic w2)))
            (eq_op pos_eqType (Obj.magic len1) (Obj.magic len2))
       then (match match check_v x3 x4 m with
                   | Ok x -> check_e e1 e3 x
                   | Error s -> Error s with
             | Ok x -> check_varc x3 x4 x
             | Error s -> Error s)
       else Error error_lv
     | _ -> Error error_lv)

(** val loop :
    (M.t -> M.t cexec) -> nat -> M.t -> (pp_error_loc, M.t) result **)

let rec loop check_c n m =
  match n with
  | O -> Error E.loop_iterator
  | S n0 ->
    (match check_c m with
     | Ok x -> if M.incl m x then Ok m else loop check_c n0 (M.merge m x)
     | Error s -> Error s)

(** val loop2 :
    (M.t -> (M.t * M.t) cexec) -> nat -> M.t -> (pp_error_loc, M.t) result **)

let rec loop2 check_c2 n m =
  match n with
  | O -> Error E.loop_iterator
  | S n0 ->
    (match check_c2 m with
     | Ok x ->
       if M.incl m (snd x)
       then Ok (fst x)
       else loop2 check_c2 n0 (M.merge m (snd x))
     | Error s -> Error s)

(** val check_es :
    pexpr list -> pexpr list -> M.t -> (pp_error_loc, M.t) result **)

let check_es es1 es2 r =
  fold2 E.fold2 check_e es1 es2 r

(** val check_lvals :
    lval list -> lval list -> M.t -> (pp_error_loc, M.t) result **)

let check_lvals =
  fold2 E.fold2 (check_lval None)

(** val check_var : var_i -> var_i -> M.t -> M.t cexec **)

let check_var x1 x2 r =
  check_lval None (Lvar x1) (Lvar x2) r

(** val check_vars :
    var_i list -> var_i list -> M.t -> (pp_error_loc, M.t) result **)

let check_vars xs1 xs2 r =
  check_lvals (map (fun x -> Lvar x) xs1) (map (fun x -> Lvar x) xs2) r

(** val check_I :
    'a1 asmOp -> 'a1 instr -> 'a1 instr -> M.t_ -> (pp_error_loc, M.t_) result **)

let check_I asmop =
  let rec check_i i1 i2 r =
    match i1 with
    | Cassgn (x1, _, ty1, e1) ->
      (match i2 with
       | Cassgn (x2, _, ty2, e2) ->
         if eq_op stype_eqType (Obj.magic ty1) (Obj.magic ty2)
         then (match check_e e1 e2 r with
               | Ok x -> check_lval (Some (ty2, e2)) x1 x2 x
               | Error s -> Error s)
         else Error
                (alloc_error
                  ('b'::('a'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('i'::('n'::(' '::('a'::('s'::('s'::('i'::('g'::('n'::('m'::('e'::('n'::('t'::[])))))))))))))))))))))))
       | _ ->
         Error
           (alloc_error
             ('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))
    | Copn (xs1, _, o1, es1) ->
      (match i2 with
       | Copn (xs2, _, o2, es2) ->
         if eq_op (sopn_eqType asmop) (Obj.magic o1) (Obj.magic o2)
         then (match check_es es1 es2 r with
               | Ok x -> check_lvals xs1 xs2 x
               | Error s -> Error s)
         else Error
                (alloc_error
                  ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))
       | _ ->
         Error
           (alloc_error
             ('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))
    | Csyscall (xs1, o1, es1) ->
      (match i2 with
       | Csyscall (xs2, o2, es2) ->
         if eq_op syscall_t_eqType (Obj.magic o1) (Obj.magic o2)
         then (match check_es es1 es2 r with
               | Ok x -> check_lvals xs1 xs2 x
               | Error s -> Error s)
         else Error
                (alloc_error
                  ('o'::('p'::('e'::('r'::('a'::('t'::('o'::('r'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))
       | _ ->
         Error
           (alloc_error
             ('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))
    | Cif (e1, c11, c12) ->
      (match i2 with
       | Cif (e2, c21, c22) ->
         (match check_e e1 e2 r with
          | Ok x ->
            (match fold2 E.fold2 check_I0 c11 c21 x with
             | Ok x0 ->
               (match fold2 E.fold2 check_I0 c12 c22 x with
                | Ok x1 -> Ok (M.merge x0 x1)
                | Error s -> Error s)
             | Error s -> Error s)
          | Error s -> Error s)
       | _ ->
         Error
           (alloc_error
             ('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))
    | Cfor (x1, r0, c1) ->
      let (p, hi1) = r0 in
      let (d1, lo1) = p in
      (match i2 with
       | Cfor (x2, r1, c2) ->
         let (p0, hi2) = r1 in
         let (d2, lo2) = p0 in
         if eq_op dir_eqType (Obj.magic d1) (Obj.magic d2)
         then (match match check_e lo1 lo2 r with
                     | Ok x -> check_e hi1 hi2 x
                     | Error s -> Error s with
               | Ok x ->
                 let check_c = fun r2 ->
                   match check_var x1 x2 r2 with
                   | Ok x0 -> fold2 E.fold2 check_I0 c1 c2 x0
                   | Error s -> Error s
                 in
                 loop check_c Loop.nb x
               | Error s -> Error s)
         else Error
                (alloc_error
                  ('l'::('o'::('o'::('p'::(' '::('d'::('i'::('r'::('e'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))))
       | _ ->
         Error
           (alloc_error
             ('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))
    | Cwhile (_, c1, e1, c1') ->
      (match i2 with
       | Cwhile (_, c2, e2, c2') ->
         let check_c = fun r0 ->
           match fold2 E.fold2 check_I0 c1 c2 r0 with
           | Ok x ->
             (match check_e e1 e2 x with
              | Ok x0 ->
                (match fold2 E.fold2 check_I0 c1' c2' x0 with
                 | Ok x1 -> Ok (x0, x1)
                 | Error s -> Error s)
              | Error s -> Error s)
           | Error s -> Error s
         in
         (match loop2 check_c Loop.nb r with
          | Ok x -> Ok x
          | Error s -> Error s)
       | _ ->
         Error
           (alloc_error
             ('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))
    | Ccall (_, x1, f1, arg1) ->
      (match i2 with
       | Ccall (_, x2, f2, arg2) ->
         if eq_op pos_eqType (Obj.magic f1) (Obj.magic f2)
         then (match check_es arg1 arg2 r with
               | Ok x -> check_lvals x1 x2 x
               | Error s -> Error s)
         else Error
                (alloc_error
                  ('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))
       | _ ->
         Error
           (alloc_error
             ('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))))
  and check_I0 i1 i2 r =
    let MkI (_, i3) = i1 in let MkI (_, i4) = i2 in check_i i3 i4 r
  in check_I0

(** val check_cmd :
    'a1 asmOp -> 'a1 instr list -> 'a1 instr list -> M.t_ -> (pp_error_loc,
    M.t_) result **)

let check_cmd asmop =
  fold2 E.fold2 (check_I asmop)

(** val check_fundef :
    'a1 asmOp -> Equality.coq_type -> progT -> (Equality.sort -> extra_prog_t
    -> extra_prog_t -> M.t cexec) -> extra_prog_t -> extra_prog_t ->
    (funname * 'a1 fundef) -> (funname * 'a1 fundef) -> unit -> unit cexec **)

let check_fundef asmop t0 pT init_alloc ep1 ep2 f1 f2 _ =
  let (f3, fd1) = f1 in
  let (f4, fd2) = f2 in
  add_funname f3
    (add_finfo fd1.f_info
      (if (&&)
            ((&&)
              ((&&) (eq_op pos_eqType (Obj.magic f3) (Obj.magic f4))
                (eq_op (seq_eqType stype_eqType) (Obj.magic fd1.f_tyin)
                  (Obj.magic fd2.f_tyin)))
              (eq_op (seq_eqType stype_eqType) (Obj.magic fd1.f_tyout)
                (Obj.magic fd2.f_tyout)))
            (eq_op (extra_fun_t t0 pT) fd1.f_extra fd2.f_extra)
       then (match init_alloc fd1.f_extra ep1 ep2 with
             | Ok x ->
               (match check_vars fd1.f_params fd2.f_params x with
                | Ok x0 ->
                  (match check_cmd asmop fd1.f_body fd2.f_body x0 with
                   | Ok x1 ->
                     let es1 = map coq_Plvar fd1.f_res in
                     let es2 = map coq_Plvar fd2.f_res in
                     (match check_es es1 es2 x1 with
                      | Ok _ -> Ok ()
                      | Error s -> Error s)
                   | Error s -> Error s)
                | Error s -> Error s)
             | Error s -> Error s)
       else Error
              (E.error
                ('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('q'::('u'::('a'::('l'::('s'::[])))))))))))))))))))))))

(** val check_prog_error : pp_error_loc **)

let check_prog_error =
  alloc_error
    ('c'::('h'::('e'::('c'::('k'::('_'::('f'::('u'::('n'::('d'::('e'::('f'::(' '::('('::('f'::('o'::('l'::('d'::('2'::(')'::[]))))))))))))))))))))

(** val check_prog :
    'a1 asmOp -> Equality.coq_type -> progT -> (Equality.sort -> extra_prog_t
    -> extra_prog_t -> M.t cexec) -> extra_prog_t -> (funname * 'a1 fundef)
    list -> extra_prog_t -> (funname * 'a1 fundef) list -> (pp_error_loc,
    unit) result **)

let check_prog asmop t0 pT init_alloc ep1 p_funcs1 ep2 p_funcs2 =
  fold2 check_prog_error (check_fundef asmop t0 pT init_alloc ep1 ep2)
    p_funcs1 p_funcs2 ()

(** val init_alloc_uprog :
    Equality.sort -> extra_prog_t -> extra_prog_t -> M.t cexec **)

let init_alloc_uprog _ _ _ =
  Ok M.empty

(** val check_ufundef :
    'a1 asmOp -> extra_prog_t -> extra_prog_t -> (funname * 'a1 fundef) ->
    (funname * 'a1 fundef) -> unit -> unit cexec **)

let check_ufundef asmop =
  check_fundef asmop
    (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
    progUnit init_alloc_uprog

(** val check_uprog :
    'a1 asmOp -> extra_prog_t -> (funname * 'a1 fundef) list -> extra_prog_t
    -> (funname * 'a1 fundef) list -> (pp_error_loc, unit) result **)

let check_uprog asmop =
  check_prog asmop
    (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
    progUnit init_alloc_uprog

(** val init_alloc_sprog :
    coq_PointerData -> Equality.sort -> extra_prog_t -> extra_prog_t -> M.t
    cexec **)

let init_alloc_sprog pd _ ep1 ep2 =
  check_vars ({ v_var = { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
    (Obj.magic ep1).sp_rsp }; v_info = dummy_var_info } :: ({ v_var =
    { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
    (Obj.magic ep1).sp_rip }; v_info = dummy_var_info } :: [])) ({ v_var =
    { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
    (Obj.magic ep2).sp_rsp }; v_info = dummy_var_info } :: ({ v_var =
    { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
    (Obj.magic ep2).sp_rip }; v_info = dummy_var_info } :: [])) M.empty

(** val check_sprog :
    'a1 asmOp -> coq_PointerData -> extra_prog_t -> (funname * 'a1 fundef)
    list -> extra_prog_t -> (funname * 'a1 fundef) list -> (pp_error_loc,
    unit) result **)

let check_sprog asmop pd =
  check_prog asmop
    (Equality.clone sfe_eqType (Obj.magic sfe_eqMixin) (fun x -> x))
    (progStack pd) (init_alloc_sprog pd)
