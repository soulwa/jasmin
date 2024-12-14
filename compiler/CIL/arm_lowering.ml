open BinNums
open Arch_extra
open Arm_decl
open Arm_extra
open Arm_instr_decl
open Eqtype
open Expr
open Lowering
open Seq
open Shift_kind
open Sopn
open Ssrbool
open Type
open Utils0
open Var0
open Wsize

type __ = Obj.t

type fresh_vars = { fv_NF : Equality.sort; fv_ZF : Equality.sort;
                    fv_CF : Equality.sort; fv_VF : Equality.sort }

(** val all_fresh_vars : fresh_vars -> Equality.sort list **)

let all_fresh_vars fv =
  fv.fv_NF :: (fv.fv_ZF :: (fv.fv_CF :: (fv.fv_VF :: [])))

(** val fvNF : fresh_vars -> Var.var **)

let fvNF fv =
  { Var.vtype = Coq_sbool; Var.vname = fv.fv_NF }

(** val fvZF : fresh_vars -> Var.var **)

let fvZF fv =
  { Var.vtype = Coq_sbool; Var.vname = fv.fv_ZF }

(** val fvCF : fresh_vars -> Var.var **)

let fvCF fv =
  { Var.vtype = Coq_sbool; Var.vname = fv.fv_CF }

(** val fvVF : fresh_vars -> Var.var **)

let fvVF fv =
  { Var.vtype = Coq_sbool; Var.vname = fv.fv_VF }

(** val fresh_flags : fresh_vars -> Var.var list **)

let fresh_flags fv =
  (fvNF fv) :: ((fvZF fv) :: ((fvCF fv) :: ((fvVF fv) :: [])))

(** val fvars : fresh_vars -> SvExtra.Sv.t **)

let fvars fv =
  SvExtra.sv_of_list (fun x -> Obj.magic x) (fresh_flags fv)

(** val mk_fv_vari : Var.var -> var_i **)

let mk_fv_vari x =
  { v_var = x; v_info = dummy_var_info }

(** val mk_fv_gvar : Var.var -> gvar **)

let mk_fv_gvar x =
  { gv = (mk_fv_vari x); gs = Slocal }

(** val lflags_of_mn : fresh_vars -> arm_mnemonic -> lval list **)

let lflags_of_mn fv mn =
  let ids =
    match mn with
    | ADD -> []
    | ADC -> []
    | MUL -> []
    | SDIV -> []
    | SUB -> []
    | RSB -> []
    | UDIV -> []
    | UMULL -> []
    | AND -> []
    | BIC -> []
    | EOR -> []
    | MVN -> []
    | ORR -> []
    | ASR -> []
    | LSL -> []
    | LSR -> []
    | ROR -> []
    | MOV -> []
    | MOVT -> []
    | UBFX -> []
    | UXTB -> []
    | UXTH -> []
    | SBFX -> []
    | CMP ->
      (fun f -> f.fv_NF) :: ((fun f -> f.fv_ZF) :: ((fun f ->
        f.fv_CF) :: ((fun f -> f.fv_VF) :: [])))
    | TST ->
      (fun f -> f.fv_NF) :: ((fun f -> f.fv_ZF) :: ((fun f -> f.fv_CF) :: []))
    | _ -> []
  in
  map (fun x -> Lvar
    (mk_fv_vari { Var.vtype = Coq_sbool; Var.vname = (x fv) })) ids

(** val lower_TST : pexpr -> pexpr -> pexpr list option **)

let lower_TST e0 e1 =
  match e0 with
  | Papp2 (s, e00, e01) ->
    (match s with
     | Oland _ ->
       (match e1 with
        | Papp1 (s0, p) ->
          (match s0 with
           | Oword_of_int _ ->
             (match p with
              | Pconst z ->
                (match z with
                 | Z0 -> Some (e00 :: (e01 :: []))
                 | _ -> None)
              | _ -> None)
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | _ -> None

(** val lower_condition_Papp2 :
    fresh_vars -> sop2 -> pexpr -> pexpr -> ((arm_mnemonic * pexpr) * pexpr
    list) option **)

let lower_condition_Papp2 fv op e0 e1 =
  let eNF = Pvar (mk_fv_gvar (fvNF fv)) in
  let eZF = Pvar (mk_fv_gvar (fvZF fv)) in
  let eCF = Pvar (mk_fv_gvar (fvCF fv)) in
  let eVF = Pvar (mk_fv_gvar (fvVF fv)) in
  let cmp = fun e -> Some ((CMP, e), (e0 :: (e1 :: []))) in
  (match op with
   | Oeq o ->
     (match o with
      | Op_int -> None
      | Op_w w ->
        (match w with
         | U32 ->
           (match lower_TST e0 e1 with
            | Some es -> Some ((TST, eZF), es)
            | None -> cmp eZF)
         | _ -> None))
   | Oneq o ->
     (match o with
      | Op_int -> None
      | Op_w w -> (match w with
                   | U32 -> cmp (enot eZF)
                   | _ -> None))
   | Olt c ->
     (match c with
      | Cmp_int -> None
      | Cmp_w (s, w) ->
        (match s with
         | Signed -> (match w with
                      | U32 -> cmp (eneq eNF eVF)
                      | _ -> None)
         | Unsigned -> (match w with
                        | U32 -> cmp (enot eCF)
                        | _ -> None)))
   | Ole c ->
     (match c with
      | Cmp_int -> None
      | Cmp_w (s, w) ->
        (match s with
         | Signed ->
           (match w with
            | U32 -> cmp (eor eZF (eneq eNF eVF))
            | _ -> None)
         | Unsigned ->
           (match w with
            | U32 -> cmp (eor (enot eCF) eZF)
            | _ -> None)))
   | Ogt c ->
     (match c with
      | Cmp_int -> None
      | Cmp_w (s, w) ->
        (match s with
         | Signed ->
           (match w with
            | U32 -> cmp (eand (enot eZF) (eeq eNF eVF))
            | _ -> None)
         | Unsigned ->
           (match w with
            | U32 -> cmp (eand eCF (enot eZF))
            | _ -> None)))
   | Oge c ->
     (match c with
      | Cmp_int -> None
      | Cmp_w (s, w) ->
        (match s with
         | Signed -> (match w with
                      | U32 -> cmp (eeq eNF eVF)
                      | _ -> None)
         | Unsigned -> (match w with
                        | U32 -> cmp eCF
                        | _ -> None)))
   | _ -> None)

(** val lower_condition_pexpr :
    fresh_vars -> pexpr -> (((lval list * (register, __, __, rflag, condt,
    arm_op, __) extended_op sopn) * pexpr list) * pexpr) option **)

let lower_condition_pexpr fv = function
| Papp2 (op, e0, e1) ->
  (match lower_condition_Papp2 fv op e0 e1 with
   | Some p ->
     let (p0, es) = p in
     let (mn, e') = p0 in
     Some ((((lflags_of_mn fv mn), (coq_Oarm (ARM_op (mn, default_opts)))),
     es), e')
   | None -> None)
| _ -> None

(** val lower_condition :
    fresh_vars -> pexpr -> (register, __, __, rflag, condt, arm_op, __)
    extended_op instr_r list * pexpr **)

let lower_condition fv e =
  match lower_condition_pexpr fv e with
  | Some p ->
    let (p0, c) = p in
    let (p1, es) = p0 in
    let (lvs, op) = p1 in (((Copn (lvs, AT_none, op, es)) :: []), c)
  | None -> ([], e)

(** val get_arg_shift :
    wsize -> pexpr list -> ((pexpr * shift_kind) * pexpr) option **)

let get_arg_shift ws = function
| [] -> None
| p :: l ->
  (match p with
   | Papp2 (op, v, n) ->
     (match v with
      | Pvar _ ->
        (match n with
         | Papp1 (s, p0) ->
           (match s with
            | Oword_of_int w ->
              (match w with
               | U8 ->
                 (match p0 with
                  | Pconst z ->
                    (match l with
                     | [] ->
                       (match shift_of_sop2 ws op with
                        | Some sh ->
                          if check_shift_amount sh z
                          then Some ((v, sh), n)
                          else None
                        | None -> None)
                     | _ :: _ -> None)
                  | _ -> None)
               | _ -> None)
            | _ -> None)
         | _ -> None)
      | _ -> None)
   | _ -> None)

(** val arg_shift :
    arm_mnemonic -> wsize -> pexpr list -> arm_op * pexpr list **)

let arg_shift mn ws e =
  if in_mem (Obj.magic mn)
       (mem (seq_predType arm_mnemonic_eqType)
         (Obj.magic has_shift_mnemonics))
  then (match get_arg_shift ws e with
        | Some p ->
          let (p0, esham) = p in
          let (ebase, sh) = p0 in
          let osh = Some sh in
          let es = ebase :: (esham :: []) in
          let opts = { set_flags = false; is_conditional = false; has_shift =
            osh }
          in
          ((ARM_op (mn, opts)), es)
        | None ->
          let osh = None in
          let opts = { set_flags = false; is_conditional = false; has_shift =
            osh }
          in
          ((ARM_op (mn, opts)), e))
  else let osh = None in
       let opts = { set_flags = false; is_conditional = false; has_shift =
         osh }
       in
       ((ARM_op (mn, opts)), e)

(** val lower_Pvar :
    (var_i -> bool) -> wsize -> gvar -> (arm_op * pexpr list) option **)

let lower_Pvar is_var_in_memory ws v =
  match ws with
  | U32 ->
    let mn = if is_var_in_memory v.gv then LDR else MOV in
    Some ((ARM_op (mn, default_opts)), ((Pvar v) :: []))
  | _ -> None

(** val lower_Pload :
    wsize -> wsize -> var_i -> pexpr -> (arm_op * pexpr list) option **)

let lower_Pload ws ws' v e =
  match ws with
  | U32 -> Some ((ARM_op (LDR, default_opts)), ((Pload (ws', v, e)) :: []))
  | _ -> None

(** val is_load : (var_i -> bool) -> pexpr -> bool **)

let is_load is_var_in_memory = function
| Pvar g ->
  let { gv = x; gs = gs0 } = g in
  (match gs0 with
   | Slocal -> is_var_in_memory x
   | Sglob -> true)
| Pget (_, _, _, _) -> true
| Pload (_, _, _) -> true
| _ -> false

(** val lower_Papp1 :
    (var_i -> bool) -> wsize -> sop1 -> pexpr -> (arm_op * pexpr list) option **)

let lower_Papp1 is_var_in_memory ws op e =
  match ws with
  | U32 ->
    (match op with
     | Oword_of_int w ->
       (match w with
        | U32 -> Some ((ARM_op (MOV, default_opts)), ((Papp1 (op, e)) :: []))
        | _ -> None)
     | Osignext (w, ws') ->
       (match w with
        | U32 ->
          if is_load is_var_in_memory e
          then (match sload_mn_of_wsize ws' with
                | Some mn -> Some ((ARM_op (mn, default_opts)), (e :: []))
                | None -> None)
          else None
        | _ -> None)
     | Ozeroext (w, ws') ->
       (match w with
        | U32 ->
          if is_load is_var_in_memory e
          then (match uload_mn_of_wsize ws' with
                | Some mn -> Some ((ARM_op (mn, default_opts)), (e :: []))
                | None -> None)
          else None
        | _ -> None)
     | Olnot w ->
       (match w with
        | U32 -> Some (arg_shift MVN U32 (e :: []))
        | _ -> None)
     | _ -> None)
  | _ -> None

(** val lower_Papp2_op :
    wsize -> sop2 -> pexpr -> pexpr -> ((arm_mnemonic * pexpr) * pexpr list)
    option **)

let lower_Papp2_op ws op e0 e1 =
  match ws with
  | U32 ->
    (match op with
     | Oadd o ->
       (match o with
        | Op_int -> None
        | Op_w _ -> Some ((ADD, e0), (e1 :: [])))
     | Omul o ->
       (match o with
        | Op_int -> None
        | Op_w _ -> Some ((MUL, e0), (e1 :: [])))
     | Osub o ->
       (match o with
        | Op_int -> None
        | Op_w _ -> Some ((SUB, e0), (e1 :: [])))
     | Odiv c ->
       (match c with
        | Cmp_int -> None
        | Cmp_w (unigned, w) ->
          (match unigned with
           | Signed ->
             (match w with
              | U32 -> Some ((SDIV, e0), (e1 :: []))
              | _ -> None)
           | Unsigned ->
             (match w with
              | U32 -> Some ((UDIV, e0), (e1 :: []))
              | _ -> None)))
     | Oland _ -> Some ((AND, e0), (e1 :: []))
     | Olor _ -> Some ((ORR, e0), (e1 :: []))
     | Olxor _ -> Some ((EOR, e0), (e1 :: []))
     | Olsr w ->
       (match w with
        | U32 ->
          if is_zero (Obj.magic U8) e1
          then Some ((MOV, e0), [])
          else Some ((LSR, e0), (e1 :: []))
        | _ -> None)
     | Olsl o ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 -> Some ((LSL, e0), (e1 :: []))
           | _ -> None))
     | Oasr o ->
       (match o with
        | Op_int -> None
        | Op_w w ->
          (match w with
           | U32 ->
             if is_zero (Obj.magic U8) e1
             then Some ((MOV, e0), [])
             else Some ((ASR, e0), (e1 :: []))
           | _ -> None))
     | Oror w ->
       (match w with
        | U32 ->
          if is_zero (Obj.magic U8) e1
          then Some ((MOV, e0), [])
          else Some ((ROR, e0), (e1 :: []))
        | _ -> None)
     | _ -> None)
  | _ -> None

(** val lower_Papp2 :
    wsize -> sop2 -> pexpr -> pexpr -> (arm_op * pexpr list) option **)

let lower_Papp2 ws op e0 e1 =
  match lower_Papp2_op ws op e0 e1 with
  | Some p ->
    let (p0, e1') = p in
    let (mn, e0') = p0 in
    let (aop, es) = arg_shift mn ws e1' in Some (aop, (e0' :: es))
  | None -> None

(** val lower_pexpr_aux :
    (var_i -> bool) -> wsize -> pexpr -> (arm_op * pexpr list) option **)

let lower_pexpr_aux is_var_in_memory ws = function
| Pvar v -> lower_Pvar is_var_in_memory ws v
| Pload (ws', v, e0) -> lower_Pload ws ws' v e0
| Papp1 (op, e0) -> lower_Papp1 is_var_in_memory ws op e0
| Papp2 (op, a, b) -> lower_Papp2 ws op a b
| _ -> None

(** val no_pre :
    (arm_op * pexpr list) option -> (((register, __, __, rflag, condt,
    arm_op, __) extended_op instr_r list * arm_op) * pexpr list) option **)

let no_pre = function
| Some p -> let (aop, es) = p in Some (([], aop), es)
| None -> None

(** val lower_pexpr :
    fresh_vars -> (var_i -> bool) -> wsize -> pexpr -> (((register, __, __,
    rflag, condt, arm_op, __) extended_op instr_r list * arm_op) * pexpr
    list) option **)

let lower_pexpr fv is_var_in_memory ws e = match e with
| Pif (s, c, e0, e1) ->
  (match s with
   | Coq_sword ws' ->
     (match lower_pexpr_aux is_var_in_memory ws e0 with
      | Some p ->
        let (a, es) = p in
        let ARM_op (mn, opts) = a in
        if eq_op wsize_eqType (Obj.magic ws) (Obj.magic ws')
        then let (pre, c') = lower_condition fv c in
             Some ((pre, (ARM_op (mn, (set_is_conditional opts)))),
             (cat es (c' :: (e1 :: []))))
        else None
      | None -> None)
   | _ -> no_pre (lower_pexpr_aux is_var_in_memory ws e))
| _ -> no_pre (lower_pexpr_aux is_var_in_memory ws e)

(** val lower_store : wsize -> pexpr -> (arm_op * pexpr list) option **)

let lower_store ws e =
  match store_mn_of_wsize ws with
  | Some mn ->
    let args =
      match e with
      | Pconst _ -> None
      | Pbool _ -> None
      | Parr_init _ -> None
      | Pvar _ -> Some (default_opts, (e :: []))
      | Pif (_, c, e0, e1) ->
        Some ((set_is_conditional default_opts), (e0 :: (c :: (e1 :: []))))
      | _ -> None
    in
    (match args with
     | Some p -> let (opts, es) = p in Some ((ARM_op (mn, opts)), es)
     | None -> None)
  | None -> None

(** val lower_cassgn :
    fresh_vars -> (var_i -> bool) -> lval -> stype -> pexpr -> ((register,
    __, __, rflag, condt, arm_op, __) extended_op instr_r list * ((lval
    list * (register, __, __, rflag, condt, arm_op, __) extended_op
    sopn) * pexpr list)) option **)

let lower_cassgn fv is_var_in_memory lv ty e =
  match ty with
  | Coq_sword ws ->
    let le =
      if is_lval_in_memory is_var_in_memory lv
      then no_pre (lower_store ws e)
      else lower_pexpr fv is_var_in_memory ws e
    in
    (match le with
     | Some p ->
       let (p0, es) = p in
       let (pre, aop) = p0 in Some (pre, (((lv :: []), (coq_Oarm aop)), es))
     | None -> None)
  | _ -> None

(** val lower_add_carry :
    lval list -> pexpr list -> ((lval list * (register, __, __, rflag, condt,
    arm_op, __) extended_op sopn) * pexpr list) option **)

let lower_add_carry lvs es =
  match lvs with
  | [] -> None
  | cf :: l ->
    (match l with
     | [] -> None
     | r :: l0 ->
       (match l0 with
        | [] ->
          (match es with
           | [] -> None
           | x :: l1 ->
             (match l1 with
              | [] -> None
              | y :: l2 ->
                (match l2 with
                 | [] -> None
                 | b :: l3 ->
                   (match l3 with
                    | [] ->
                      let args =
                        match b with
                        | Pconst _ -> None
                        | Pbool b0 ->
                          if b0 then None else Some (ADD, (x :: (y :: [])))
                        | Pvar _ -> Some (ADC, es)
                        | _ -> None
                      in
                      (match args with
                       | Some p ->
                         let (mn, es') = p in
                         let opts = { set_flags = true; is_conditional =
                           false; has_shift = None }
                         in
                         let lnoneb = Lnone (dummy_var_info, Coq_sbool) in
                         let lvs' =
                           lnoneb :: (lnoneb :: (cf :: (lnoneb :: (r :: []))))
                         in
                         Some ((lvs', (Oasm (BaseOp (None, (ARM_op (mn,
                         opts)))))), es')
                       | None -> None)
                    | _ :: _ -> None))))
        | _ :: _ -> None))

(** val lower_base_op :
    lval list -> arm_op -> pexpr list -> ((lval list * (register, __, __,
    rflag, condt, arm_op, __) extended_op sopn) * pexpr list) option **)

let lower_base_op lvs aop es =
  let ARM_op (mn, opts) = aop in
  if in_mem (Obj.magic mn)
       (mem (seq_predType arm_mnemonic_eqType)
         (Obj.magic has_shift_mnemonics))
  then Some ((lvs, (Oasm (BaseOp (None, (ARM_op (mn, opts)))))), es)
  else None

(** val lower_copn :
    lval list -> (register, __, __, rflag, condt, arm_op, __) extended_op
    sopn -> pexpr list -> ((lval list * (register, __, __, rflag, condt,
    arm_op, __) extended_op sopn) * pexpr list) option **)

let lower_copn lvs op es =
  match op with
  | Oaddcarry w -> (match w with
                    | U32 -> lower_add_carry lvs es
                    | _ -> None)
  | Oasm a ->
    (match a with
     | BaseOp a0 ->
       let (o, aop) = a0 in
       (match o with
        | Some _ -> None
        | None -> lower_base_op lvs aop es)
     | ExtOp _ -> None)
  | _ -> None

type lowering_options = unit

(** val lower_i :
    fresh_vars -> (var_i -> bool) -> (register, __, __, rflag, condt, arm_op,
    __) extended_op instr -> (register, __, __, rflag, condt, arm_op, __)
    extended_op instr list **)

let rec lower_i fv is_var_in_memory i = match i with
| MkI (ii, ir) ->
  (match ir with
   | Cassgn (lv, tag, ty, e) ->
     let irs =
       match lower_cassgn fv is_var_in_memory lv ty e with
       | Some p ->
         let (pre, p0) = p in
         let (p1, es) = p0 in
         let (lvs, op) = p1 in cat pre ((Copn (lvs, tag, op, es)) :: [])
       | None -> ir :: []
     in
     map (fun x -> MkI (ii, x)) irs
   | Copn (lvs, tag, op, es) ->
     let ir' =
       match lower_copn lvs op es with
       | Some p ->
         let (p0, es') = p in
         let (lvs', op') = p0 in Copn (lvs', tag, op', es')
       | None -> ir
     in
     (MkI (ii, ir')) :: []
   | Cif (e, c1, c2) ->
     let (pre, e') = lower_condition fv e in
     let c1' = conc_map (lower_i fv is_var_in_memory) c1 in
     let c2' = conc_map (lower_i fv is_var_in_memory) c2 in
     map (fun x -> MkI (ii, x)) (cat pre ((Cif (e', c1', c2')) :: []))
   | Cfor (v, r, c) ->
     let c' = conc_map (lower_i fv is_var_in_memory) c in
     (MkI (ii, (Cfor (v, r, c')))) :: []
   | Cwhile (a, c0, e, c1) ->
     let (pre, e') = lower_condition fv e in
     let c0' = conc_map (lower_i fv is_var_in_memory) c0 in
     let c1' = conc_map (lower_i fv is_var_in_memory) c1 in
     (MkI (ii, (Cwhile (a, (cat c0' (map (fun x -> MkI (ii, x)) pre)), e',
     c1')))) :: []
   | _ -> i :: [])
