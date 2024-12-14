open BinInt
open BinNums
open Datatypes
open Eqtype
open Expr
open Flag_combination
open Global
open Low_memory
open Memory_model
open Sem_op_typed
open Sem_pexpr_params
open Sem_type
open Sopn
open Ssralg
open Ssrbool
open Ssreflect
open Syscall
open Type
open Utils0
open Values
open Var0
open Warray_
open Word0
open Wsize
open Xseq

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val undef_addr : stype -> sem_t exec **)

let undef_addr = function
| Coq_sarr n -> Ok (Obj.magic WArray.empty n)
| _ -> undef_error

(** val vmap0 : sem_t exec Fv.t **)

let vmap0 =
  Fv.empty (fun x -> undef_addr (Var.vtype x))

(** val on_vu : ('a1 -> 'a2) -> 'a2 exec -> 'a1 exec -> 'a2 exec **)

let on_vu fv fu = function
| Ok v0 -> Ok (fv v0)
| Error e -> (match e with
              | ErrAddrUndef -> fu
              | _ -> Error e)

(** val on_vuP :
    ('a1 -> 'a2) -> 'a2 exec -> 'a1 exec -> 'a2 -> ('a1 -> __ -> __ -> 'a3)
    -> (__ -> __ -> 'a3) -> 'a3 **)

let on_vuP _ _ v _ x x0 =
  let _evar_0_ = fun a hfv _ -> hfv a __ __ in
  let _evar_0_0 = fun __top_assumption_ ->
    let _evar_0_0 = fun _ _ -> assert false (* absurd case *) in
    let _evar_0_1 = fun _ hfu -> hfu __ in
    let _evar_0_2 = fun _ _ -> assert false (* absurd case *) in
    let _evar_0_3 = fun _ _ -> assert false (* absurd case *) in
    let _evar_0_4 = fun _ _ -> assert false (* absurd case *) in
    (match __top_assumption_ with
     | ErrOob -> (fun hfv hfu _ -> _evar_0_0 hfv hfu)
     | ErrAddrUndef -> _evar_0_1
     | ErrAddrInvalid -> (fun hfv hfu _ -> _evar_0_2 hfv hfu)
     | ErrStack -> (fun hfv hfu _ -> _evar_0_3 hfv hfu)
     | ErrType -> (fun hfv hfu _ -> _evar_0_4 hfv hfu))
  in
  (match v with
   | Ok a -> _evar_0_ a x x0
   | Error e -> _evar_0_0 e x x0 __)

(** val get_var : sem_t exec Fv.t -> Var.var -> value exec **)

let get_var m x =
  on_vu (to_val (Var.vtype x)) undef_error (Fv.get m x)

(** val set_var :
    sem_t exec Fv.t -> Var.var -> value -> sem_t exec Fv.t exec **)

let set_var m x v =
  on_vu (fun v0 -> Fv.set m x (Ok v0))
    (if is_sbool (Obj.magic Var.vtype x)
     then Ok (Fv.set m x (undef_addr (Var.vtype x)))
     else type_error) (of_val (Var.vtype x) v)

(** val set_varP :
    sem_t exec Fv.t -> sem_t exec Fv.t -> Var.var -> value -> (sem_t -> __ ->
    __ -> 'a1) -> (__ -> __ -> __ -> 'a1) -> 'a1 **)

let set_varP m m' x v h1 h2 =
  on_vuP (fun v0 -> Fv.set m x (Ok v0))
    (if is_sbool (Obj.magic Var.vtype x)
     then Ok (Fv.set m x (undef_addr (Var.vtype x)))
     else type_error) (of_val (Var.vtype x) v) m' h1
    (let _evar_0_ = fun _ _ _ -> h2 __ __ __ in
     let _evar_0_0 = fun _ _ _ -> assert false (* absurd case *) in
     (match ifPn (is_sbool (Obj.magic Var.vtype x)) (Ok
              (Fv.set m x (undef_addr (Var.vtype x)))) type_error with
      | IfSpecTrue -> _evar_0_ __
      | IfSpecFalse -> _evar_0_0 __))

(** val sem_sop1 : sop1 -> value -> value exec **)

let sem_sop1 o v =
  match of_val (fst (type_of_op1 o)) v with
  | Ok x -> Ok (to_val (snd (type_of_op1 o)) (sem_sop1_typed o x))
  | Error s -> Error s

(** val sem_sop2 : sop2 -> value -> value -> value exec **)

let sem_sop2 o v1 v2 =
  match of_val (fst (fst (type_of_op2 o))) v1 with
  | Ok x ->
    (match of_val (snd (fst (type_of_op2 o))) v2 with
     | Ok x0 ->
       (match sem_sop2_typed o x x0 with
        | Ok x1 -> Ok (to_val (snd (type_of_op2 o)) x1)
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val sem_opN : coq_FlagCombinationParams -> opN -> values -> value exec **)

let sem_opN cfcd op vs =
  match app_sopn (fst (type_of_opN op)) (sem_opN_typed cfcd op) vs with
  | Ok x -> Ok (to_val (snd (type_of_opN op)) x)
  | Error s -> Error s

type 'syscall_state estate = { escs : 'syscall_state syscall_state_t;
                               emem : Memory.mem; evm : sem_t exec Fv.t }

(** val escs :
    coq_PointerData -> 'a1 syscall_sem -> 'a1 estate -> 'a1 syscall_state_t **)

let escs _ _ e =
  e.escs

(** val emem :
    coq_PointerData -> 'a1 syscall_sem -> 'a1 estate -> Memory.mem **)

let emem _ _ e =
  e.emem

(** val evm :
    coq_PointerData -> 'a1 syscall_sem -> 'a1 estate -> sem_t exec Fv.t **)

let evm _ _ e =
  e.evm

(** val get_global_value : glob_decl list -> Var.var -> glob_value option **)

let get_global_value gd g =
  assoc Var.var_eqType (Obj.magic gd) (Obj.magic g)

(** val gv2val : glob_value -> value **)

let gv2val = function
| Gword (ws, w) -> Vword (ws, w)
| Garr (p, a) -> Varr (p, a)

(** val get_global : glob_decl list -> Var.var -> value exec **)

let get_global gd g =
  match get_global_value gd g with
  | Some ga ->
    let v = gv2val ga in
    if eq_op stype_eqType (Obj.magic type_of_val v) (Obj.magic Var.vtype g)
    then Ok v
    else type_error
  | None -> type_error

(** val get_gvar : glob_decl list -> sem_t exec Fv.t -> gvar -> value exec **)

let get_gvar gd vm x =
  if is_lvar x then get_var vm x.gv.v_var else get_global gd x.gv.v_var

(** val on_arr_var :
    value exec -> (positive -> WArray.array -> 'a1 exec) -> (error, 'a1)
    result **)

let on_arr_var v f =
  match v with
  | Ok x -> (match x with
             | Varr (n, t0) -> f n t0
             | _ -> type_error)
  | Error s -> Error s

(** val on_arr_varP :
    coq_PointerData -> 'a1 syscall_sem -> (positive -> WArray.array -> 'a2
    exec) -> 'a2 -> 'a1 estate -> Var.var -> (positive -> WArray.array -> __
    -> __ -> __ -> 'a3) -> 'a3 **)

let on_arr_varP _ _ f v s x h =
  rbindP (get_var s.evm x) (fun v0 ->
    match v0 with
    | Varr (n, t0) -> f n t0
    | _ -> type_error) v (fun vx _ ->
    ssr_have __ (fun _ ->
      let _evar_0_ = fun _ -> assert false (* absurd case *) in
      let _evar_0_0 = fun _ -> assert false (* absurd case *) in
      let _evar_0_1 = fun len t0 -> h len t0 __ in
      let _evar_0_2 = fun _ _ -> assert false (* absurd case *) in
      let _evar_0_3 = fun _ -> assert false (* absurd case *) in
      (match vx with
       | Vbool b -> (fun _ -> _evar_0_ b)
       | Vint z -> (fun _ -> _evar_0_0 z)
       | Varr (len, a) -> _evar_0_1 len a __
       | Vword (s0, s1) -> (fun _ -> _evar_0_2 s0 s1)
       | Vundef t0 -> (fun _ -> _evar_0_3 t0))))

(** val on_arr_gvarP :
    (positive -> WArray.array -> 'a1 exec) -> 'a1 -> glob_decl list -> sem_t
    exec Fv.t -> gvar -> (positive -> WArray.array -> __ -> __ -> __ -> 'a2)
    -> 'a2 **)

let on_arr_gvarP f v gd s x h =
  rbindP (get_gvar gd s x) (fun v0 ->
    match v0 with
    | Varr (n, t0) -> f n t0
    | _ -> type_error) v (fun vx _ ->
    ssr_have __ (fun _ ->
      let _evar_0_ = fun _ -> assert false (* absurd case *) in
      let _evar_0_0 = fun _ -> assert false (* absurd case *) in
      let _evar_0_1 = fun len t0 -> h len t0 __ in
      let _evar_0_2 = fun _ _ -> assert false (* absurd case *) in
      let _evar_0_3 = fun _ -> assert false (* absurd case *) in
      (match vx with
       | Vbool b -> (fun _ -> _evar_0_ b)
       | Vint z -> (fun _ -> _evar_0_0 z)
       | Varr (len, a) -> _evar_0_1 len a __
       | Vword (s0, s1) -> (fun _ -> _evar_0_2 s0 s1)
       | Vundef t0 -> (fun _ -> _evar_0_3 t0))))

(** val sem_pexpr :
    ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a2 estate -> pexpr ->
    value exec **)

let rec sem_pexpr spp gd s = function
| Pconst z -> Ok (Vint z)
| Pbool b -> Ok (Vbool b)
| Parr_init n -> Ok (Varr (n, (WArray.empty n)))
| Pvar v -> get_gvar gd s.evm v
| Pget (aa, ws, x, e0) ->
  on_arr_var (get_gvar gd s.evm x) (fun n t0 ->
    match match sem_pexpr spp gd s e0 with
          | Ok x0 -> to_int x0
          | Error s0 -> Error s0 with
    | Ok x0 ->
      (match WArray.get n aa ws t0 x0 with
       | Ok x1 -> Ok (Vword (ws, x1))
       | Error s0 -> Error s0)
    | Error s0 -> Error s0)
| Psub (aa, ws, len, x, e0) ->
  on_arr_var (get_gvar gd s.evm x) (fun n t0 ->
    match match sem_pexpr spp gd s e0 with
          | Ok x0 -> to_int x0
          | Error s0 -> Error s0 with
    | Ok x0 ->
      (match WArray.get_sub n aa ws len t0 x0 with
       | Ok x1 -> Ok (Varr ((Z.to_pos (arr_size ws len)), x1))
       | Error s0 -> Error s0)
    | Error s0 -> Error s0)
| Pload (sz, x, e0) ->
  (match match get_var s.evm x.v_var with
         | Ok x0 -> to_word (coq_Uptr spp._pd) x0
         | Error s0 -> Error s0 with
   | Ok x0 ->
     (match match sem_pexpr spp gd s e0 with
            | Ok x1 -> to_word (coq_Uptr spp._pd) x1
            | Error s0 -> Error s0 with
      | Ok x1 ->
        (match CoreMem.read
                 (GRing.Zmodule.eqType
                   (GRing.ComRing.zmodType (word (coq_Uptr spp._pd))))
                 (coq_Pointer spp._pd) (Memory.coq_CM spp._pd) s.emem
                 (GRing.add
                   (GRing.ComRing.zmodType (word (coq_Uptr spp._pd))) x0 x1)
                 sz with
         | Ok x2 -> Ok (to_val (Coq_sword sz) x2)
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | Error s0 -> Error s0)
| Papp1 (o, e1) ->
  (match sem_pexpr spp gd s e1 with
   | Ok x -> sem_sop1 o x
   | Error s0 -> Error s0)
| Papp2 (o, e1, e2) ->
  (match sem_pexpr spp gd s e1 with
   | Ok x ->
     (match sem_pexpr spp gd s e2 with
      | Ok x0 -> sem_sop2 o x x0
      | Error s0 -> Error s0)
   | Error s0 -> Error s0)
| PappN (op, es) ->
  (match mapM (sem_pexpr spp gd s) es with
   | Ok x -> sem_opN spp._fcp op x
   | Error s0 -> Error s0)
| Pif (t0, e0, e1, e2) ->
  (match match sem_pexpr spp gd s e0 with
         | Ok x -> to_bool x
         | Error s0 -> Error s0 with
   | Ok x ->
     (match match sem_pexpr spp gd s e1 with
            | Ok x0 -> truncate_val t0 x0
            | Error s0 -> Error s0 with
      | Ok x0 ->
        (match match sem_pexpr spp gd s e2 with
               | Ok x1 -> truncate_val t0 x1
               | Error s0 -> Error s0 with
         | Ok x1 -> Ok (if x then x0 else x1)
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
   | Error s0 -> Error s0)

(** val sem_pexprs :
    ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a2 estate -> pexpr
    list -> (error, value list) result **)

let sem_pexprs spp gd s =
  mapM (sem_pexpr spp gd s)

(** val write_var :
    ('a1, 'a2) coq_SemPexprParams -> var_i -> value -> 'a2 estate -> 'a2
    estate exec **)

let write_var _ x v s =
  match set_var s.evm x.v_var v with
  | Ok x0 -> Ok { escs = s.escs; emem = s.emem; evm = x0 }
  | Error s0 -> Error s0

(** val write_vars :
    ('a1, 'a2) coq_SemPexprParams -> var_i list -> value list -> 'a2 estate
    -> (error, 'a2 estate) result **)

let write_vars spp xs vs s =
  fold2 ErrType (write_var spp) xs vs s

(** val write_none :
    ('a1, 'a2) coq_SemPexprParams -> 'a2 estate -> Equality.sort -> value ->
    'a2 estate exec **)

let write_none _ s ty v =
  on_vu (fun _ -> s) (if is_sbool ty then Ok s else type_error)
    (of_val (Obj.magic ty) v)

(** val write_lval :
    ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> lval -> value -> 'a2
    estate -> 'a2 estate exec **)

let write_lval spp gd l v s =
  match l with
  | Lnone (_, ty) -> write_none spp s (Obj.magic ty) v
  | Lvar x -> write_var spp x v s
  | Lmem (sz, x, e) ->
    (match match get_var s.evm x.v_var with
           | Ok x0 -> to_word (coq_Uptr spp._pd) x0
           | Error s0 -> Error s0 with
     | Ok x0 ->
       (match match sem_pexpr spp gd s e with
              | Ok x1 -> to_word (coq_Uptr spp._pd) x1
              | Error s0 -> Error s0 with
        | Ok x1 ->
          let p =
            GRing.add (GRing.ComRing.zmodType (word (coq_Uptr spp._pd))) x0 x1
          in
          (match to_word sz v with
           | Ok x2 ->
             (match CoreMem.write
                      (GRing.Zmodule.eqType
                        (GRing.ComRing.zmodType (word (coq_Uptr spp._pd))))
                      (coq_Pointer spp._pd) (Memory.coq_CM spp._pd) s.emem p
                      sz x2 with
              | Ok x3 -> Ok { escs = s.escs; emem = x3; evm = s.evm }
              | Error s0 -> Error s0)
           | Error s0 -> Error s0)
        | Error s0 -> Error s0)
     | Error s0 -> Error s0)
  | Laset (aa, ws, x, i) ->
    on_arr_var (get_var s.evm x.v_var) (fun n t0 ->
      match match sem_pexpr spp gd s i with
            | Ok x0 -> to_int x0
            | Error s0 -> Error s0 with
      | Ok x0 ->
        (match to_word ws v with
         | Ok x1 ->
           (match WArray.set n ws t0 aa x0 x1 with
            | Ok x2 -> write_var spp x (to_val (Coq_sarr n) (Obj.magic x2)) s
            | Error s0 -> Error s0)
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)
  | Lasub (aa, ws, len, x, i) ->
    on_arr_var (get_var s.evm x.v_var) (fun n t0 ->
      match match sem_pexpr spp gd s i with
            | Ok x0 -> to_int x0
            | Error s0 -> Error s0 with
      | Ok x0 ->
        (match to_arr (Z.to_pos (arr_size ws len)) v with
         | Ok x1 ->
           (match WArray.set_sub n aa ws len t0 x0 x1 with
            | Ok x2 -> write_var spp x (to_val (Coq_sarr n) (Obj.magic x2)) s
            | Error s0 -> Error s0)
         | Error s0 -> Error s0)
      | Error s0 -> Error s0)

(** val write_lvals :
    ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a2 estate -> lval
    list -> value list -> (error, 'a2 estate) result **)

let write_lvals spp gd s xs vs =
  fold2 ErrType (write_lval spp gd) xs vs s

(** val exec_sopn :
    ('a1, 'a2) coq_SemPexprParams -> 'a1 sopn -> values -> values exec **)

let exec_sopn spp o vs =
  let semi = sopn_sem spp._asmop o in
  (match app_sopn (get_instr_desc spp._asmop o).tin semi vs with
   | Ok x -> Ok (list_ltuple (get_instr_desc spp._asmop o).tout x)
   | Error s -> Error s)

(** val sem_sopn :
    ('a1, 'a2) coq_SemPexprParams -> glob_decl list -> 'a1 sopn -> 'a2 estate
    -> lval list -> pexpr list -> (error, 'a2 estate) result **)

let sem_sopn spp gd o m lvs args =
  match match sem_pexprs spp gd m args with
        | Ok x -> exec_sopn spp o x
        | Error s -> Error s with
  | Ok x -> write_lvals spp gd m lvs x
  | Error s -> Error s

(** val exec_getrandom :
    ('a1, 'a2) coq_SemPexprParams -> 'a2 syscall_state_t -> positive -> value
    list -> (error, 'a2 syscall_state_t * value list) result **)

let exec_getrandom spp scs len vs =
  match match vs with
        | [] -> type_error
        | v :: l -> (match l with
                     | [] -> to_arr len v
                     | _ :: _ -> type_error) with
  | Ok _ ->
    let sd = get_random spp._sc_sem scs (Zpos len) in
    (match WArray.fill len (snd sd) with
     | Ok x -> Ok ((fst sd), ((Varr (len, x)) :: []))
     | Error s -> Error s)
  | Error s -> Error s

(** val exec_syscall :
    ('a1, 'a2) coq_SemPexprParams -> 'a2 syscall_state_t -> Memory.mem ->
    BinNums.positive Syscall_t.syscall_t -> values -> (('a2
    syscall_state_t * Memory.mem) * values) exec **)

let exec_syscall spp scs m o vs =
  let Syscall_t.RandomBytes len = o in
  (match exec_getrandom spp scs len vs with
   | Ok x -> Ok (((fst x), m), (snd x))
   | Error s -> Error s)
