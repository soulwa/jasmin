open BinNums
open Bool
open Datatypes
open Allocation
open Arch_decl
open Arch_extra
open Arch_params
open Array_copy
open Array_expansion
open Array_init
open Asm_gen
open Compiler_util
open Constant_prop
open Dead_calls
open Dead_code
open Eqtype
open Expr
open Flag_combination
open Global
open Inline
open Linear
open Linearization
open Lowering
open MakeReferenceArguments
open Merge_varmaps
open Propagate_inline
open Remove_globals
open Seq
open Sopn
open Ssralg
open Ssrbool
open Stack_alloc
open Tunneling
open Type
open Unrolling
open Utils0
open Var0
open Wsize

(** val pp_s : char list -> pp_error **)

let pp_s x =
  PPEstring x

(** val unroll :
    'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t -> bool) -> nat
    -> 'a1 uprog -> 'a1 uprog cexec **)

let unroll asmop fcp is_move_op =
  let postprocess = fun p ->
    let p0 =
      const_prop_prog fcp asmop
        (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
        progUnit p
    in
    dead_code_prog asmop is_move_op
      (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
      progUnit p0 false
  in
  let rec unroll0 n p =
    match n with
    | O ->
      Error
        (loop_iterator
          ('u'::('n'::('r'::('o'::('l'::('l'::('i'::('n'::('g'::[]))))))))))
    | S n' ->
      let (p', repeat) =
        unroll_prog asmop
          (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
          progUnit p
      in
      if repeat
      then (match postprocess p' with
            | Ok x -> unroll0 n' x
            | Error s -> Error s)
      else Ok p
  in unroll0

(** val unroll_loop :
    'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t -> bool) -> 'a1
    uprog -> (pp_error_loc, 'a1 uprog) result **)

let unroll_loop asmop fcp is_move_op =
  let postprocess = fun p ->
    let p0 =
      const_prop_prog fcp asmop
        (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
        progUnit p
    in
    dead_code_prog asmop is_move_op
      (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
      progUnit p0 false
  in
  (fun p ->
  match postprocess p with
  | Ok x -> unroll asmop fcp is_move_op Loop.nb x
  | Error s -> Error s)

type compiler_step =
| Typing
| ParamsExpansion
| ArrayCopy
| AddArrInit
| Inlining
| RemoveUnusedFunction
| Unrolling
| Splitting
| Renaming
| RemovePhiNodes
| DeadCode_Renaming
| RemoveArrInit
| RegArrayExpansion
| RemoveGlobal
| MakeRefArguments
| LowerInstruction
| PropagateInline
| StackAllocation
| RemoveReturn
| RegAllocation
| DeadCode_RegAllocation
| Linearization
| Tunneling
| Assembly

(** val compiler_step_list : compiler_step list **)

let compiler_step_list =
  Typing :: (ParamsExpansion :: (ArrayCopy :: (AddArrInit :: (Inlining :: (RemoveUnusedFunction :: (Unrolling :: (Splitting :: (Renaming :: (RemovePhiNodes :: (DeadCode_Renaming :: (RemoveArrInit :: (RegArrayExpansion :: (RemoveGlobal :: (MakeRefArguments :: (LowerInstruction :: (PropagateInline :: (StackAllocation :: (RemoveReturn :: (RegAllocation :: (DeadCode_RegAllocation :: (Linearization :: (Tunneling :: (Assembly :: [])))))))))))))))))))))))

(** val compiler_step_beq : compiler_step -> compiler_step -> bool **)

let compiler_step_beq x y =
  match x with
  | Typing -> (match y with
               | Typing -> true
               | _ -> false)
  | ParamsExpansion -> (match y with
                        | ParamsExpansion -> true
                        | _ -> false)
  | ArrayCopy -> (match y with
                  | ArrayCopy -> true
                  | _ -> false)
  | AddArrInit -> (match y with
                   | AddArrInit -> true
                   | _ -> false)
  | Inlining -> (match y with
                 | Inlining -> true
                 | _ -> false)
  | RemoveUnusedFunction ->
    (match y with
     | RemoveUnusedFunction -> true
     | _ -> false)
  | Unrolling -> (match y with
                  | Unrolling -> true
                  | _ -> false)
  | Splitting -> (match y with
                  | Splitting -> true
                  | _ -> false)
  | Renaming -> (match y with
                 | Renaming -> true
                 | _ -> false)
  | RemovePhiNodes -> (match y with
                       | RemovePhiNodes -> true
                       | _ -> false)
  | DeadCode_Renaming -> (match y with
                          | DeadCode_Renaming -> true
                          | _ -> false)
  | RemoveArrInit -> (match y with
                      | RemoveArrInit -> true
                      | _ -> false)
  | RegArrayExpansion -> (match y with
                          | RegArrayExpansion -> true
                          | _ -> false)
  | RemoveGlobal -> (match y with
                     | RemoveGlobal -> true
                     | _ -> false)
  | MakeRefArguments -> (match y with
                         | MakeRefArguments -> true
                         | _ -> false)
  | LowerInstruction -> (match y with
                         | LowerInstruction -> true
                         | _ -> false)
  | PropagateInline -> (match y with
                        | PropagateInline -> true
                        | _ -> false)
  | StackAllocation -> (match y with
                        | StackAllocation -> true
                        | _ -> false)
  | RemoveReturn -> (match y with
                     | RemoveReturn -> true
                     | _ -> false)
  | RegAllocation -> (match y with
                      | RegAllocation -> true
                      | _ -> false)
  | DeadCode_RegAllocation ->
    (match y with
     | DeadCode_RegAllocation -> true
     | _ -> false)
  | Linearization -> (match y with
                      | Linearization -> true
                      | _ -> false)
  | Tunneling -> (match y with
                  | Tunneling -> true
                  | _ -> false)
  | Assembly -> (match y with
                 | Assembly -> true
                 | _ -> false)

(** val compiler_step_eq_dec : compiler_step -> compiler_step -> bool **)

let compiler_step_eq_dec x y =
  let b = compiler_step_beq x y in if b then true else false

(** val compiler_step_eq_axiom : compiler_step Equality.axiom **)

let compiler_step_eq_axiom x y =
  iffP (compiler_step_beq x y)
    (if compiler_step_beq x y then ReflectT else ReflectF)

(** val compiler_step_eqMixin : compiler_step Equality.mixin_of **)

let compiler_step_eqMixin =
  { Equality.op = compiler_step_beq; Equality.mixin_of__1 =
    compiler_step_eq_axiom }

(** val compiler_step_eqType : Equality.coq_type **)

let compiler_step_eqType =
  Obj.magic compiler_step_eqMixin

type stack_alloc_oracles = { ao_globals : GRing.ComRing.sort list;
                             ao_global_alloc : ((Var.var * wsize) * coq_Z)
                                               list;
                             ao_stack_alloc : (funname -> stk_alloc_oracle_t) }

(** val ao_globals : stack_alloc_oracles -> GRing.ComRing.sort list **)

let ao_globals s =
  s.ao_globals

(** val ao_global_alloc :
    stack_alloc_oracles -> ((Var.var * wsize) * coq_Z) list **)

let ao_global_alloc s =
  s.ao_global_alloc

(** val ao_stack_alloc :
    stack_alloc_oracles -> funname -> stk_alloc_oracle_t **)

let ao_stack_alloc s =
  s.ao_stack_alloc

type ('asm_op, 'fresh_vars, 'lowering_options) compiler_params = { rename_fd : 
                                                                   (instr_info
                                                                   -> funname
                                                                   -> 'asm_op
                                                                   _ufundef
                                                                   -> 'asm_op
                                                                   _ufundef);
                                                                   expand_fd : 
                                                                   (funname
                                                                   -> 'asm_op
                                                                   _ufundef
                                                                   ->
                                                                   expand_info);
                                                                   split_live_ranges_fd : 
                                                                   (funname
                                                                   -> 'asm_op
                                                                   _ufundef
                                                                   -> 'asm_op
                                                                   _ufundef);
                                                                   renaming_fd : 
                                                                   (funname
                                                                   -> 'asm_op
                                                                   _ufundef
                                                                   -> 'asm_op
                                                                   _ufundef);
                                                                   remove_phi_nodes_fd : 
                                                                   (funname
                                                                   -> 'asm_op
                                                                   _ufundef
                                                                   -> 'asm_op
                                                                   _ufundef);
                                                                   lowering_vars : 
                                                                   'fresh_vars;
                                                                   inline_var : 
                                                                   (Var.var
                                                                   -> bool);
                                                                   is_var_in_memory : 
                                                                   (var_i ->
                                                                   bool);
                                                                   stack_register_symbol : 
                                                                   Equality.sort;
                                                                   global_static_data_symbol : 
                                                                   Equality.sort;
                                                                   stackalloc : 
                                                                   ('asm_op
                                                                   _uprog ->
                                                                   stack_alloc_oracles);
                                                                   removereturn : 
                                                                   ('asm_op
                                                                   _sprog ->
                                                                   funname ->
                                                                   bool list
                                                                   option);
                                                                   regalloc : 
                                                                   ('asm_op
                                                                   _sfun_decl
                                                                   list ->
                                                                   'asm_op
                                                                   _sfun_decl
                                                                   list);
                                                                   extra_free_registers : 
                                                                   (instr_info
                                                                   -> Var.var
                                                                   option);
                                                                   print_uprog : 
                                                                   (compiler_step
                                                                   -> 'asm_op
                                                                   _uprog ->
                                                                   'asm_op
                                                                   _uprog);
                                                                   print_sprog : 
                                                                   (compiler_step
                                                                   -> 'asm_op
                                                                   _sprog ->
                                                                   'asm_op
                                                                   _sprog);
                                                                   print_linear : 
                                                                   (compiler_step
                                                                   -> 'asm_op
                                                                   lprog ->
                                                                   'asm_op
                                                                   lprog);
                                                                   warning : 
                                                                   (instr_info
                                                                   ->
                                                                   warning_msg
                                                                   ->
                                                                   instr_info);
                                                                   lowering_opt : 
                                                                   'lowering_options;
                                                                   is_glob : 
                                                                   (Var.var
                                                                   -> bool);
                                                                   fresh_id : 
                                                                   (glob_decl
                                                                   list ->
                                                                   Var.var ->
                                                                   Equality.sort);
                                                                   fresh_reg : 
                                                                   (char list
                                                                   -> stype
                                                                   ->
                                                                   Equality.sort);
                                                                   fresh_reg_ptr : 
                                                                   (char list
                                                                   -> stype
                                                                   ->
                                                                   Equality.sort);
                                                                   fresh_counter : 
                                                                   Equality.sort;
                                                                   is_reg_ptr : 
                                                                   (Var.var
                                                                   -> bool);
                                                                   is_ptr : 
                                                                   (Var.var
                                                                   -> bool);
                                                                   is_reg_array : 
                                                                   (Var.var
                                                                   -> bool);
                                                                   is_regx : 
                                                                   (Var.var
                                                                   -> bool) }

(** val rename_fd :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> instr_info -> funname ->
    'a1 _ufundef -> 'a1 _ufundef **)

let rename_fd _ c =
  c.rename_fd

(** val expand_fd :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef
    -> expand_info **)

let expand_fd _ c =
  c.expand_fd

(** val split_live_ranges_fd :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef
    -> 'a1 _ufundef **)

let split_live_ranges_fd _ c =
  c.split_live_ranges_fd

(** val renaming_fd :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef
    -> 'a1 _ufundef **)

let renaming_fd _ c =
  c.renaming_fd

(** val remove_phi_nodes_fd :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef
    -> 'a1 _ufundef **)

let remove_phi_nodes_fd _ c =
  c.remove_phi_nodes_fd

(** val lowering_vars :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a2 **)

let lowering_vars _ c =
  c.lowering_vars

(** val inline_var :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool **)

let inline_var _ c =
  c.inline_var

(** val is_var_in_memory :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> var_i -> bool **)

let is_var_in_memory _ c =
  c.is_var_in_memory

(** val stack_register_symbol :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Equality.sort **)

let stack_register_symbol _ c =
  c.stack_register_symbol

(** val global_static_data_symbol :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Equality.sort **)

let global_static_data_symbol _ c =
  c.global_static_data_symbol

(** val stackalloc :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a1 _uprog ->
    stack_alloc_oracles **)

let stackalloc _ c =
  c.stackalloc

(** val removereturn :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a1 _sprog -> funname ->
    bool list option **)

let removereturn _ c =
  c.removereturn

(** val regalloc :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a1 _sfun_decl list ->
    'a1 _sfun_decl list **)

let regalloc _ c =
  c.regalloc

(** val extra_free_registers :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> instr_info -> Var.var
    option **)

let extra_free_registers _ c =
  c.extra_free_registers

(** val print_uprog :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> compiler_step -> 'a1
    _uprog -> 'a1 _uprog **)

let print_uprog _ c =
  c.print_uprog

(** val print_sprog :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> compiler_step -> 'a1
    _sprog -> 'a1 _sprog **)

let print_sprog _ c =
  c.print_sprog

(** val print_linear :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> compiler_step -> 'a1
    lprog -> 'a1 lprog **)

let print_linear _ c =
  c.print_linear

(** val warning :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> instr_info -> warning_msg
    -> instr_info **)

let warning _ c =
  c.warning

(** val lowering_opt : 'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a3 **)

let lowering_opt _ c =
  c.lowering_opt

(** val is_glob :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool **)

let is_glob _ c =
  c.is_glob

(** val fresh_id :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> glob_decl list -> Var.var
    -> Equality.sort **)

let fresh_id _ c =
  c.fresh_id

(** val fresh_reg :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> char list -> stype ->
    Equality.sort **)

let fresh_reg _ c =
  c.fresh_reg

(** val fresh_reg_ptr :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> char list -> stype ->
    Equality.sort **)

let fresh_reg_ptr _ c =
  c.fresh_reg_ptr

(** val fresh_counter :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Equality.sort **)

let fresh_counter _ c =
  c.fresh_counter

(** val is_reg_ptr :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool **)

let is_reg_ptr _ c =
  c.is_reg_ptr

(** val is_ptr :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool **)

let is_ptr _ c =
  c.is_ptr

(** val is_reg_array :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool **)

let is_reg_array _ c =
  c.is_reg_array

(** val is_regx :
    'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool **)

let is_regx _ c =
  c.is_regx

(** val split_live_ranges_prog :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> ('a1, 'a2, 'a3,
    'a4, 'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op _uprog **)

let split_live_ranges_prog asm_e cparams p =
  Obj.magic map_prog_name (asm_opI asm_e) unit_eqMixin progUnit
    cparams.split_live_ranges_fd p

(** val renaming_prog :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> ('a1, 'a2, 'a3,
    'a4, 'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op _uprog **)

let renaming_prog asm_e cparams p =
  Obj.magic map_prog_name (asm_opI asm_e) unit_eqMixin progUnit
    cparams.renaming_fd p

(** val remove_phi_nodes_prog :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> ('a1, 'a2, 'a3,
    'a4, 'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
    'a7) extended_op _uprog **)

let remove_phi_nodes_prog asm_e cparams p =
  Obj.magic map_prog_name (asm_opI asm_e) unit_eqMixin progUnit
    cparams.remove_phi_nodes_fd p

(** val var_tmp :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8, 'a9) architecture_params -> Var.var **)

let var_tmp asm_e aparams =
  { Var.vtype = (Coq_sword (coq_Uptr (arch_pd asm_e._asm._arch_decl)));
    Var.vname = aparams.ap_lip.lip_tmp }

(** val check_removereturn :
    funname list -> (funname -> bool list option) -> (pp_error_loc, unit)
    result **)

let check_removereturn entries remove_return =
  if eq_op (seq_eqType (seq_eqType bool_eqType))
       (Obj.magic pmap remove_return entries) (Obj.magic [])
  then Ok ()
  else Error
         (pp_internal_error_s
           ('r'::('e'::('m'::('o'::('v'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::[])))))))))))))
           ('S'::('i'::('g'::('n'::('a'::('t'::('u'::('r'::('e'::(' '::('o'::('f'::(' '::('s'::('o'::('m'::('e'::(' '::('e'::('x'::('p'::('o'::('r'::('t'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('a'::('r'::('e'::(' '::('m'::('o'::('d'::('i'::('f'::('i'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))

(** val allNone : 'a1 option list -> bool **)

let allNone m =
  all (fun a -> match a with
                | Some _ -> false
                | None -> true) m

(** val check_no_ptr :
    funname list -> (funname -> stk_alloc_oracle_t) -> unit cexec **)

let check_no_ptr entries ao =
  allM (fun fn ->
    if allNone (ao fn).sao_params
    then if allNone (ao fn).sao_return
         then Ok ()
         else Error
                (pp_at_fn fn
                  (Stack_alloc.E.stk_error_no_var
                    ('e'::('x'::('p'::('o'::('r'::('t'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('d'::('o'::('n'::('\226'::('\128'::('\153'::('t'::(' '::('s'::('u'::('p'::('p'::('o'::('r'::('t'::(' '::('\226'::('\128'::('\156'::('p'::('t'::('r'::('\226'::('\128'::('\157'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('v'::('a'::('l'::('u'::('e'::('s'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    else let s =
           pp_at_fn fn
             (Stack_alloc.E.stk_error_no_var
               ('e'::('x'::('p'::('o'::('r'::('t'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::('s'::(' '::('d'::('o'::('n'::('\226'::('\128'::('\153'::('t'::(' '::('s'::('u'::('p'::('p'::('o'::('r'::('t'::(' '::('\226'::('\128'::('\156'::('p'::('t'::('r'::('\226'::('\128'::('\157'::(' '::('a'::('r'::('g'::('u'::('m'::('e'::('n'::('t'::('s'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))
         in
         Error s) entries

(** val compiler_first_part :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8, 'a9) architecture_params -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> funname list ->
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op prog -> ('a1, 'a2, 'a3,
    'a4, 'a5, 'a6, 'a7) extended_op uprog cexec **)

let compiler_first_part asm_e aparams cparams to_keep p =
  match array_copy_prog (asm_opI asm_e) cparams.fresh_counter
          (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x -> x))
          progUnit p with
  | Ok x ->
    let p0 = cparams.print_uprog ArrayCopy (Obj.magic x) in
    let p1 =
      add_init_prog (asm_opI asm_e) cparams.is_ptr
        (Equality.clone unit_eqType (Obj.magic unit_eqMixin) (fun x0 -> x0))
        progUnit (Obj.magic p0)
    in
    let p2 = cparams.print_uprog AddArrInit (Obj.magic p1) in
    (match inline_prog_err (asm_opI asm_e) cparams.inline_var
             (Obj.magic cparams.rename_fd) (Obj.magic p2) with
     | Ok x0 ->
       let p3 = cparams.print_uprog Inlining (Obj.magic x0) in
       (match dead_calls_err_seq (asm_opI asm_e)
                (Equality.clone unit_eqType (Obj.magic unit_eqMixin)
                  (fun x1 -> x1)) progUnit to_keep (Obj.magic p3) with
        | Ok x1 ->
          let p4 = cparams.print_uprog RemoveUnusedFunction (Obj.magic x1) in
          (match unroll_loop (asm_opI asm_e) asm_e._asm._arch_decl.ad_fcp
                   aparams.ap_is_move_op (Obj.magic p4) with
           | Ok x2 ->
             let p5 = cparams.print_uprog Unrolling (Obj.magic x2) in
             let pv = split_live_ranges_prog asm_e cparams p5 in
             let pv0 = cparams.print_uprog Splitting pv in
             let pv1 = renaming_prog asm_e cparams pv0 in
             let pv2 = cparams.print_uprog Renaming pv1 in
             let pv3 = remove_phi_nodes_prog asm_e cparams pv2 in
             let pv4 = cparams.print_uprog RemovePhiNodes pv3 in
             (match check_uprog (asm_opI asm_e) (Obj.magic p5).p_extra
                      (Obj.magic p5).p_funcs (Obj.magic pv4).p_extra
                      (Obj.magic pv4).p_funcs with
              | Ok _ ->
                (match dead_code_prog (asm_opI asm_e) aparams.ap_is_move_op
                         (Equality.clone unit_eqType (Obj.magic unit_eqMixin)
                           (fun x3 -> x3)) progUnit (Obj.magic pv4) false with
                 | Ok x3 ->
                   let pv5 =
                     cparams.print_uprog DeadCode_Renaming (Obj.magic x3)
                   in
                   let pr =
                     remove_init_prog (asm_opI asm_e) cparams.is_reg_array
                       (Equality.clone unit_eqType (Obj.magic unit_eqMixin)
                         (fun x4 -> x4)) progUnit (Obj.magic pv5)
                   in
                   let pr0 = cparams.print_uprog RemoveArrInit (Obj.magic pr)
                   in
                   (match expand_prog (asm_opI asm_e)
                            (Obj.magic cparams.expand_fd) (Obj.magic pr0) with
                    | Ok x4 ->
                      let pe =
                        cparams.print_uprog RegArrayExpansion (Obj.magic x4)
                      in
                      (match remove_glob_prog (asm_opI asm_e) cparams.is_glob
                               cparams.fresh_id (Obj.magic pe) with
                       | Ok x5 ->
                         let pg =
                           cparams.print_uprog RemoveGlobal (Obj.magic x5)
                         in
                         (match makereference_prog (asm_opI asm_e)
                                  cparams.is_reg_ptr
                                  (Obj.magic cparams.fresh_reg_ptr)
                                  (Obj.magic pg) with
                          | Ok x6 ->
                            let pa =
                              cparams.print_uprog MakeRefArguments
                                (Obj.magic x6)
                            in
                            if aparams.ap_lop.lop_fvars_correct
                                 cparams.lowering_vars
                                 (Obj.magic unit_eqMixin) progUnit
                                 (Obj.magic pa).p_funcs
                            then let pl =
                                   lower_prog (asm_opI asm_e)
                                     aparams.ap_lop.lop_lower_i
                                     cparams.lowering_opt cparams.warning
                                     cparams.lowering_vars
                                     (Equality.clone unit_eqType
                                       (Obj.magic unit_eqMixin) (fun x7 ->
                                       x7)) progUnit cparams.is_var_in_memory
                                     (Obj.magic pa)
                                 in
                                 let pl0 =
                                   cparams.print_uprog LowerInstruction
                                     (Obj.magic pl)
                                 in
                                 (match pi_prog (asm_opI asm_e)
                                          asm_e._asm._arch_decl.ad_fcp
                                          (Equality.clone unit_eqType
                                            (Obj.magic unit_eqMixin)
                                            (fun x7 -> x7)) progUnit
                                          (Obj.magic pl0) with
                                  | Ok x7 ->
                                    let pp =
                                      cparams.print_uprog PropagateInline
                                        (Obj.magic x7)
                                    in
                                    Ok (Obj.magic pp)
                                  | Error s -> Error s)
                            else let s =
                                   pp_internal_error_s
                                     ('l'::('o'::('w'::('e'::('r'::('i'::('n'::('g'::[]))))))))
                                     ('l'::('o'::('w'::('e'::('r'::('i'::('n'::('g'::(' '::('c'::('h'::('e'::('c'::('k'::(' '::('f'::('a'::('i'::('l'::('s'::[]))))))))))))))))))))
                                 in
                                 Error s
                          | Error s -> Error s)
                       | Error s -> Error s)
                    | Error s -> Error s)
                 | Error s -> Error s)
              | Error s -> Error s)
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val compiler_third_part :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8, 'a9) architecture_params -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> funname list ->
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog -> ('a1, 'a2, 'a3,
    'a4, 'a5, 'a6, 'a7) extended_op sprog cexec **)

let compiler_third_part asm_e aparams cparams entries ps =
  let rminfo = cparams.removereturn (Obj.magic ps) in
  (match check_removereturn entries rminfo with
   | Ok _ ->
     (match dead_code_prog_tokeep (asm_opI asm_e) aparams.ap_is_move_op false
              rminfo
              (Equality.clone sfe_eqType (Obj.magic sfe_eqMixin) (fun x -> x))
              (progStack (arch_pd asm_e._asm._arch_decl)) ps with
      | Ok x ->
        let pr = cparams.print_sprog RemoveReturn (Obj.magic x) in
        let pa = { p_funcs = (cparams.regalloc pr.p_funcs); p_globs =
          pr.p_globs; p_extra = pr.p_extra }
        in
        let pa0 = cparams.print_sprog RegAllocation pa in
        (match check_sprog (asm_opI asm_e) (arch_pd asm_e._asm._arch_decl)
                 (Obj.magic pr).p_extra (Obj.magic pr).p_funcs
                 (Obj.magic pa0).p_extra (Obj.magic pa0).p_funcs with
         | Ok _ ->
           (match dead_code_prog (asm_opI asm_e) aparams.ap_is_move_op
                    (Equality.clone sfe_eqType (Obj.magic sfe_eqMixin)
                      (fun x0 -> x0))
                    (progStack (arch_pd asm_e._asm._arch_decl))
                    (Obj.magic pa0) true with
            | Ok x0 ->
              let pd =
                cparams.print_sprog DeadCode_RegAllocation (Obj.magic x0)
              in
              Ok (Obj.magic pd)
            | Error s -> Error s)
         | Error s -> Error s)
      | Error s -> Error s)
   | Error s -> Error s)

(** val compiler_front_end :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7, 'a8, 'a9) architecture_params -> (('a1, 'a2, 'a3, 'a4,
    'a5, 'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> funname list ->
    funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op prog ->
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog cexec **)

let compiler_front_end asm_e aparams cparams entries subroutines p =
  match compiler_first_part asm_e aparams cparams (cat entries subroutines) p with
  | Ok x ->
    let ao = cparams.stackalloc (Obj.magic x) in
    (match check_no_ptr entries ao.ao_stack_alloc with
     | Ok _ ->
       (match alloc_prog (arch_pd asm_e._asm._arch_decl) (asm_opI asm_e) true
                (aparams.ap_sap cparams.is_regx) cparams.fresh_reg
                cparams.global_static_data_symbol
                cparams.stack_register_symbol ao.ao_globals
                ao.ao_global_alloc ao.ao_stack_alloc (Obj.magic x) with
        | Ok x0 ->
          let ps = cparams.print_sprog StackAllocation x0 in
          (match compiler_third_part asm_e aparams cparams entries
                   (Obj.magic ps) with
           | Ok x1 -> Ok x1
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val check_export :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> funname list -> ('a1,
    'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog -> unit cexec **)

let check_export _ entries p =
  allM (fun fn ->
    match get_fundef p.p_funcs fn with
    | Some fd ->
      if eq_op return_address_location_eqType
           (Obj.magic (Obj.magic fd).f_extra.sf_return_address)
           (Obj.magic RAnone)
      then Ok ()
      else Error
             (pp_at_fn fn
               (Merge_varmaps.E.gen_error true None
                 (pp_s
                   ('e'::('x'::('p'::('o'::('r'::('t'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('s'::(' '::('a'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('a'::('d'::('d'::('r'::('e'::('s'::('s'::[])))))))))))))))))))))))))))))))))))))))))))
    | None ->
      Error
        (pp_at_fn fn
          (Merge_varmaps.E.gen_error true None
            (pp_s
              ('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('e'::('x'::('p'::('o'::('r'::('t'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))
    entries

(** val compiler_back_end :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5) calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9)
    architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
    'a8, 'a9) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op lprog) result **)

let compiler_back_end asm_e call_conv aparams cparams entries pd =
  match check_export asm_e entries pd with
  | Ok _ ->
    (match check (arch_pd asm_e._asm._arch_decl) (asm_opI asm_e)
             (ovm_i asm_e._asm._arch_decl call_conv) pd
             cparams.extra_free_registers (var_tmp asm_e aparams) with
     | Ok _ ->
       (match linear_prog (arch_pd asm_e._asm._arch_decl) (asm_opI asm_e)
                aparams.ap_lip pd cparams.extra_free_registers with
        | Ok x ->
          let pl = cparams.print_linear Linearization x in
          (match tunnel_program (asm_opI asm_e) pl with
           | Ok x0 -> let pl0 = cparams.print_linear Tunneling x0 in Ok pl0
           | Error s -> Error s)
        | Error s -> Error s)
     | Error s -> Error s)
  | Error s -> Error s

(** val compiler_back_end_to_asm :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5) calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9)
    architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
    'a8, 'a9) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6, 'a7) extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6) asm_prog) result **)

let compiler_back_end_to_asm asm_e call_conv aparams cparams entries p =
  match compiler_back_end asm_e call_conv aparams cparams entries p with
  | Ok x -> assemble_prog asm_e call_conv aparams.ap_agp x
  | Error s -> Error s

(** val compile_prog_to_asm :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4,
    'a5) calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9)
    architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
    'a8, 'a9) compiler_params -> funname list -> funname list -> ('a1, 'a2,
    'a3, 'a4, 'a5, 'a6, 'a7) extended_op prog -> ('a1, 'a2, 'a3, 'a4, 'a5,
    'a6) asm_prog cexec **)

let compile_prog_to_asm asm_e call_conv aparams cparams entries subroutines p =
  match compiler_front_end asm_e aparams cparams entries subroutines p with
  | Ok x -> compiler_back_end_to_asm asm_e call_conv aparams cparams entries x
  | Error s -> Error s
