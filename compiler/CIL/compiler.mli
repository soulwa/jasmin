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

val pp_s : char list -> pp_error

val unroll :
  'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t -> bool) -> nat ->
  'a1 uprog -> 'a1 uprog cexec

val unroll_loop :
  'a1 asmOp -> coq_FlagCombinationParams -> ('a1 asm_op_t -> bool) -> 'a1
  uprog -> (pp_error_loc, 'a1 uprog) result

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

val compiler_step_list : compiler_step list

val compiler_step_beq : compiler_step -> compiler_step -> bool

val compiler_step_eq_dec : compiler_step -> compiler_step -> bool

val compiler_step_eq_axiom : compiler_step Equality.axiom

val compiler_step_eqMixin : compiler_step Equality.mixin_of

val compiler_step_eqType : Equality.coq_type

type stack_alloc_oracles = { ao_globals : GRing.ComRing.sort list;
                             ao_global_alloc : ((Var.var * wsize) * coq_Z)
                                               list;
                             ao_stack_alloc : (funname -> stk_alloc_oracle_t) }

val ao_globals : stack_alloc_oracles -> GRing.ComRing.sort list

val ao_global_alloc : stack_alloc_oracles -> ((Var.var * wsize) * coq_Z) list

val ao_stack_alloc : stack_alloc_oracles -> funname -> stk_alloc_oracle_t

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

val rename_fd :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> instr_info -> funname ->
  'a1 _ufundef -> 'a1 _ufundef

val expand_fd :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef ->
  expand_info

val split_live_ranges_fd :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef ->
  'a1 _ufundef

val renaming_fd :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef ->
  'a1 _ufundef

val remove_phi_nodes_fd :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> funname -> 'a1 _ufundef ->
  'a1 _ufundef

val lowering_vars : 'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a2

val inline_var :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool

val is_var_in_memory :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> var_i -> bool

val stack_register_symbol :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Equality.sort

val global_static_data_symbol :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Equality.sort

val stackalloc :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a1 _uprog ->
  stack_alloc_oracles

val removereturn :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a1 _sprog -> funname ->
  bool list option

val regalloc :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a1 _sfun_decl list -> 'a1
  _sfun_decl list

val extra_free_registers :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> instr_info -> Var.var option

val print_uprog :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> compiler_step -> 'a1 _uprog
  -> 'a1 _uprog

val print_sprog :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> compiler_step -> 'a1 _sprog
  -> 'a1 _sprog

val print_linear :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> compiler_step -> 'a1 lprog
  -> 'a1 lprog

val warning :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> instr_info -> warning_msg
  -> instr_info

val lowering_opt : 'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> 'a3

val is_glob : 'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool

val fresh_id :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> glob_decl list -> Var.var
  -> Equality.sort

val fresh_reg :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> char list -> stype ->
  Equality.sort

val fresh_reg_ptr :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> char list -> stype ->
  Equality.sort

val fresh_counter :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Equality.sort

val is_reg_ptr :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool

val is_ptr : 'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool

val is_reg_array :
  'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool

val is_regx : 'a1 asmOp -> ('a1, 'a2, 'a3) compiler_params -> Var.var -> bool

val split_live_ranges_prog :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> ('a1, 'a2, 'a3, 'a4,
  'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op _uprog

val renaming_prog :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> ('a1, 'a2, 'a3, 'a4,
  'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op _uprog

val remove_phi_nodes_prog :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> (('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op, 'a8, 'a9) compiler_params -> ('a1, 'a2, 'a3, 'a4,
  'a5, 'a6, 'a7) extended_op _uprog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7)
  extended_op _uprog

val var_tmp :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8, 'a9) architecture_params -> Var.var

val check_removereturn :
  funname list -> (funname -> bool list option) -> (pp_error_loc, unit) result

val allNone : 'a1 option list -> bool

val check_no_ptr :
  funname list -> (funname -> stk_alloc_oracle_t) -> unit cexec

val compiler_first_part :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8, 'a9) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
  'a7) extended_op, 'a8, 'a9) compiler_params -> funname list -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op prog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
  'a7) extended_op uprog cexec

val compiler_third_part :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8, 'a9) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
  'a7) extended_op, 'a8, 'a9) compiler_params -> funname list -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7) extended_op sprog cexec

val compiler_front_end :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a6, 'a7, 'a8, 'a9) architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
  'a7) extended_op, 'a8, 'a9) compiler_params -> funname list -> funname list
  -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op prog -> ('a1, 'a2, 'a3,
  'a4, 'a5, 'a6, 'a7) extended_op sprog cexec

val check_export :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> funname list -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op sprog -> unit cexec

val compiler_back_end :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9)
  architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
  'a8, 'a9) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
  'a7) extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
  'a7) extended_op lprog) result

val compiler_back_end_to_asm :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9)
  architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
  'a8, 'a9) compiler_params -> funname list -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6,
  'a7) extended_op sprog -> (pp_error_loc, ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_prog) result

val compile_prog_to_asm :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> ('a1, 'a2, 'a3, 'a4, 'a5)
  calling_convention -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9)
  architecture_params -> (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op,
  'a8, 'a9) compiler_params -> funname list -> funname list -> ('a1, 'a2,
  'a3, 'a4, 'a5, 'a6, 'a7) extended_op prog -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a6)
  asm_prog cexec
