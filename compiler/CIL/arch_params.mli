open Arch_extra
open Asm_gen
open Compiler_util
open Eqtype
open Expr
open Linearization
open Sopn
open Stack_alloc
open Var0

type ('asm_op, 'fresh_vars, 'lowering_options) lowering_params = { lop_lower_i : 
                                                                   ('lowering_options
                                                                   ->
                                                                   (instr_info
                                                                   ->
                                                                   warning_msg
                                                                   ->
                                                                   instr_info)
                                                                   ->
                                                                   'fresh_vars
                                                                   -> (var_i
                                                                   -> bool)
                                                                   -> 'asm_op
                                                                   instr ->
                                                                   'asm_op
                                                                   instr
                                                                   list);
                                                                   lop_fvars_correct : 
                                                                   ('fresh_vars
                                                                   ->
                                                                   Equality.coq_type
                                                                   -> progT
                                                                   -> 'asm_op
                                                                   fun_decl
                                                                   list ->
                                                                   bool) }

type ('reg, 'regx, 'xreg, 'rflag, 'cond, 'asm_op, 'extra_op, 'fresh_vars,
      'lowering_options) architecture_params = { ap_sap : ((Var.var -> bool)
                                                          -> ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond, 'asm_op,
                                                          'extra_op)
                                                          extended_op
                                                          stack_alloc_params);
                                                 ap_lip : ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond, 'asm_op,
                                                          'extra_op)
                                                          extended_op
                                                          linearization_params;
                                                 ap_lop : (('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond, 'asm_op,
                                                          'extra_op)
                                                          extended_op,
                                                          'fresh_vars,
                                                          'lowering_options)
                                                          lowering_params;
                                                 ap_agp : ('reg, 'regx,
                                                          'xreg, 'rflag,
                                                          'cond, 'asm_op,
                                                          'extra_op)
                                                          asm_gen_params;
                                                 ap_is_move_op : (('reg,
                                                                 'regx,
                                                                 'xreg,
                                                                 'rflag,
                                                                 'cond,
                                                                 'asm_op,
                                                                 'extra_op)
                                                                 extended_op
                                                                 asm_op_t ->
                                                                 bool) }
