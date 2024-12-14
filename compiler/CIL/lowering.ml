open Compiler_util
open Eqtype
open Expr
open Seq
open Sopn
open Utils0
open Var0

(** val disj_fvars : SvExtra.Sv.t -> SvExtra.Sv.t -> bool **)

let disj_fvars fvars x =
  SvExtra.disjoint x fvars

(** val fvars_correct :
    'a1 asmOp -> Equality.coq_type -> progT -> Equality.sort list ->
    SvExtra.Sv.t -> 'a1 fun_decl list -> bool **)

let fvars_correct asmop eft pT all_fresh_vars fvars fds =
  (&&) (disj_fvars fvars (vars_p asmop eft pT fds))
    (uniq Ident.Ident.ident all_fresh_vars)

(** val is_lval_in_memory : (var_i -> bool) -> lval -> bool **)

let is_lval_in_memory is_var_in_memory = function
| Lnone (_, _) -> false
| Lvar v -> is_var_in_memory v
| Lmem (_, _, _) -> true
| Laset (_, _, v, _) -> is_var_in_memory v
| Lasub (_, _, _, v, _) -> is_var_in_memory v

(** val lower_cmd :
    'a1 asmOp -> ('a2 -> (instr_info -> warning_msg -> instr_info) -> 'a3 ->
    (var_i -> bool) -> 'a1 instr -> 'a1 instr list) -> 'a2 -> (instr_info ->
    warning_msg -> instr_info) -> 'a3 -> (var_i -> bool) -> 'a1 instr list ->
    'a1 instr list **)

let lower_cmd _ lower_i0 options warning fv is_var_in_memory c =
  conc_map (lower_i0 options warning fv is_var_in_memory) c

(** val lower_fd :
    'a1 asmOp -> ('a2 -> (instr_info -> warning_msg -> instr_info) -> 'a3 ->
    (var_i -> bool) -> 'a1 instr -> 'a1 instr list) -> 'a2 -> (instr_info ->
    warning_msg -> instr_info) -> 'a3 -> Equality.coq_type -> progT -> (var_i
    -> bool) -> 'a1 fundef -> 'a1 fundef **)

let lower_fd asmop lower_i0 options warning fv _ _ is_var_in_memory fd =
  { f_info = fd.f_info; f_tyin = fd.f_tyin; f_params = fd.f_params; f_body =
    (lower_cmd asmop lower_i0 options warning fv is_var_in_memory fd.f_body);
    f_tyout = fd.f_tyout; f_res = fd.f_res; f_extra = fd.f_extra }

(** val lower_prog :
    'a1 asmOp -> ('a2 -> (instr_info -> warning_msg -> instr_info) -> 'a3 ->
    (var_i -> bool) -> 'a1 instr -> 'a1 instr list) -> 'a2 -> (instr_info ->
    warning_msg -> instr_info) -> 'a3 -> Equality.coq_type -> progT -> (var_i
    -> bool) -> 'a1 prog -> 'a1 prog **)

let lower_prog asmop lower_i0 options warning fv eft pT is_var_in_memory p =
  map_prog asmop eft pT
    (lower_fd asmop lower_i0 options warning fv eft pT is_var_in_memory) p
