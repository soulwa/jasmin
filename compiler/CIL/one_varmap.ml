open Expr
open Sopn
open Type
open Var0
open Wsize

type ovm_syscall_sig_t = { scs_vin : Var.var list; scs_vout : Var.var list }

type one_varmap_info = { syscall_sig : (BinNums.positive Syscall_t.syscall_t
                                       -> ovm_syscall_sig_t);
                         all_vars : SvExtra.Sv.t;
                         callee_saved : SvExtra.Sv.t; vflags : SvExtra.Sv.t }

(** val syscall_kill : one_varmap_info -> SvExtra.Sv.t **)

let syscall_kill ovm_i =
  SvExtra.Sv.diff ovm_i.all_vars ovm_i.callee_saved

(** val magic_variables :
    coq_PointerData -> 'a1 asmOp -> 'a1 sprog -> SvExtra.Sv.t **)

let magic_variables pd _ p =
  let vgd = { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
    (Obj.magic p).p_extra.sp_rip }
  in
  let vrsp = { Var.vtype = (Coq_sword (coq_Uptr pd)); Var.vname =
    (Obj.magic p).p_extra.sp_rsp }
  in
  SvExtra.Sv.add (Obj.magic vgd) (SvExtra.Sv.singleton (Obj.magic vrsp))

(** val savedstackreg : saved_stack -> SvExtra.Sv.t **)

let savedstackreg = function
| SavedStackReg r -> SvExtra.Sv.singleton (Obj.magic r)
| _ -> SvExtra.Sv.empty

(** val saved_stack_vm :
    'a1 asmOp -> ('a1, stk_fun_extra) _fundef -> SvExtra.Sv.t **)

let saved_stack_vm _ fd =
  savedstackreg fd.f_extra.sf_save_stack

(** val ra_vm :
    one_varmap_info -> stk_fun_extra -> Var.var -> SvExtra.Sv.t **)

let ra_vm ovm_i e x =
  match e.sf_return_address with
  | RAnone -> SvExtra.Sv.add (Obj.magic x) ovm_i.vflags
  | RAreg ra -> SvExtra.Sv.singleton (Obj.magic ra)
  | RAstack _ -> SvExtra.Sv.empty

(** val ra_undef :
    'a1 asmOp -> one_varmap_info -> ('a1, stk_fun_extra) _fundef -> Var.var
    -> SvExtra.Sv.t **)

let ra_undef asmop ovm_i fd x =
  SvExtra.Sv.union (ra_vm ovm_i fd.f_extra x) (saved_stack_vm asmop fd)
