open BinNums
open Eqtype
open Expr
open Label
open Sopn
open Ssralg
open Type
open Utils0
open Var0
open Wsize

type 'asm_op linstr_r =
| Lopn of lval list * 'asm_op sopn * pexpr list
| Lsyscall of BinNums.positive Syscall_t.syscall_t
| Lcall of remote_label
| Lret
| Lalign
| Llabel of label
| Lgoto of remote_label
| Ligoto of pexpr
| LstoreLabel of Var.var * label
| Lcond of pexpr * label

type 'asm_op linstr = { li_ii : instr_info; li_i : 'asm_op linstr_r }

type 'asm_op lcmd = 'asm_op linstr list

type 'asm_op lfundef = { lfd_info : fun_info; lfd_align : wsize;
                         lfd_tyin : stype list; lfd_arg : var_i list;
                         lfd_body : 'asm_op lcmd; lfd_tyout : stype list;
                         lfd_res : var_i list; lfd_export : bool;
                         lfd_callee_saved : Var.var list;
                         lfd_total_stack : coq_Z }

type 'asm_op lprog = { lp_rip : Equality.sort; lp_rsp : Equality.sort;
                       lp_globs : GRing.ComRing.sort list;
                       lp_funcs : (funname * 'asm_op lfundef) list }
