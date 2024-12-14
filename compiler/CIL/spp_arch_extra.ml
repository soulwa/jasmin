open Arch_decl
open Arch_extra
open Sem_pexpr_params
open Syscall

(** val spp_of_asm_e :
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> 'a8 syscall_sem ->
    (('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op, 'a8) coq_SemPexprParams **)

let spp_of_asm_e asm_e scs =
  { _pd = (arch_pd asm_e._asm._arch_decl); _asmop = (asm_opI asm_e); _fcp =
    asm_e._asm._arch_decl.ad_fcp; _sc_sem = scs }
