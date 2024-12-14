open Arch_decl
open Arch_extra
open Sem_pexpr_params
open Syscall

val spp_of_asm_e :
  ('a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) asm_extra -> 'a8 syscall_sem -> (('a1,
  'a2, 'a3, 'a4, 'a5, 'a6, 'a7) extended_op, 'a8) coq_SemPexprParams
