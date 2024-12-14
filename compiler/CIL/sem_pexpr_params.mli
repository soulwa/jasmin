open Flag_combination
open Sopn
open Syscall
open Wsize

type ('asm_op, 'syscall_state) coq_SemPexprParams = { _pd : coq_PointerData;
                                                      _asmop : 'asm_op asmOp;
                                                      _fcp : coq_FlagCombinationParams;
                                                      _sc_sem : 'syscall_state
                                                                syscall_sem }
