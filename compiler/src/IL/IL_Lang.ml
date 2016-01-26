(* * Intermediate language IL *)

(* ** Imports and abbreviations *)
open Core_kernel.Std
open Arith

module F = Format
module P = ParserUtil
module L = ParserUtil.Lexing

(* ** Compile time expressions
 * ------------------------------------------------------------------------ *)
(* *** Summary
Programs in our language are parameterized by parameter variables.
For a mapping from parameter variables to u64 values, the program
can be partially evaluated and the following constructs can be eliminated:
- for loops 'for i in lb..ub { ... }' can be unfolded
- if-then-else 'if ce { i1 } else { i2 }' can be replaced by i1/i2 after
  evaluating 'ce'
- indexes: array accesses 'r[e]' indexed with expressions 'e' over parameters
  can be indexed by u64 values
*)
(* *** Code *)

type name = string with sexp, compare

type pop_u64 =
  | Pplus
  | Pmult
  | Pminus
  with sexp, compare

type patom =
  | Pparam of name (* global parameter (constant) *)
  | Pvar   of name (* function local variable *) 
  with sexp, compare

type 'a pexpr_g =
  | Patom of 'a
  | Pbinop of pop_u64 * 'a pexpr_g * 'a pexpr_g
  | Pconst of u64
  with sexp, compare

(* dimension expression in types *)
type dexpr = name pexpr_g with sexp, compare

(* parameter expression used in indexes and if-condition *)
type pexpr = patom pexpr_g with sexp, compare

type pop_bool =
  | Peq
  | Pineq
  | Pless
  | Pleq
  | Pgreater
  | Pgeq
  with sexp, compare

type pcond =
  | Ptrue
  | Pnot  of pcond
  | Pand  of pcond * pcond
  | Pcond of pop_bool * pexpr * pexpr
  with sexp, compare

(* ** Pseudo-registers, sources, and destinations
 * ------------------------------------------------------------------------ *)
(* *** Summary
We define:
- pseudo-registers that hold values and addresses
- sources (r-values)
- destinations (l-values)
*)
(* *** Code *)

type ty =
  | Bool
  | U64
  | Arr of dexpr
  with sexp, compare

type dest = {
  d_name : name;         (* r[i] has name r and (optional) index i, *)
  d_oidx : pexpr option; (* i denotes index for array get           *)
  d_loc  : L.loc;        (* location where pseudo-register occurs   *)
} with sexp, compare

type src =
  | Imm of pexpr (* Simm(i): immediate value i            *)
  | Src of dest  (* Sreg(d): where d destination register *)
  with sexp, compare

(* ** Operands and constructs for intermediate language
 * ------------------------------------------------------------------------ *)
(* *** Summary
The language supports the fixed operations given in 'op' (and function calls).
*)
(* *** Code *)

type cmov_flag = CfSet of bool with sexp, compare

type dir      = Left   | Right                with sexp, compare
type carry_op = O_Add  | O_Sub                with sexp, compare
type three_op = O_Imul | O_And | O_Xor | O_Or with sexp, compare

type op =
  | ThreeOp of three_op
  | Umul    of             dest
  | Carry   of carry_op  * dest option * src option
  | CMov    of cmov_flag               * src
  | Shift   of dir       * dest option
  with sexp, compare

(* ** Base instructions, instructions, and statements
 * ------------------------------------------------------------------------ *)
(* *** Summary
- base instructions (assignment, operation, call, comment)
- instructions (base instructions, if, for)
- statements (list of instructions) *)
(* *** Code *)


type assgn_type =
  | Mv (* compile to move *)
  | Eq (* use as equality constraint in reg-alloc and compile to no-op *)
  with sexp, compare

type for_type =
  | Unfold
  | Loop
  with sexp, compare

type base_instr =
  
  | Assgn of dest * src * assgn_type
    (* Assgn(d,s): d = s *)

  | Op of op * dest * (src * src)
    (* Op(o,d,(s1,s2)): d = o s1 s2
       o can contain additional dests and srcs *)

  | Call of name * dest list * src list
    (* Call(fname,rets,args): rets = fname(args) *)

  | Load of dest * src * pexpr
    (* Load(d,src,pe): d = MEM[src + pe] *)

  | Store of src * pexpr * src
    (* Store(src1,pe,src2): MEM[src1 + pe] = src2 *) 

  | Comment of string
    (* Comment(s): /* s */ *)

  with sexp, compare

type instr =

  | Binstr of base_instr

  | If of pcond * stmt * stmt
    (* If(c1,i1,i2): if c1 { i1 } else i2 *)

  | For of for_type * name * pexpr * pexpr * stmt
    (* For(v,lower,upper,i): for v in lower..upper { i } *)

and stmt = (instr L.located) list
  with sexp, compare

(* ** Function definitions, declarations, and modules
 * ------------------------------------------------------------------------ *)

type call_conv =
  | Extern
  | Custom
  with sexp, compare

type storage =
  | Flag
  | Inline
  | Stack
  | Reg
  with sexp, compare

type fundef = {
  fd_decls  : (storage * name * ty) list; (* function-local declarations *)
  fd_body   : stmt;                       (* function body *)
  fd_ret    : name list                   (* return values *)
} with sexp, compare

type fundef_or_py =
  | Undef
  | Def of fundef
  | Py of string
  with sexp, compare

type func = {
  f_name      : name;                       (* function name *)
  f_call_conv : call_conv;                  (* callable or internal function *)
  f_args      : (storage * name * ty) list; (* formal function arguments *)
  f_def       : fundef_or_py;               (* def. unless function just declared *)
  f_ret_ty    : (storage * ty) list;        (* return type *)
} with sexp, compare

type modul = {
  m_params : (name * ty) list; (* module parameters *)
  m_funcs  : func list;        (* module functions  *)
} with sexp, compare

(* ** Values
 * ------------------------------------------------------------------------ *)

type value =
  | Vu64 of u64
  | Varr of u64 U64.Map.t
  with sexp, compare

(* ** Define Map, Hashtables, and Sets
 * ------------------------------------------------------------------------ *)

module Dest = struct
  module T = struct
    type t = dest with sexp
    let compare = compare_dest
    let hash v = Hashtbl.hash v
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make(T)
end
