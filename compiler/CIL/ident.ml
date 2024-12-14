open Bool
open Datatypes
open Eqtype
open Gen_map
open Strings

type __ = Obj.t

module type IDENT =
 sig
  val ident : Equality.coq_type

  module Mid :
   MAP
 end

module Ident =
 struct
  (** val ident : Equality.coq_type **)

  let ident =
    Equality.clone string_eqType (Obj.magic string_eqMixin) (fun x -> x)

  module Mid = Ms
 end
