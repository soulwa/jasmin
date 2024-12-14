open Datatypes
open Equalities
open MSetFacts
open MSetInterface

module WDecideOn =
 functor (E:DecidableType) ->
 functor (M:sig
  type elt = E.t

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1

  val for_all : (elt -> bool) -> t -> bool

  val exists_ : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> nat

  val elements : t -> elt list

  val choose : t -> elt option

  val eq_dec : t -> t -> bool
 end) ->
 struct
  module F = WFactsOn(E)(M)

  module MSetLogicalFacts =
   struct
   end

  module MSetDecideAuxiliary =
   struct
   end

  module MSetDecideTestCases =
   struct
   end
 end

module WDecide =
 functor (M:WSets) ->
 WDecideOn(M.E)(M)
