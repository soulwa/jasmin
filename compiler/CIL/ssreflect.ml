
(** val locked_with : unit -> 'a1 -> 'a1 **)

let locked_with _ x =
  x

(** val ssr_have : 'a1 -> ('a1 -> 'a2) -> 'a2 **)

let ssr_have step rest =
  rest step
