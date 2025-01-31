open CRelationClasses
open Init

type ('a, 'r) coq_Proper = 'r

type ('a, 'b, 'r, 'x) respectful = 'a -> 'a -> 'r -> 'x

(** val subrelation_respectful :
    ('a1, 'a2, 'a3) subrelation -> ('a4, 'a5, 'a6) subrelation -> ('a1 ->
    'a4, ('a1, 'a4, 'a3, 'a5) respectful, ('a1, 'a4, 'a2, 'a6) respectful)
    subrelation **)

let subrelation_respectful subl subr x y x0 x1 y0 x2 =
  subr (x x1) (y y0) (x0 x1 y0 (subl x1 y0 x2))

(** val subrelation_proper :
    'a1 -> ('a1, 'a2) coq_Proper -> 'a1 crelation coq_Unconvertible -> ('a1,
    'a2, 'a3) subrelation -> ('a1, 'a3) coq_Proper **)

let subrelation_proper m mor _ sub =
  sub m m mor
