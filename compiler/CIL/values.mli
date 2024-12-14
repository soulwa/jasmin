open BinNums
open Datatypes
open Sem_type
open Ssralg
open Type
open Utils0
open Warray_
open Word0
open Wsize

type value =
| Vbool of bool
| Vint of coq_Z
| Varr of positive * WArray.array
| Vword of wsize * GRing.ComRing.sort
| Vundef of stype

type values = value list

val type_of_val : value -> stype

val to_bool : value -> (error, bool) result

val to_int : value -> (error, coq_Z) result

val to_arr : positive -> value -> WArray.array exec

val to_word : wsize -> value -> GRing.ComRing.sort exec

val of_val : stype -> value -> sem_t exec

val to_val : stype -> sem_t -> value

val oto_val : stype -> sem_ot -> value

val truncate_val : stype -> value -> value exec

val list_ltuple : stype list -> sem_tuple -> values

val app_sopn : stype list -> 'a1 exec sem_prod -> value list -> 'a1 exec
