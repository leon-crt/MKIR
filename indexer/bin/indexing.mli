open Kernel.Basic
open Kernel.Term

type 'a index
type resultList =
  | Term of (mident * ident) list
  | Rule of (mident * name * loc) list

val empty : 'a index
val insert : 'a index -> term -> 'a -> 'a index
val search : 'a index -> term -> 'a list

(*Cambiamento del tipo dei metodi da ident a mident provvisorio*)
module DB : sig
 val insert : term -> mident * ident -> unit
 val search : term -> (mident * ident) list
end 
