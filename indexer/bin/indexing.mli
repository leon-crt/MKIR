open Kernel.Basic
open Kernel.Term

type 'a index
val empty : 'a index
val insert : 'a index -> term -> 'a -> 'a index
val search : 'a index -> term -> 'a list
val save: 'a index -> string -> unit
val load: 'a index -> string -> unit

module DB : sig
 val save: string -> unit
 val load: string -> unit
 
 type obj = 
  | Const of (mident * ident * (*isFullTerm*) bool)
  | Rule of (mident * loc * name * (*false = lhs,true = rhs*) bool)
 val insert : term -> obj -> unit
 val search : term -> obj list
end 
