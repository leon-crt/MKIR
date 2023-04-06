open Kernel.Basic
open Kernel.Term

type 'a index
(*
type resultList =
  | Term of (mident * ident * (*isFullTerm*) bool) list
  | Rule of (mident * loc * name) list
*)
val empty : 'a index
val insert : 'a index -> term -> 'a -> 'a index
val search : 'a index -> term -> 'a list

module DB : sig
 val insert : term -> (mident * ident) -> unit
 val search : term -> (mident * ident) list
end 
