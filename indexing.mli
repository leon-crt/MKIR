open Basic
open Term

type 'a index

val empty : 'a index
val insert : 'a index -> term -> 'a -> 'a index
val search : 'a index -> term -> 'a list

module DB : sig
 val insert : term -> ident -> unit
 val search : term -> ident list
end 
