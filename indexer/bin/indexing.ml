open Kernel.Basic
open Kernel.Term

(* substitution trees would be best *)

(* discrimination tree *)
type 'a index =
 | Leaf of 'a list
 | Choice of 'a node list
and 'a node =
 | IHOLE of 'a index
 | IRigid of rigid * 'a index
and rigid =
 | IKind
 | IType
 | IConst of name
 | IApp of int (* number of args *)
 | IRule of 
 | ILam
 | IPi

let empty = Choice []

let rec node_of_stack t s v =
 match t with
 | Kind -> IRigid(IKind, index_of_stack s v)
 | Type _ -> IRigid(IType, index_of_stack s v)
 | DB(_,_,_) -> IHOLE (index_of_stack s v)
 | Const(_,n) -> IRigid(IConst n, index_of_stack s v)
 | App(t1,t2,tl) ->
    IRigid(IApp (2 + List.length tl), index_of_stack (t1::t2::tl@s) v)
 | Lam(_,_,Some t1,t2) ->
    IRigid(ILam, index_of_stack (t1::t2::s) v)
 | Lam(_,_,None,_) -> assert false
 | Pi(_,_,t1,t2) -> IRigid(IPi, index_of_stack (t1::t2::s) v)
and index_of_stack stack v =
 match stack with
 | [] -> Leaf [v]
 | t::s -> Choice [node_of_stack t s v]

exception NoMatch

(* match a rigid with a term, either raising NoMatch or returning the
   (ordered) list of immediate subterms of the term *)
let match_rigid r term =
 match r,term with
 | IKind, Kind -> []
 | IType, Type _ -> []
 | IConst n, Const(_,n') when n = n' -> []
 | IApp n, App(t1,t2,tl) ->
    if n = 2 + List.length tl then t1::t2::tl
    else raise NoMatch
 | ILam, Lam(_,_,Some t1,t2) -> [t1;t2]
 | ILam, Lam(_,_,None,_) -> assert false (* I could use an IHOLE in recursive call *)
 | IPi, Pi(_,_,t1,t2) -> [t1;t2]
 | _, _ -> raise NoMatch

let rec insert_index index stack v =
 match index,stack with
 | Leaf vs, [] -> Leaf(v::vs)
 | Choice l, t::s ->
    let rec aux =
     function
     | [] -> [node_of_stack t s v]
     | n::nl ->
       try
        insert_node n t s v :: nl
       with
        NoMatch -> n :: aux nl
    in Choice(aux l)
 | _, _ -> assert false (* ill-typed term *)
and insert_node node term s v =
 match node,term with
 | IHOLE i, DB _ -> IHOLE (insert_index i s v)
 | IRigid(r,i), t ->
    let s' = match_rigid r t in
    IRigid(r,insert_index i (s'@s) v)
 | _, _ -> raise NoMatch

let insert index term v = insert_index index [term] v

let rec search_index index stack =
 match index,stack with
 | Leaf vs, [] -> vs
 | Choice l, t::s ->
    List.fold_right
     (fun n res -> search_node n t s @ res) l []
 | _, _ -> assert false (* ill-typed term *)
and search_node node term s =
 match node,term with
 | IHOLE i, _ -> search_index i s
 | IRigid(r,i), t ->
     match match_rigid r t with
     | s' -> search_index i (s'@s)
     | exception NoMatch -> []

let search index term = search_index index [term]

module DB = struct 
 let db = ref empty
 let insert k v = db := insert !db k v
 let search k = search !db k
end
