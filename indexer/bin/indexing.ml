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

(* INSERTION *)
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
        | NoMatch -> n :: aux nl
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

(* SUBTERM INSERTION *)
let rec get_subs term =
 match term with 
 | Pi(_,_,t1,t2)
 | Lam(_,_,Some t1, t2) -> t1::t2::(get_subs t1)@(get_subs t2)
 | App(t1,t2,tl) -> let rec aux =
         function
         | [] -> []
         | t::tl -> (get_subs t) @ (aux tl)
         in t1::t2::(aux (t1::t2::tl));
 | _ -> []

let insert_all_subterms index term v = (*Possono esserci foglie duplicate*) 
        List.fold_left (fun ind t -> insert_index ind [t] v) index (term::(get_subs term))

(* SEARCH *)
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

(* TREE PRINTING
let stringify rigid =
  match rigid with
        | IKind -> "Kind"
        | IType -> "Type"
        | IConst n -> "Const "^(string_of_ident (id n))
        | IApp n -> "App"^(string_of_int n)
        | ILam -> "Lambda"
        | IPi -> "Pi" 

let ident_of_obj o = 
 match o with
 | Const (_,i,_) -> i
 | Rule (_,_,name,_) -> id name

let rec istringify identlist = 
  match identlist with
    | [] -> []
    | o::il -> ("    Leaf: "^(string_of_ident (ident_of_obj o)))::(istringify il)

let rec print_tree index = 
  match index with
    | Leaf vs -> istringify vs
    | Choice l -> 
        let rec aux = 
          function
          | [] -> []
          | n :: nl -> (follow_branch n)@(aux nl)
        in aux l
and follow_branch node =
  match node with
    | IHOLE i -> "_"::(print_tree i)
    | IRigid (r,i) -> (stringify r)::(print_tree i)
*)

let save index fileName = 
        let out_chan = open_out fileName in
        Marshal.to_channel out_chan index [] ;
        prerr_endline "Salvataggio"

let load index fileName =
        let in_chan = open_in fileName in
        Marshal.from_channel in_chan 

module DB = struct 
 let db = ref empty
 let save filename = save !db filename
 let load filename = db := load !db filename (*sovrascrive il db già caricato, sarebbe carino stampare un warning*)
 type obj =
  | Const of (mident * ident * (*isFullTerm*) bool)
  | Rule of (mident * loc * name * bool)
 let insert k v = db := insert_all_subterms !db k v
 let search k = search !db k
(* let print () = print_tree !db*)
end
