open Basic
open Term

let type 'a index =
        | Leaf of 'a list
        | Choice of node list
and 'a node =
        | IHOLE of 'a index
        | IRigid of rigid * index
and rigid =
        | IKind
        | IType
        | IConst of name
        | IApp of int (*l'int è l'arietà dell'applicazione*)
        | ILam
        | IPi

let empty = Choice of []

(*dato uno stack di termini e v che non so cosa sia, crea un indice di quei termini*)
let rec node_of_stack term stack v =
        match term with
        | Kind -> IRigid(IKind, index_of_stack stack v)
        | Type -> IRigid(IType, index_of_stack stack v)
        | DB(_,_,_) -> IHOLE(index_of_stack stack v) (*??*)
        | Const(_,name) -> IRigid(IConst name, index_of_stack stack v)
        | App(t1, t2, tl) -> IRigid(IApp(2 + List.length tl, index_of_stack (t1::t2::tl@stack) v)
        | Lam(_,_,Some t1 t2) -> IRigid(ILam, index_of_stack (t1::t2::stack) v)
        | Lam(_,_,None,_) -> assert false (*??*)
        | Pi(_,_,t1,t2) -> IRigid(IPi, index_of_stack (t1::t2::stack) v)
and index_of_stack stack v =
        match stack with
        | [] -> Leaf [v]
        | t::s -> Choice [node_of_stack t s v]

exception NoMatch

let match_rigid rigid term = 
        match rigid, term with
        | IKind, Kind -> []
        | IType, Type -> []
        | IConst n, Const(_,n') -> when n = n' -> []
        | IApp n, App(t1,t2,tl) -> 
                if(n = 2 + List.length tl) then [] 
                else raise NoMatch
        | ILam, Lam(_,_,Some t1, t2) -> [t1; t2]
        | ILam, Lam(_,_,None,_) -> assert false
        | IPi, Pi(_,_,t1,t2) -> [t1;t2]
        | _, _ -> raise NoMatch (*caso default*)

let rec insert_index index stack v = 
        match index, stack with
        | Leaf vs, [] -> Leaf [v::vs]
        | Choice nodes, t::s -> 
                let rec aux = 
                        function
                        | [] -> [node_of_stack t s v]
                        | n::nl -> 
                                try (insert_node n t s v) :: nl                
                                with NoMatch -> n :: (aux nl)
                        in Choice (aux nodes)
        | _, _ -> assert false (*termine scritto male*)
and insert_node node term stack v = 
        match node,term with
        | IHOLE index, DB _ -> IHOLE (insert_index index stack v)
        | IRigid (rigid,index), t ->
                let s' = match_rigid rigid t @ stack in
                IRigid (r, insert_index index (s'@s) v) (*restituiamo un nodo con r come contenuto e l'indice formato inserendoci lo stack con aggiunti i termini da valutare in più dati da match_rigid*)
        | _, _ -> raise NoMatch

let insert index term v = insert_index index [term] v (*l'inserimento di un termine è fatto inserendo uno stack contenente solo quel termine*)

let rec search_index index stack =
        match index,stack with
        | Leaf vs, [] -> vs
        | Choice nodes, t::s -> 
                (* mio tentativo:
                        let rec aux =
                        function
                        | [] -> raise NoMatch
                        | n::nl -> search_node n t s
                in Choice (aux nodes)
                *)
                (*restituiamo una lista contenente tutte le v che matchano scorrendo l'indice per ognuno dei nodi in nodes? Se non ce ne sono restituisce lista vuota. 
                fold_right applica fun come segue: 
                        fun nodes dove nodes = n::nl -> (search_node n t s) @ (fun nl) *)
                List.fold_right 
                        (fun n res -> (search_node n t s)@res) nodes [] 
        | _,_ -> assert false
and search_node node term stack =
        match node,term with
        | IHOLE index, _ -> search_index index stack
        | IRigid (r, index), t -> 
                match match_rigid r t with
                | s' -> search_index index (s'@stack)
                | exception NoMatch -> []

