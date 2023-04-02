open Parsers
open Kernel
open Indexing

(*
 Sto indicizzando i termini usando l'identificatore del modulo,
 dovrei invece utilizzare l'ident delle entry ma mi sfugge come recuperarlo.
 In Env viene fatta una hashtable di tutti i name e la si riempie iterando 
 sui simboli nella signature (?), dai name si può facilmente recuperare l'ident 
 e si potrebbe indicizzare così. 
 Non mi è chiaro il collegamento tra term e name / ident.
 *)

let file = "/home/leon/tesi/Dedukti/examples/append.dk"
exception Err_term of string

(*misc functions*)
let print_entry e = 
        Entry.pp_entry Format.std_formatter e   

let index t = 
        match t with
        | (term, ident) -> DB.insert term ident

(*Whole file parsing
let extract_term_ident e =
        let t = Term.mk_Type (Entry.loc_of_entry e) in
        (t * (Parser.md_of_input

let handle_indexing e =
        let term_to_index = extract_term_ident e in
        index term_to_index

let parse_file_at_once f = 
        Parser.handle f handle_indexing 
*)

(*Line by line parsing*)
let handle_indexing_lbl e mident = 
        let term_to_index = ((Term.mk_Type (Entry.loc_of_entry e)), mident) in
        index term_to_index

let rec parse_line_by_line stream mident=
        try 
                let e = Parser.read stream in
                handle_indexing_lbl e mident;
                parse_line_by_line stream mident
        with e ->
                if e = End_of_file then ()
                else raise e

let () =  
        try
                let f = Parser.input_from_file file in
                parse_line_by_line (Parser.from f) (Parser.md_of_input f);
                flush stdout;
        with e ->
                raise e
