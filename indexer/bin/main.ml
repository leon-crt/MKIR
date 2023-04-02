open Parsers
open Kernel
open Kernel.Basic

let file = "/home/leon/tesi/Dedukti/examples/append.dk"
exception Err_term

(*misc functions*)
let print_entry e = 
        Entry.pp_entry Format.std_formatter e   

let index t = 
        match t with
        | (term, ident) -> Indexing.DB.insert term ident
        | _ -> raise Err_term 

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
        let term_to_index = ((Term.mk_Type (Entry.loc_of_entry e)) * mident) in
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
