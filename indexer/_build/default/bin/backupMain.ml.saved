open Parsers
open Kernel.Basic

let file = "/home/leon/tesi/Dedukti/examples/append.dk"
exception Err_term of loc

let print_entry e = 
        Entry.pp_entry Format.std_formatter e   

let extract_term_ident e =
        match Entry.loc_of_entry e with
        | (i:ident, sc: scope, st: staticity, t:term) -> (t*i)
        | err -> raise Err_term err (*ill-typed term*)

let index t = 
        match t with
        | (term, ident) -> Indexing.DB.insert term ident
        | _ -> raise Err_term

let handle_indexing e =
        let term_to_index = extract_term_ident e in
        index term_to_index

let parse_file f = 
        Parser.handle f handle_indexing 

let () = 
        let ic = open_in file in 
        try
                parse_file (Parser.input_from_file file);
                flush stdout;
                close_in ic
        with e ->
                close_in_noerr ic;
                raise e
