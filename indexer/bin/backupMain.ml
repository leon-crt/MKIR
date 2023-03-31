open Parsers

let file = "/home/leon/tesi/Dedukti/examples/append.dk"

let print_entry e = 
        Entry.pp_entry Format.std_formatter e      

let parse_file f = 
        Parser.handle f print_entry
       

let () = 
        let ic = open_in file in 
        try
                parse_file (Parser.input_from_file (input_line ic));
                flush stdout;
                close_in ic
        with e ->
                close_in_noerr ic;
                raise e
