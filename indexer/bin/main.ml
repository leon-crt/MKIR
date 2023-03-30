open Parsers

let file = "/home/leon/tesi/Dedukti/examples/append.dk"
let rec print_file f = 
        try   
                let l = input_line f in
                let parsed_line = dk_parse l in
                print_file f
        with e ->
                if e = End_of_file then 
                        print_endline "done"
                else raise e 

let rec dk_parse s =
        let i = input_from_file s in
        parse i;

let () = 
        let ic = open_in file in 
        try
                print_file ic;
                flush stdout;
                close_in ic
        with e ->
                close_in_noerr ic;
                raise e
