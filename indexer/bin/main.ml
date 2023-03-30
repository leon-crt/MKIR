let file = "/home/leon/Tesi/Dedukti/examples/append.dk"
let rec print_file l = 
        try
                
let () = 
        let ic = open_in file in 
        try
                let line = input_line ic in
                print_endline line;
                flush stdout;
                close_in ic
        with e ->
                close_in_noerr ic;
                raise e
