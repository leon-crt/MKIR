let file = "/home/leon/tesi/Dedukti/examples/append.dk"
let rec print_file f = 
        try   
                let l = input_line f in
                print_endline l;
                print_file f
        with e ->
                if e = End_of_file then 
                        print_endline "done"
                else raise e 


let () = 
        let ic = open_in file in 
        try
                print_file ic;
                flush stdout;
                close_in ic
        with e ->
                close_in_noerr ic;
                raise e
