open Parsers
open Kernel
open Rule
open Indexing

(*
 Sarebbe tutto pronto da testare ma il nuovo tipo di ritorno DB.obj fa avere conflitti 
 alle dichiarazioni di insert. Ora dovrebbero venire indicizzate anche le regole e sono 
 disponibili funzioni di marshal e unmarshal.
 *)

let file = "/home/leon/tesi/MKIR/indexer/test.dk"
exception Err_term of string

(*misc functions*)
let print_entry e = 
        Entry.pp_entry Format.std_formatter e   

let rec print_index nlist = 
  match nlist with
    | [] -> prerr_endline "Finished"
    | n::nl -> prerr_endline n; print_index nl

let name_of_ruleName rn mi =
 match rn with 
 | Beta -> Basic.mk_name mi (Basic.mk_ident "Beta_Rule")
 | Delta name | Gamma (_,name) -> name

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

let index_rule mi r loc = let index_pattern pat mi name loc = 
   match pat with
   | Brackets term -> DB.insert term (DB.Rule(mi, loc, (name_of_ruleName name mi), false)) 
   | Var _ | Pattern _ | Lambda _ -> () (*Avrebbe senso usare le info contenute qui per arricchire le foglie delle regole?*)  
 in index_pattern r.pat mi r.name loc ; DB.insert r.rhs (DB.Rule (mi, loc, (name_of_ruleName r.name mi), true))

let handle_indexing_lbl e mident = 
 match e with
  | Entry.Require (_, _mident) -> ()  (*TODO???*)
  | Decl (_loc, ident, _, _, typ) ->
             DB.insert typ (DB.Const (mident, ident, true)) ;
     prerr_endline ("NUOVA INDICIZZAZIONE: " ^ Basic.string_of_mident mident ^ "." ^ Basic.string_of_ident ident);
     List.iter
      (fun (res) ->
        match res with
        | DB.Const (mi, i, b) -> prerr_endline ("TROVATO: " ^ Basic.string_of_mident mi ^ "." ^ Basic.string_of_ident i ^ ":" ^ (string_of_bool b))
        | DB.Rule (mi, _loc, name, b) -> prerr_endline ("TROVATO: " ^ Basic.string_of_mident mi ^ "." ^ Basic.string_of_ident (Basic.id name) ^ ":" ^ string_of_bool b))
      (DB.search typ)
  | Def (_loc, ident, _, _, _def (* term option*), typ) ->
     DB.insert typ (DB.Const (mident, ident, true)) 
  | Rules (loc, rules (*Rule.partially_typed_rule list*)) -> List.iter (fun r -> index_rule mident r loc) rules 
  | Eval _ | Check _ | Infer _ | Print _ | DTree _ | Name _ -> ()  

let rec parse_line_by_line stream mident=
  let eopt =
   try Some (Parser.read stream)
   with End_of_file -> None in
  match eopt with
     None -> ()
   | Some e ->
      handle_indexing_lbl e mident;
      parse_line_by_line stream mident

let () =  
        try
                let f = Parser.input_from_file file in
                parse_line_by_line (Parser.from f) (Parser.md_of_input f);
                flush stdout;
        with e ->
                raise e
