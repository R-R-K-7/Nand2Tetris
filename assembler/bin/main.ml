open Assembler

(*reads file's contents as a string*)
let read_file fname = In_channel.with_open_text fname In_channel.input_all
	
(*writes a string into a file*)
let write_file fname content = Out_channel.with_open_text fname (fun oc->Out_channel.output_string oc content) 

let () = if Array.length Sys.argv < 2 then
		print_endline "Provide file name to be assembled"
	 else if Array.length Sys.argv < 3 then
		print_endline "Provide file name to write the instructions"
	 else 
		let infile = Sys.argv.(1) in   (*Has path to input file*)
		let outfile = Sys.argv.(2) in  (*Has path of output file*)
		try
                        let check_suff = Filename.check_suffix in
                        if not (check_suff infile ".asm") then (
                                Printf.eprintf "Error: Input file '%s' must have an .asm extension\n" infile;
                        exit 1 ) else
                                if not (check_suff outfile ".hack") then (
                                        Printf.eprintf "Error: Output file '%s' must have a .hack extension\n" outfile;
                                        exit 1
                        ) else
			let f_str = read_file infile in 
			let prog = Parser.Parse.get_program f_str in
			let label_table = Ast.Program.address prog in
			let n = ref 16 in                               (*n gives the address of new variables created*)
			let symbol_table = Hashtbl.create 100 in        (*symbol table created, and labels, all default variables are added to it*)
			let () = List.iter (fun (label,address)->Hashtbl.add symbol_table label address) label_table in
			let f x = match int_of_string_opt x with
				| Some i -> Some i                      (*if i is an int its a constant*)
				| None   ->(match Hashtbl.find_opt symbol_table x with
					    | Some i -> Some i          (*if i is in symbol table, its a label, or already encountered variable*)
					    | None   -> (let newvar = !n in
							Hashtbl.add symbol_table x newvar; 
							n := !n + 1;    (*if not in symbol table, new variable is created*)
							Some newvar)
					   ) in
			let resolved_ast = Ast.Program.resolve f prog in (*converts string program into int program*)
			let assembled = match resolved_ast with
					| Ast.Result.Value a -> Machine.Assemble.encode_pgm_base2 a (*encoding program into binary strings*)
					| Ast.Result.List e -> let err = String.concat "\n" e in
                                                               failwith (Printf.sprintf "Assembly Error:\n%s" err)
					in
                        write_file outfile assembled;
                        print_endline ("Successfully compiled to " ^ outfile)
		with
			| Sys_error msg ->
                                           Printf.eprintf "Error reading file: %s\n" msg
                        | Failure msg   ->
                                           Printf.eprintf "Assembly Failed:\n%s" msg
                        | Parser.Parse.ParseError msg ->
                                           Printf.eprintf "Assembly Failed:\n%s\n" msg
                        | e             ->
                                           Printf.eprintf "Unexpected Error occurred:\n%s\n" (Printexc.to_string e)


