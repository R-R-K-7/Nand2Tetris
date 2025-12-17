open Jackvm

let read_file fname = In_channel.with_open_text fname In_channel.input_all

let write_file fname content = Out_channel.with_open_text fname (fun oc->Out_channel.output_string oc content)

(*Read Command line arguments and return a list of all valid filenames*)
let read f = let has_vm_suffix file = Filename.check_suffix file ".vm" in
             if Sys.file_exists f then
                if Sys.is_directory f then
                        let all_contents = Sys.readdir f |> Array.to_list in
                        List.filter_map (fun file ->
                               let full_path = Filename.concat f file in
                               try
                                      if 
                                      Sys.file_exists full_path &&
                                      not(Sys.is_directory full_path) &&
                                      (has_vm_suffix full_path)
                                      then Some full_path
                                      else None
                               with Sys_error _ -> None
                        ) all_contents
                else
                        if (has_vm_suffix f) then [f] else []
             else(
             prerr_endline "Provide valid file/folder name";
             exit 1
             )
let base_name file = Filename.remove_extension (Filename.basename file)

(*Returns a list of (filename,contents of file as string)*)
let get_all_file_contents file_list = List.map (fun f -> (f,read_file f)) file_list

(* ('a,'b) list -> 'b list *)
let get_second_list xs = List.map (fun xs -> let (_,s) = xs in s ) xs

(*Checks for usage of undefined functions and duplicate functions across all files*)
let function_check prog_list = let function_set = Translate.Check.function_set in
                               let empty = Translate.LabelSet.empty in (*Gives an empty set of strings*)
                               let fun_set, e_def = List.fold_left (fun (set,e) prog -> function_set (set,e) prog) (empty,"") prog_list in
                               let function_call = Translate.Check.function_call in      (*e_def = errors due to duplicate function definitions*)
                               let chk_h xs = List.map (function_call fun_set) xs in     (*fun_set = Set of all functions across all files*)
                               let e_call_list = List.filter (fun i -> i<>"") (chk_h prog_list) in 
                               let e_call = String.concat "\n" e_call_list in            (*e_call = errors due to calling of undefined functions*)
                               let e = e_call^e_def in
                               if e <> "" then 
                                       raise (Translate.TranslateError e)
                               else ()

(*Gets list of vm_prog of all files*)
let get_prog_list files_content_list = let get_vm_prog = Parser.Parse.get_vm_prog in
                                       List.map (fun xs -> let f,s = xs in
                                       (f, get_vm_prog s)) files_content_list

(*Converts list of vm_prog to asm_program*)
(*Note: the labels in asm instructions have type 'addess'*)
let get_all_asm file_prog_list =  let getasm = Translate.Program.getasm in
                                  let helper (xs:(string*Ast.Program.t)) = let f, prog = xs in
                                                  let base = base_name f in
                                                  getasm prog base in
                                  let all_asm = List.map helper file_prog_list in
                                  List.flatten ((Translate.Init.init)::(all_asm))

(* This gives the string prog type by converting 'address' type to string *)
let get_program p = let map_to_string = Translate.Map_address.map_to_string in 
                    Assembler.Ast.Program.map map_to_string (Translate.Program.get_prog p) 

let pretty_print = Assembler.Ast.PrettyPrint.prog

(*Getting binary version of program*)
let get_bin prog = let label_table = Assembler.Ast.Program.address prog in (*Creates a list of tuples containing label_name, their address, as well as inbuilt variables*)
                   let n = ref 16 in                                       (*n points to the address where a new variable will be allocated space*)
                   let symbol_table = Hashtbl.create 100 in                (*Symbol table (Hash table) created, and values in label table are stored in it*)
                   let () = List.iter (fun (label,address)->Hashtbl.add symbol_table label address) label_table in
                   let f x = match int_of_string_opt x with
                            | Some i -> Some i                             (*if i is a int, then it refers to a constant (e.g. @42)*)
                            | None   ->(match Hashtbl.find_opt symbol_table x with
                                        | Some i -> Some i                 (*Checks if i is a label/ already encountered variable*)
                                        | None   -> (let newvar = !n in    (*adds i to symbol table if its a new variable*)
                                                     Hashtbl.add symbol_table x newvar;
                                                     n := !n + 1;
                                                     Some newvar)
                                        ) in
                   let resolved_ast = Assembler.Ast.Program.resolve f prog in 
                   match resolved_ast with                                 (*Program with type string is converted into type integer*)
                            | Assembler.Ast.Result.Value a -> Assembler.Machine.Assemble.encode_pgm_base2 a
                            | Assembler.Ast.Result.List e -> String.concat "\n" e (*List = Errors, Value = Correct output*)

let output_file_base f = if Sys.is_directory f then
                                let base = Filename.basename f in
                                Filename.concat f base
                         else 
                                Filename.remove_extension f

let () = try
                let input_path = ref "" in
                let asm_flag = ref  false in                               (* -a flag for getting assembly output*)
                let specs = [("-a", Arg.Set asm_flag, "Compiles to Assembly, ignores generation of hack file")] in
                let msg = "Usage: dune exec jackvm [-a] <file_or_dir.vm>" in
                let getfiles fname = (input_path:=fname) in
                Arg.parse specs getfiles msg;
                if (!input_path="") then(
                        prerr_endline ("Error: No input file or directory provided");
                        exit 1);
                let f = !input_path in
                let files = read f in (*reads directory to give list of valid files*)
                if files = [] then( 
                        prerr_endline ("Error: No .vm files found in "^f) ;
                        exit 1);
                let s_files = get_all_file_contents files in
                let s_prog_list = get_prog_list s_files in 
                let prog_list = get_second_list s_prog_list in
                let _ = function_check prog_list in
                let total_asm = get_all_asm s_prog_list in
                let prog = get_program total_asm in
                let out_file_base = output_file_base f in
                let asm_outfile = out_file_base^".asm" in 
                if not !asm_flag then(                            (*if asm_flag not used, then only hack file is created*)
                        let bin = get_bin prog in
                        let hack_outfile = out_file_base^".hack" in 
                        write_file hack_outfile bin;
                        print_endline ("Successfully compiled to " ^ hack_outfile)
                ) else (                                          (*if asm_flag not used, only asm file created*)
                        let pretty_printed = pretty_print prog in 
                        write_file asm_outfile pretty_printed;
                        print_endline ("Successfully compiled to " ^ asm_outfile)
                )
         with 
         | Parser.ParseError msg ->
                         Printf.eprintf "Fatal Error:\n%s" msg
         | Translate.TranslateError msg -> 
                         Printf.eprintf "Translate Error:\n%s" msg
         | e ->
                         Printf.eprintf "Unexpected Error occurred:\n%s" (Printexc.to_string e) 
    
