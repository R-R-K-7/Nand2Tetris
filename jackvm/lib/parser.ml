(*Parser for VM language*)

module Segment = Ast.Segment
module Stack_inst = Translate.Stack_inst
module Cmd = Ast.Commands

exception ParseError of string
(*Used for converting list of errors into string*)
let map_error xs = let e_list = List.map (fun x -> let (i,s) = x in
                                "line "^(string_of_int i)^": "^s) xs in
                   let res = String.concat "\n" e_list in
                   res^"\n"

module Parse = struct
       
        
        let strip_comment line = match String.index_opt line '/' with
                                | Some i -> String.trim (String.sub line 0 i)   (*Even in case of incorrect comments, p_line will take care of the error*)
                                | None   -> String.trim line

        let segment seg = match seg with
                        | "argument" -> Some Segment.ARG
                        | "local"    -> Some Segment.LCL
                        | "this"     -> Some Segment.THIS
                        | "that"     -> Some Segment.THAT
                        | "temp"     -> Some Segment.TEMP
                        | _          -> None

        let index n = int_of_string_opt n
                                
        let push s ind = match s with
                           | "static"   -> Some (Cmd.PushStatic ind)
                           | "constant" -> Some (Cmd.PushConst ind)
                           | "pointer"  -> (match ind with
                                            |0 -> Some (Cmd.Push(Segment.THIS,-1)) 
                                            |1 -> Some (Cmd.Push(Segment.THAT,-1))
                                            |_ -> None
                                            )
                           | _          -> match (segment s) with
                                           | Some i -> Some(Cmd.Push(i,ind))
                                           | None   -> None

        let pop s ind = match s with
                           | "static"   -> Some (Cmd.PopStatic ind)
                           | "pointer"  -> (match ind with
                                            |0 -> Some (Cmd.Pop(Segment.THIS,-1)) 
                                            |1 -> Some (Cmd.Pop(Segment.THAT,-1))
                                            |_ -> None
                                            )
                           | _          -> match (segment s) with
                                           | Some i -> Some (Cmd.Pop(i,ind))
                                           | None   -> None
         let arithmetic op = match op with
                          | "add" -> Some (Cmd.Arithmetic Add)
                          | "sub" -> Some (Cmd.Arithmetic Sub)
                          | "neg" -> Some (Cmd.Arithmetic Neg)
                          | "eq"  -> Some (Cmd.Arithmetic Eq)
                          | "gt"  -> Some (Cmd.Arithmetic Gt)
                          | "lt"  -> Some (Cmd.Arithmetic Lt)
                          | "and" -> Some (Cmd.Arithmetic And)
                          | "or"  -> Some (Cmd.Arithmetic Or)
                          | "not" -> Some (Cmd.Arithmetic Not)
                          | _     -> None

        let return = function
                     | "return" -> Some Cmd.Return
                     | _        -> None

 (*Checks correctness of the label and function names used*)  
        let check_char = function
                        |'a'..'z'
                        |'A'..'Z'
                        |'0'..'9'
                        | ':' | '.' | '_' -> true
                        | _ -> false 

        let is_not_digit = function
                          |'0'..'9' -> false
                          |_    -> true

        let check_var_name s = (String.for_all check_char s) && (is_not_digit s.[0])

(*Functions for three types of commands: with no, one and two arguments*)
        let no_arg s line n = match (arithmetic s) with
                              | Some i -> Ok (Some i)
                              | None   -> (match (return s) with
                                          | Some r -> Ok (Some r)
                                          | None   -> Error (n,"Invalid Command: "^line))

        let one_arg s arg1 line n = match s with
                                    | "goto"    -> Ok (Some (Cmd.Goto arg1))
                                    | "if-goto" -> Ok (Some (Cmd.If arg1))
                                    | "label"   -> if (check_var_name arg1) then Ok (Some (Cmd.Label arg1)) 
                                                   else Error (n,"Invalid Label Name: "^arg1^" in "^line)
                                    |  _        -> Error (n,"Invalid Command: "^line)


        let two_arg s arg1 arg2 line n = match index arg2 with 
                                         | Some num ->
                                                      (if num < 0 && not(arg1="constant" && s="push") then
                                                        Error (n, "Invalid index: cannot be negative: "^arg2^" in "^line)
                                                       else
                                                       match s with    (*Checking name correctness before converting to ast of vm*)
                                                       | "function" -> if (check_var_name arg1) then Ok (Some (Cmd.Function (arg1,num)))
                                                                       else Error (n,"Invalid Function Name "^arg1^"in: "^line)
                                                       | "call"     -> Ok (Some (Cmd.Call (arg1,num)))
                                                       | "push"     -> (match push arg1 num with
                                                                        | Some i -> Ok (Some i)
                                                                        | None   -> Error (n,"Invalid Command: "^line))
                                                       | "pop"      -> (match pop arg1 num with
                                                                        | Some i -> Ok (Some i)
                                                                        | None   -> Error (n,"Invalid Command: "^line))
                                                       | _          -> Error (n,"Invalid command"^line))
                                         | _        -> Error (n,"Invalid index "^arg2^"in: "^line)

(*Function for parsing a single line*)
        let p_line stripped line n = let parts =  String.split_on_char ' ' stripped 
                                               |> List.filter (fun s -> s<>"") in (*Removes unwanted spaces*)
                                    match parts with
                                    | [cmd]           -> no_arg cmd line n
                                    | [cmd;arg1]      -> one_arg cmd arg1 line n
                                    | [cmd;arg1;arg2] -> two_arg cmd arg1 arg2 line n
                                    | _               -> Error (n,"Invalid Command: "^line)

(*Gets a string and gives a list of its lines*)
        let get_lines s = String.split_on_char '\n' s

(*Numbers the lines*)
        let numbered = List.mapi (fun i line -> (i+1,line))

(*Segregates the output of parsing, separating Ok and Error lines into separate lists*)
        let partition_result = let helper = (fun f -> match f with
                                                      | Ok i    -> Either.Left i
                                                      | Error e -> Either.Right e) in
                               List.partition_map helper 

(*Gives the vm_ast as list of Commands from string of file contents as input*)
        let get_vm s = let lines = get_lines s in
                       let numbered = numbered lines in
                       let res = List.map (fun (i,line)-> let s_line = strip_comment line in
                                                          if s_line <> "" then 
                                                                p_line s_line line i
                                                          else Ok None ) numbered in
                       let ok,err = partition_result res in
                       let filtered_ok = List.filter_map (fun opt -> opt ) ok in
                       match err with
                       | [] -> filtered_ok
                       | _  -> let e_str = map_error err in
                               raise (ParseError e_str)

(*Functions used to get vm ast's program type from string of file contents as input*)
        let get_vm_prog_h (cur_block, res) x = match x with
                                                | Cmd.Function (s,_) -> let name, blk = cur_block in
                                                                        ((s, [x]), (name, List.rev blk)::res)
                                                | _                  -> let name, blk = cur_block in
                                                                        ((name, (x::blk)), res)

(*Gets list of vm commands and groups them as functions*)
        let get_vm_prog s =  let xs = get_vm s in
                             let last_blk, res = List.fold_left get_vm_prog_h (("Bootstrap", []),[]) xs in 
                             let name, blk = last_blk in
                             let prog = List.rev ((name,List.rev blk)::res) in
                             List.iter (fun (name, blk) ->
                                if name = "Bootstrap" && blk <> [] then
                                        raise (ParseError ("Instructions before first function declaration not allowed\n" ))) prog;
                             List.filter (fun (name,_) -> name<>"Bootstrap") prog


end
