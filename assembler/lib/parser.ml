(* parser *)
open Ast

module Parse = struct

exception ParseError of string

(*Creating a type to hold various type of instructions that can be parsed*)

type 'v parsed = Label of 'v | C_inst of 'v Inst.inst | A_inst of 'v Inst.inst


(* to remove all spaces *)

let rm_space line = String.concat "" (String.split_on_char ' ' line)

(* to remove comments and strip the spaces *)

let strip_comment line = match String.index_opt line '/' with
                                | Some i -> rm_space (String.sub line 0 i)
                                | None   -> rm_space line

(*Function to strip comments, spaces in between characters, and empty lines*)

let filter_lines x = List.filter (fun s -> s<>"") (List.map strip_comment (String.split_on_char '\n' x))

(*Registers*)

let reg r = match r with
		| "A"  -> Some [Reg.A] 
		| "D"  -> Some [Reg.D]
		| "M"  -> Some [Reg.M]
		| "AD" -> Some [Reg.A;Reg.D]
		| "MD" -> Some [Reg.M;Reg.D]
		| "AM" -> Some [Reg.A;Reg.M]
		| "AMD"-> Some [Reg.A;Reg.M;Reg.D]
		| _    -> None

(*Constants*)

let const = function
		| "0" -> Some (Inst.Constant Inst.Zero)
		| "1" -> Some (Inst.Constant Inst.One)
		| "-1"-> Some (Inst.Constant Inst.MinusOne)
		| _   -> None

(*Unary Operations*)

let unary = function
		| "!A" -> Some (Inst.Uapply (Inst.BNeg,Reg.A))
		| "!D" -> Some (Inst.Uapply (Inst.BNeg,Reg.D))
		| "!M" -> Some (Inst.Uapply (Inst.BNeg,Reg.M))
		| "-A" -> Some (Inst.Uapply (Inst.UMinus,Reg.A))
                | "-D" -> Some (Inst.Uapply (Inst.UMinus,Reg.D))
                | "-M" -> Some (Inst.Uapply (Inst.UMinus,Reg.M))
                | "A+1" -> Some (Inst.Uapply (Inst.Succ,Reg.A))
                | "D+1" -> Some (Inst.Uapply (Inst.Succ,Reg.D))
                | "M+1" -> Some (Inst.Uapply (Inst.Succ,Reg.M))
                | "A-1" -> Some (Inst.Uapply (Inst.Pred,Reg.A))
                | "D-1" -> Some (Inst.Uapply (Inst.Pred,Reg.D))
                | "M-1" -> Some (Inst.Uapply (Inst.Pred,Reg.M))
                | "A" -> Some (Inst.Uapply (Inst.ID,Reg.A))
                | "D" -> Some (Inst.Uapply (Inst.ID,Reg.D))
                | "M" -> Some (Inst.Uapply (Inst.ID,Reg.M))
		| _   -> None

(*Binary Operations*)

let binary = function
		| "D+M" -> Some (Inst.Bapply (Inst.Add,M))
                | "D+A" -> Some (Inst.Bapply (Inst.Add,A))
                | "D-M" -> Some (Inst.Bapply (Inst.Sub,M))
                | "D-A" -> Some (Inst.Bapply (Inst.Sub,A))
                | "M-D" -> Some (Inst.Bapply (Inst.SubFrom,M))
                | "A-D" -> Some (Inst.Bapply (Inst.SubFrom,A))
                | "D&M" -> Some (Inst.Bapply (Inst.BAnd,M))
                | "D&A" -> Some (Inst.Bapply (Inst.BAnd,A))
                | "D|M" -> Some (Inst.Bapply (Inst.BOr,M))
                | "D|A" -> Some (Inst.Bapply (Inst.BOr,A))
		| _ 	-> None

(*Jump Instructions*)

let jmp j = match j with
		| "JMP" -> Some Inst.JMP
		| "JEQ" -> Some Inst.JEQ
		| "JGT" -> Some Inst.JGT
		| "JLT" -> Some Inst.JLT
		| "JGE" -> Some Inst.JGE
		| "JLE" -> Some Inst.JLE
		| "JNE" -> Some Inst.JNE
		| _     -> None

(*Parsing the output part of C-instruction*)

let output_h ops line = List.find_map (fun f -> f line) ops

let output comp = output_h [const;unary;binary] comp

(*Parsing a C-Instruction*)
(*C instruction is of form
  dest=output;jmp
  based on this structure, the function parses a C instruction*)

let make_C dest output jmp = C_inst (Inst.C (Inst.{destination = dest;
                                                   output = output;
                                                   jump = jmp}))
let is_none = function
              | None -> true
              | _    -> false

let c_inst line = let split c s = String.split_on_char c s in
                  let helper regl op j = (reg regl,output op,jmp j) in
                  let err = Error ("Invalid Instruction: "^line^"\n") in
                  match (split '=' line) with
                  | [dest_str;other] -> (match (split ';' other) with
                                         | [out_str;jmp_str]-> (let regl,op,j = helper dest_str out_str jmp_str in
                                                                if is_none regl || is_none op || is_none j then
                                                                err else
                                                                Ok (make_C regl op j))
                                         | [out_str] -> (let regl,op,_ = helper dest_str out_str "" in
                                                         if is_none regl || is_none op  then
                                                         err else
                                                         Ok (make_C regl op None))
                                         | _ -> err) 
                  | [other] -> (match (split ';' other) with
                                | [out_str;jmp_str] -> (let _,op,j = helper "" out_str jmp_str in
                                                        if is_none op || is_none j then
                                                        err else
                                                        Ok (make_C None op j))
                                | _ -> err)
                  | _ -> err

(*Checking validity of Variable name and Label name*)
(*Allowed characters are alphabets, digits, ':', '$', '.', '_'.
  But should not start with digits.
 *)
let check_char = function
                        |'a'..'z'
                        |'A'..'Z'
                        |'0'..'9'
                        | '$' | '.' | '_' | ':' -> true
                        | _ -> false

let is_not_digit = function
                          |'0'..'9' -> false
                          |_    -> true

let check_var_name s = match int_of_string_opt s with
                       | Some _ -> true
                       | None   -> (String.for_all check_char s) && (is_not_digit s.[0]) (*Check only when A instruction defines variable or label, not integer*)

(*Function to parse a single line*)

let pline line = if (String.contains line '(' && String.contains line ')') then(   (*if a line has ( and ) then its a label declaration*)
                        let var = String.sub line 1 ((String.length line)-2) in
                        if check_var_name var then 
                                Ok (Label var)
                        else 
                                Error ("Invalid Label name: "^var^" in "^line^"\n"))
		 else if (String.starts_with ~prefix:"@" line) then(               (*if a line starts with @ then its A instruction*)
                        let var_name = String.sub line 1 (String.length line - 1) in
                        if check_var_name var_name  then
                                Ok (A_inst (Inst.A var_name))
                        else 
                                Error ("Invalid Variable / Label name: "^var_name^" in "^line^"\n"))
		 else
		        c_inst line

(*Function to create program datatype belonging to AST to assembly*)

let get_prog_h (isprmbl,prmbl,bdy,cur_label,cur_blk,errors) line = 
                                        match pline line with
					| Ok (Label l)  -> if isprmbl then
							(false,List.rev cur_blk,[],l,[],errors) 
						     else 
							(false,prmbl,(cur_label,List.rev cur_blk)::bdy,l,[],errors)
					| Ok (C_inst c) -> (isprmbl,prmbl,bdy,cur_label,c::cur_blk,errors)
					| Ok (A_inst a) -> (isprmbl,prmbl,bdy,cur_label,a::cur_blk,errors)
                                        | Error e       -> (isprmbl,prmbl,bdy,cur_label,cur_blk,e::errors) 

(*General function for getting program*)       
(*Checks if errors are present and raises them appropraitely*)
let get_program_gen helper init lines = let (isprmbl,prmbl,bdy,final_label,last_insts,errors) = List.fold_left helper init lines in
                                        match errors with 
                                        | [] ->(
		                                if isprmbl then
			                                Program.{preamble = List.rev last_insts;body=[]}
		                                else 
				                        let last_blk = (final_label,List.rev last_insts) in
					                Program.{preamble = prmbl ; body = List.rev (last_blk::bdy) })
                                        | _  -> let err_str = String.concat "" (List.rev errors) in
                                                raise (ParseError err_str)

(*Gets asm program type*)    
let get_program s = let init = (true,[],[],"Preamble",[],[]) in
                    let lines = filter_lines s in
                    get_program_gen get_prog_h init lines

end
