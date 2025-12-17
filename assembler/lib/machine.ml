(* To encode the datatypes to sequence of bytes  *)
open Ast
open Inst
open Reg
open Program

(*Converts list of int into string*)
let intl_to_strl xs = List.map Int.to_string xs

(*Converts integer to list of integers of length 15 denoting its binary form*)
let int_to_list n = List.init 15 (fun i -> (n lsr (14-i)) land 1)

module Jump = struct

	let jump = function
		| JGT -> [0;0;1]
		| JEQ -> [0;1;0]
		| JGE -> [0;1;1]
		| JLT -> [1;0;0]
		| JNE -> [1;0;1]
		| JLE -> [1;1;0]
		| JMP -> [1;1;1]

	let encode = function
		| Some j -> jump j
		| _   -> [0;0;0]		

end

module Destination = struct

	let dest = function 
		| A -> [1;0;0]
		| D -> [0;1;0]
		| M -> [0;0;1]

(*Does logical Or operation to determine the destination*)                
	let comb s1 s2 = List.map2 Int.logor s1 s2

	let encode = function
		| Some reglist -> List.fold_left comb [0;0;0] (List.map dest reglist)
		| _ -> [0;0;0]

end

module Constant = struct

(*Functions to encode constants 1 0 -1*)
	let constant = function
		| Zero     -> [0;1;0;1;0;1;0]
		| One      -> [0;1;1;1;1;1;1]
		| MinusOne -> [0;1;1;1;0;1;0]
end

module Unary = struct
(*Functions to encode unary instructions*)
	let defaultreg = function
		| D -> [0;0;0;1;1]
		| A -> [0;1;1;0;0]
		| M -> [1;1;1;0;0]
		
	let defaultunary = function
		| BNeg   ->[0;1]
		| UMinus ->[1;1]
		| Succ   ->[1;1]
		| Pred   ->[1;0]
		| ID     ->[0;0]

	let succ = function
		| D -> [0;0;1;1;1;1;1]
		| A -> [0;1;1;0;1;1;1]
		| M -> [1;1;1;0;1;1;1]

	let encode (op,reg) = match op with
		| Succ     -> succ reg
		| _        -> (defaultreg reg)@(defaultunary op)
end

module Binary = struct

(*Functions to encode binary instructions*)        
	let ambit = function
		| A -> [0]
		| _ -> [1]

	let opbits = function
		| Add -> [0;0;0;0;1;0]
		| Sub -> [0;1;0;0;1;1]
		| SubFrom -> [0;0;0;1;1;1]
		| BAnd -> [0;0;0;0;0;0]
		| BOr -> [0;1;0;1;0;1]

	let encode (op,reg) = (ambit reg)@(opbits op)
end

module Output = struct

(*Encode output part of C instruction*)
	let encode = function
		| Some i ->( match i with
			| Constant c -> Constant.constant c
			| Uapply (op,reg) -> Unary.encode (op,reg)
			| Bapply (op,reg) -> Binary.encode (op,reg))
		| None -> List.init 7 (fun _ -> 0)

end

module Instruction = struct

(*Encode A or C instruction*)
	let encode = function
		   | Inst.A i -> 0::(int_to_list i)
		   | Inst.C inst   ->(let des = Destination.encode inst.destination in
		        	 let com = Output.encode inst.output in
			         let jmp = Jump.encode inst.jump in
				 [1;1;1]@com@des@jmp)
                		

end

module Assemble = struct

(*Encode block and program types of ast into binary strings*)
        let encode_to_list blk = List.map Instruction.encode blk

        let encode_pgm_base2 prog =
                let prmbl = encode_to_list prog.preamble in
                let bdy = List.fold_right (fun (_, blk) acc ->
                                        (encode_to_list blk) @ acc
                                        ) prog.body [] in
                let encprog = prmbl @ bdy in
                encprog
                |> List.map intl_to_strl
                |> List.map (String.concat "")
                |> String.concat "\n"

end
