(** the abstract syntax tree.

*)
(* Destination = output expression; Jump

*)

(*To capture different types of registers*)

module Reg = struct
	type reg =        A
        	        | M
                	| D
end

(*To accumulate errors in the AST*)

module Result = struct

	type ('a,'e) t =    Value of 'a  (* Value 'a = computation result , List 'e = errors  *)
			    | List of ('e list)

(*maps a function f to the input argument, accumulating errors if an error is present*)

	let map f = function
		| Value x ->  Value (f x)
		| List xs -> List xs

(*Maps a function f that takes two arguments ta,tb and applies f on the lists, accumulating errors if present*)

	let map2 f ta tb = match ta, tb with
		| Value x, Value y -> Value (f x y)
		| List xs, List ys -> List (xs@ys)
		| _, List ys -> List ys
		| List xs, _ -> List xs
(*
Folds a list of 'Result.t' values, 
if all elements are (Value x), it returns (Value [x1; x2; ...]).
if any one element is (List xs), it discards all
(Value elements), and returns (List [xs1 @ xs2 @ ...]), i.e. 
a single List containing the concatenation of all errors.
 *)
	let collect tlist  = let combine ta tblist = match ta, tblist with
					| Value x, Value ys  -> Value (x::ys)
					| List xs, List ys -> List (xs@ys)
					| _, List ys -> List ys
					| List xs, _ -> List xs
					in
					List.fold_right combine  tlist (Value []) 

end

(*Function to convert Option type to Result type*)

module Option = struct

	let from e oa = match oa with
		| Some x -> Result.Value x
		| _ -> Result.List [e]

	let app f u = from u (f u) 

end

module Inst = struct

(*Jump Instructions*)

	type jinst = JGT
	    	| JEQ
	    	| JGE
	     	| JLT
	     	| JNE
	     	| JLE
	     	| JMP

(*Different types of Operations- Constant, Unary, Binary*)

	type const = Zero | One | MinusOne

	type unary =    | BNeg
                	| UMinus
                	| Succ
                	| Pred
                	| ID

	type binary =   | Add
                	| Sub
                	| SubFrom
                	| BAnd
                	| BOr

(*output type captures the computation part of C instruction *)

	type output =   Constant of const
			| Uapply of unary*Reg.reg
			| Bapply of binary*Reg.reg

(*Type to capture C instruction as a record type, with fields 
  destination(for mentioning targeted registers), output(for computation), jump(for jump instructions)*)

	type cinst = { destination :(Reg.reg list) option;
	       	output	   : output option;
	       	jump  	   : jinst option
		     }

(*Two types of instructions - A inst, C inst*)

	type 'v inst = A of 'v
		     | C of cinst

(*Map A inst by applying a function f*)

	let map f = function
	    	| A i -> A (f i)
	    	| C i -> C i

(*Resolve A inst by applying a function f that returns Option type*)

	let resolve f = function
		| A i ->(match f i with
			| Some x -> Result.Value (A x)
			| None -> Result.List [i])
                | C i -> Result.Value (C i)

end

module Block = struct

(*Block = list of intsructions*)

	type 'v block = 'v Inst.inst list

(*Function to get number of instructions in a block*)
	
	let wordsize block = List.fold_right (fun _ x -> x+1 ) block 0 

	let map f block = List.map  (Inst.map f) block

	let resolve f block = Result.collect ( List.map (Inst.resolve f)  block ) 

end

module Labeled = struct 
(*labeled block = (label_name, list of asm instructions till next label definition)*)
	type 'v lblock = 'v  * 'v Block.block

	let map f lblock = let (xu,iu) = lblock in
				( f xu, Block.map f iu )

	let resolve f (xu,iu) = let pair x y = (x, y) in 
			Result.map2 pair (Option.app f xu ) (Block.resolve f iu)
end


module Symbol_table = struct
(*Forms a list of tuples containing variable name and its address for all built in variables*)
(*Registers, SP, LCL, etc*)
	let registers = List.init 16 (fun i -> ("R"^string_of_int i,i))
	
	let segments = [("SP",0);
			("LCL",1);
			("ARG",2);
			("THIS",3);
			("THAT",4);
			("SCREEN",16384);
			("KBD",24576)	
			]

	let def_table = registers@segments

end

module Program = struct 
(*Program = a preamble which has no label definitions, and a body which has a list of labeled blocks*)
	type 'v prog = { preamble : 'v Block.block;
			 body 	 : 'v Labeled.lblock list 
		       }

	let map f pgm = { 
			  preamble = Block.map f pgm.preamble;
			  body	   = List.map  (Labeled.map f) pgm.body 
		   	}

	let resolve f pgm = let makeprog prmbl bdy = {preamble=prmbl;body=bdy} in
			    let	prmbl = Block.resolve f pgm.preamble in
			    let bdy   = Result.collect (List.map (Labeled.resolve f) pgm.body) in
			    Result.map2 makeprog prmbl bdy

(*Functions for collecting all the labels definitions and their offsets from the beginnig of the program*)
	let labeldefcombine acc lblk =  let (ls,offset)=acc in
					let (label,blk)=lblk in
					(label,offset)::ls,offset+(Block.wordsize blk)

	let address pgm = let prmbloffset = Block.wordsize pgm.preamble in 
			  let (symbol_table,_) = List.fold_left labeldefcombine (Symbol_table.def_table,prmbloffset) pgm.body in
                          symbol_table        

end

module Mnemonics = struct

(* mnemonics for some common instructions*)
	let jumpOf j = match j with
		| Inst.JMP -> Inst.C {destination=(Some []);output=Some (Inst.Constant Inst.Zero);jump=(Some JMP);}
		| _   -> Inst.C {destination=(Some []);output=Some (Inst.Uapply (ID,Reg.D));jump=(Some j);}

	let jgt = jumpOf Inst.JGT
	let jeq = jumpOf Inst.JEQ
	let jge = jumpOf Inst.JGE
	let jlt = jumpOf Inst.JLT
	let jne = jumpOf Inst.JNE
	let jle = jumpOf Inst.JLE
	let jmp = jumpOf Inst.JMP

	let add x = Inst.Bapply (Inst.Add,x)
	let sub x = Inst.Bapply (Inst.Sub,x)
        let subfrom x = Inst.Bapply (Inst.SubFrom,x)
        let band x = Inst.Bapply (Inst.BAnd,x)
        let bor x = Inst.Bapply (Inst.BOr,x)
        let uminus x = Inst.Uapply (Inst.UMinus,x)
        let bneg x = Inst.Uapply (Inst.BNeg,x)
        let succ x = Inst.Uapply (Inst.Succ,x)
        let pred x = Inst.Uapply (Inst.Pred,x)
        let id x = Inst.Uapply (Inst.ID,x)
        let zero = Inst.Constant Inst.Zero
        let one = Inst.Constant Inst.One
        let minusone = Inst.Constant Inst.MinusOne
        
	let assign reglist op = Inst.C {destination=Some reglist;output= Some op;jump=None;}


end

module PrettyPrint = struct

(*Assumes all Instructions in given AST are valid*)

(*This module requires 'v in ('v program) to be of type string *)

(*Functions for converting each type of commands into coreesponding strings*)
        
        let ainst a = "@"^a

        let reg = function
                      |Reg.A -> "A"
                      |Reg.D -> "D"
                      |Reg.M -> "M"

        let reglist reglist = String.concat "" (List.map reg reglist)

        let const = function
                   | Inst.MinusOne -> "-1"
                   | Inst.One      -> "1"
                   | Inst.Zero     -> "0"

        let unary = function
                   | Inst.BNeg   -> "!"
                   | Inst.UMinus -> "-"
                   | Inst.Succ   -> "+1"
                   | Inst.Pred   -> "-1"
                   | Inst.ID     -> ""

        let binary = function
                   | Inst.Add     -> "+"
                   | Inst.Sub     -> "-"
                   | Inst.SubFrom -> "-"
                   | Inst.BAnd    -> "&"
                   | Inst.BOr     -> "|"

        let uapply op r = match op with
                        | Inst.BNeg | Inst.UMinus -> (unary op)^(reg r) 
                        | Inst.Succ | Inst.Pred   -> (reg r)^(unary op)
                        | Inst.ID                 -> reg r

        let bapply op r = match op with
                        | Inst.SubFrom -> (reg r)^(binary op)^"D"
                        | _            -> "D"^(binary op)^(reg r)

        let output = function
                    | Inst.Constant c  -> const c
                    | Inst.Uapply (op,r) -> uapply op r
                    | Inst.Bapply (op,r) -> bapply op r

        let jinst = function
                | Inst.JGT -> "JGT"
                | Inst.JEQ -> "JEQ"
                | Inst.JGE -> "JGE"
                | Inst.JLT -> "JLT"
                | Inst.JNE -> "JNE"
                | Inst.JLE -> "JLE"
                | Inst.JMP -> "JMP"

        let match_option f = function
                               | Some y -> f y
                               | None   -> ""

        let match_empty post str x = match x with (*post determines whether to postfix or prefix*)
                                    |"" -> x
                                    |_  -> if post==1 then x^str else str^x

        let cinst (c:Inst.cinst) = let dest = c.destination in
                      let out = c.output in
                      let j = c.jump in
                      let s_dest = match_empty 1 "=" (match_option reglist dest) in
                      let s_compt = match_option output out in
                      let s_jmp = match_empty 0 ";" (match_option jinst j) in
                      s_dest^s_compt^s_jmp

        let inst = function
                       |Inst.A a -> ainst a
                       |Inst.C c -> cinst c

(*Block converted into list of strings*)                       
        let block = List.map inst 

        let dec_label l = "("^l^")"

(*Labeled block converted into list of string*)
        let labeled l = let (i,blk) = l in
                        (dec_label i)::(block blk)

        let body b = List.flatten (List.map labeled b)

(*Whole program converted into a string*)
        let prog (p:'v Program.prog) = let prmbl = p.preamble in
                                       let bdy = p.body in
                                       String.concat "\n" ((block prmbl)@(body bdy))
end
