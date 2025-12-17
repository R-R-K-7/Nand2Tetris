(*Translator for VM to Assembly*)
module Inst = Assembler.Ast.Inst
module Reg  = Assembler.Ast.Reg
module Mne  = Assembler.Ast.Mnemonics
module Seg  = Ast.Segment
module Cmd  = Ast.Commands
module LabelSet = Set.Make(String)

exception TranslateError of string

(*Type to cover Labels and A and C instructions of asm*)
type ('a,'b) cover_inst = AC of 'a Inst.inst
                        | L of 'b

(*Used to capture different types of labels used in asm*)
type ('f,'l) address = Function of 'f    (*Used in function declarations*)
                     | Label of 'l       
                     | Segment of Seg.t  (*For referring to segments*)
                     | Constant of int

(*To capture different types of Labels used -> created by user, and generated*)
type label = User of string
           | Gen of string*int  (*Generated label*)

module Map_address = struct
(*functions to map 'address'type to string*)
        let map_label = function
                       | User s     -> "USER$"^s
                       | Gen  (s,i) -> "GENERATED$"^s^(string_of_int i)

        let map_segment = function
                        | Seg.ARG  -> "ARG"
                        | Seg.LCL  -> "LCL"
                        | Seg.SP   -> "SP"
                        | Seg.THIS -> "THIS"
                        | Seg.THAT -> "THAT"
                        | Seg.TEMP -> "R5"

        let map_to_string = function
                           |Function f -> "FUNCTION$"^f
                           |Label    l -> map_label l
                           |Segment  s -> map_segment s
                           |Constant i -> string_of_int i
end

module Arithmetic = struct

(*common instructions for binary instructions*)
        let general_binary =     [AC (Inst.A (Segment Seg.SP));
                                  AC (Mne.assign [Reg.A;Reg.M] (Mne.pred Reg.M));
                                  AC (Mne.assign [Reg.D] (Mne.id Reg.M));
                                  AC (Mne.assign [Reg.A] (Mne.pred Reg.A));
                                 ]

        let neg = [AC (Mne.assign [Reg.M] (Mne.uminus Reg.M));]

        let not = [AC (Mne.assign [Reg.M] (Mne.bneg Reg.M))]

        let add = [AC (Mne.assign [Reg.M] (Mne.add Reg.M))]
        
        let sub = [AC (Mne.assign [Reg.M] (Mne.subfrom Reg.M))]   

        let band = [AC (Mne.assign [Reg.M] (Mne.band Reg.M));]

        let bor =  [AC (Mne.assign [Reg.M] (Mne.bor Reg.M));]

(*useful helpers*)
        let set_A_to_stacktop = [AC (Inst.A (Segment Seg.SP));AC (Mne.assign [Reg.A] (Mne.pred Reg.M))]

        let set_stacktop_false = set_A_to_stacktop @ [AC (Mne.assign [Reg.M] Mne.zero)]

        let set_stacktop_true = set_A_to_stacktop @ [AC (Mne.assign [Reg.M] Mne.minusone)]

(*common instructions for eq, gt, lt*)
        let compare op jmp_inst label curr_func num = let iftrue = Label (Gen (curr_func^"$"^"IF_"^label,num)) in
                                             let endl = Label (Gen (curr_func^"$"^"END_"^label,num)) in
                                            [AC (Mne.assign [Reg.D] (op Reg.M));
                                             AC (Inst.A iftrue);
                                             jmp_inst;
                                            ]@set_stacktop_false@
                                            [AC (Inst.A endl);
                                             AC (Mne.jmp);
                                             L (iftrue);
                                            ]@set_stacktop_true@
                                            [L (endl)]

        let eq curr_func num = compare (Mne.sub) (AC Mne.jeq) "EQ" curr_func num
                        
        let gt curr_func num = compare (Mne.subfrom) (AC Mne.jgt) "GT" curr_func num  

        let lt curr_func num = compare (Mne.subfrom) (AC Mne.jlt) "LT" curr_func num 

(*Translates arithmetic instructions*)
        let translate curr_func n = function
                        | Cmd.Not -> set_A_to_stacktop@not
                        | Cmd.Neg -> set_A_to_stacktop@neg 
                        | Cmd.Add -> general_binary@add
                        | Cmd.Sub -> general_binary@sub
                        | Cmd.Eq  -> general_binary@(eq curr_func n)
                        | Cmd.Gt  -> general_binary@(gt curr_func n)
                        | Cmd.Lt  -> general_binary@(lt curr_func n)
                        | Cmd.And -> general_binary@band
                        | Cmd.Or  -> general_binary@bor


end

module Stack_inst = struct

(*loads constant into D*)
        let load_const_D const = [AC (Inst.A const);
                                  AC (Mne.assign [Reg.D] (Mne.id Reg.A))
                                 ]

(*Assigns a given addres, the value stored in D*)
        let assign_D_seg addrs = [AC (Inst.A addrs);
                                  AC (Mne.assign [Reg.M] (Mne.id Reg.D))
                                 ]

(*For pushing into stack value stored in D*)
        let pushD_incSP = [ AC (Inst.A (Segment Seg.SP));
                            AC (Mne.assign [Reg.A] (Mne.id Reg.M));
                            AC (Mne.assign [Reg.M] (Mne.id Reg.D));
                            AC (Inst.A (Segment Seg.SP));
                            AC (Mne.assign [Reg.M] (Mne.succ Reg.M))
                          ]

(*To pop stacktop into D*)
        let popD_decSP = [AC (Inst.A (Segment Seg.SP));
                          AC (Mne.assign [Reg.A;Reg.M] (Mne.pred Reg.M));
                          AC (Mne.assign [Reg.D] (Mne.id Reg.M));
                          ]

(*Pushing static variable into stack*)
        let push_static file i = [AC (Inst.A (Label (Gen (file,i))));
                                  AC (Mne.assign [Reg.D] (Mne.id Reg.M));
                                ]@pushD_incSP

(*Pop stacktop into static segment*)  
        let pop_static file i = popD_decSP@ [AC (Inst.A (Label (Gen (file,i))));
                                             AC (Mne.assign [Reg.M] (Mne.id Reg.D))
                                            ]

(*Common instructions for pushing various segments*)
        let push_addr_or_mem ismem seg i = let reg = if ismem==1 then Reg.M else Reg.A in
                                [AC (Inst.A (Constant i));
                                 AC (Mne.assign [Reg.D] (Mne.id Reg.A));
                                 AC (Inst.A seg);
                                 AC (Mne.assign [Reg.A] (Mne.add reg));
                                 AC (Mne.assign [Reg.D] (Mne.id Reg.M));
                                ]@pushD_incSP

        let push_addr seg i = push_addr_or_mem 0 seg i

        let push_mem seg i = push_addr_or_mem 1 seg i

(*Common instructions to pop various segments*)
        let pop_addr_or_mem ismem seg i = let reg = if ismem==1 then Reg.M else Reg.A in
                                    [AC (Inst.A (Constant i));
                                     AC (Mne.assign [Reg.D] (Mne.id Reg.A));
                                     AC (Inst.A seg);
                                     AC (Mne.assign [Reg.D] (Mne.add reg));
                                     AC (Inst.A (Constant 13));
                                     AC (Mne.assign [Reg.M] (Mne.id Reg.D))]
                                     @popD_decSP@
                                    [AC (Inst.A (Constant 13));
                                     AC (Mne.assign [Reg.A] (Mne.id Reg.M));
                                     AC (Mne.assign [Reg.M] (Mne.id Reg.D))
                                     ]
        let pop_addr seg i = pop_addr_or_mem 0 seg i

        let pop_mem seg i= pop_addr_or_mem 1 seg i

(*Push the value stored in ptr*)
        let push_ptr ptr = [AC (Inst.A ptr);
                            AC (Mne.assign [Reg.D] (Mne.id Reg.M));
                            ]@pushD_incSP

(*Pop the stacktop into ptr*)
        let pop_ptr ptr = popD_decSP@
                         [AC (Inst.A ptr); AC (Mne.assign [Reg.M] (Mne.id Reg.D))]

(*Pushing constant*)
        let push_const n = [AC (Inst.A (Constant n));
                            AC (Mne.assign [Reg.D] (Mne.id Reg.A));
                           ]@pushD_incSP

(*Push/ Pop this that registers*)
        let push_thisthat seg i = match i with
                                | -1 -> push_ptr (Segment seg) (* -1 implies the command was push ptr 0/1 *)
                                | n  -> push_mem (Segment seg) n

        let push seg i = match seg with
                                | Seg.LCL | Seg.ARG     -> push_mem (Segment seg) i
                                | Seg.THIS | Seg.THAT   -> push_thisthat seg i
                                | Seg.TEMP              -> push_addr (Segment seg) i
                                | Seg.SP                -> raise (TranslateError ("Cannot push Stack Pointer")) (*This condition will never be met as 
                                                                                                                  the parser takes care of it *)

        let pop_thisthat seg i = match i with
                                | -1 -> pop_ptr (Segment seg) (* -1 implies command was pop ptr 0/1 *)
                                | n  -> pop_mem (Segment seg) n

        let pop seg i = match seg with
                                | Seg.LCL | Seg.ARG     -> pop_mem (Segment seg) i
                                | Seg.THIS | Seg.THAT   -> pop_thisthat seg i
                                | Seg.TEMP              -> pop_addr (Segment seg) i
                                | Seg.SP                -> raise (TranslateError ("Cannot pop Stack Pointer")) (*This condition will never be met as 
                                                                                                                  the parser takes care of it *)


end

module Branching = struct
        let goto l = [AC (Inst.A (Label (User l)));
                      AC (Mne.jmp)
                     ]

        let ifgoto l = Stack_inst.popD_decSP@[AC (Inst.A (Label (User l)));
                                   AC (Mne.jne);
                                   ]
end

module Function = struct 
(*For pushing return address into stack upoon function call*) 
        let ret_address f i  = [AC (Inst.A (Label (Gen (f^"$ret.", i))));
                                AC (Mne.assign [Reg.D] (Mne.id Reg.A));
                              ]@Stack_inst.pushD_incSP

(*Updating LCL, ARG at function call, after execution of function*)
        let update_lcl_arg nArgs = [AC (Inst.A (Segment Seg.SP));
                                    AC (Mne.assign [Reg.D] (Mne.id Reg.M));
                                    AC (Inst.A (Constant nArgs));
                                    AC (Mne.assign [Reg.D] (Mne.sub Reg.A));
                                    AC (Inst.A (Constant 5));
                                    AC (Mne.assign [Reg.D] (Mne.sub Reg.A))]
                                    @ (Stack_inst.assign_D_seg (Segment Seg.ARG))@ 
                                   [AC (Inst.A (Segment Seg.SP));
                                    AC (Mne.assign [Reg.D] (Mne.id Reg.M))]
                                   @ (Stack_inst.assign_D_seg (Segment Seg.LCL))

(*Instructions to be implemented upon function call*)
        let call f nArgs i  = (ret_address f i)@
                              (Stack_inst.push_ptr (Segment Seg.LCL))@
                              (Stack_inst.push_ptr (Segment Seg.ARG))@
                              (Stack_inst.push_ptr (Segment Seg.THIS))@
                              (Stack_inst.push_ptr (Segment Seg.THAT))@
                              (update_lcl_arg nArgs)@
                              [AC (Inst.A (Function f));
                               AC (Mne.jmp);
                               L (Label (Gen ((f^"$ret."), i)))]

(*Retrieves the caller's segment details back after callee's execution*)
         let frame_minus_n n frame dest = (Stack_inst.load_const_D (Constant n))@
                                 [AC ((Inst.A frame));
                                  AC (Mne.assign [Reg.A] (Mne.subfrom Reg.M));
                                  AC (Mne.assign [Reg.D] (Mne.id Reg.M));
                                  AC (Inst.A dest);
                                  AC (Mne.assign [Reg.M] (Mne.id Reg.D))                                  
                                 ]

         let return = [AC (Inst.A (Segment Seg.LCL));(* pop_mem uses R13 so changing R13,14 used here to R14,15 to prevent overwriting  *)
                       AC (Mne.assign [Reg.D] (Mne.id Reg.M))]@
                      (Stack_inst.assign_D_seg (Constant 14))@
                      (frame_minus_n 5 (Constant 14) (Constant 15))@
                      (Stack_inst.pop_mem (Segment Seg.ARG) 0)@
                      (Stack_inst.load_const_D (Constant 1))@
                      [AC (Inst.A (Segment Seg.ARG));
                       AC (Mne.assign [Reg.D] (Mne.add Reg.M))]@
                      (Stack_inst.assign_D_seg (Segment Seg.SP))@
                      (frame_minus_n 1 (Constant 14) (Segment Seg.THAT))@
                      (frame_minus_n 2 (Constant 14) (Segment Seg.THIS))@
                      (frame_minus_n 3 (Constant 14) (Segment Seg.ARG))@
                      (frame_minus_n 4 (Constant 14) (Segment Seg.LCL))@
                      [AC (Inst.A (Constant 15));
                       AC (Mne.assign [Reg.A] (Mne.id Reg.M));
                       AC (Mne.jmp)
                      ]

(*Upon function definition, creates local varables*)
        let definition f nVars = let push_zero = Stack_inst.push_const 0 in
                                 let rec helper n acc = if n>0 then helper (n-1) (push_zero :: acc)
                                                        else acc in
                                (L f) :: (List.flatten (helper nVars []))

end

module Init = struct

(*To ba added at the beginning of the vm program*)
(*Initializes the addresses of segments*)
        let init = (Stack_inst.load_const_D (Constant 256))@(Stack_inst.assign_D_seg (Segment Seg.SP))@ 
                   (Stack_inst.load_const_D (Constant 300))@(Stack_inst.assign_D_seg (Segment Seg.LCL))@           
                   (Stack_inst.load_const_D (Constant 400))@(Stack_inst.assign_D_seg (Segment Seg.ARG))@
                   (Stack_inst.load_const_D (Constant 3000))@(Stack_inst.assign_D_seg (Segment Seg.THIS))@   
                   (Stack_inst.load_const_D (Constant 3010))@(Stack_inst.assign_D_seg (Segment Seg.THAT))@  
                   (Function.call "Sys.init" 0 0)
end

module Check = struct
(*For checking duplicate label declarations and making a set of all labels defined*)
        let check_lbl_def blk = let set = LabelSet.empty in
                                          let helper (set, e) = function
                                                                | Cmd.Label s -> if LabelSet.mem s set then
                                                                                    let new_e =e^("Duplicate Label Definition: "^s^"\n") in
                                                                                    set, new_e
                                                                                 else (LabelSet.add s set),e
                                                                | _           -> set, e
                                                                in
                                          List.fold_left helper (set, "") blk

(*To make sure no undefined labels are being referred to*)
        let check_label_use fblk = let f,blk = fblk in
                                           let set,e = check_lbl_def blk in
                                           let helper s = function
                                                          | Cmd.Goto i | Cmd.If i -> if LabelSet.mem i set then
                                                                                           s else
                                                                                           s^"Undefined Label: "^i^" in "^f^"\n"
                                                          | _                     ->  s in
                                           let s = List.fold_left helper "" blk in
                                           let all_errors = e^s in
                                           all_errors  

(*To check label's use in entire program of vm*)
        let check_labels_prog prog = let e = List.fold_left (fun acc fblk -> acc^(check_label_use fblk)) "" prog in
                                     if e<>"" then raise (TranslateError e)
                                     else ()

(*To make sure no function call refers to undefined functions*)

(*Helper for checking function calls within a single block of instructions*)
        let function_call_h blk fun_set = let helper e = function
                                                        | Cmd.Call (f,_) -> if LabelSet.mem f fun_set then
                                                                                    e
                                                                            else let new_e = e^("Undefined function: "^f^"\n") in
                                                                                    new_e
                                                        | _              -> e
                                                        in
                                         List.fold_left helper "" blk
(*Checking function calls for the entire vm program*)
        let function_call set prog = List.fold_left (fun acc (_,blk) -> acc^(function_call_h blk set)) "" prog
                                               
(*Collects all function definitions into a set
  Used for the entire vm code across all files combined*)        
        let function_set (set,e) prog = let fun_set,errors = List.fold_left (fun (acc_set,err) xs ->
                                                             let f,_ = xs in 
                                                             if LabelSet.mem f acc_set then 
                                                                (acc_set, err^("Duplicate Function: "^f^"\n"))  
                                                             else (LabelSet.add f acc_set, err))  (set,e) prog
                                                             in
                                        if LabelSet.mem "Sys.init" fun_set then
                                                (fun_set,errors)
                                        else let new_errors = errors^"Undefined function: Sys.init not defined\n" in
                                                (fun_set,new_errors)


end

module Program = struct
  
(*Translating a single vm line into asm instructions*)     
        let tline counter file curr_func= function
                        | Cmd.Arithmetic op  -> Arithmetic.translate curr_func counter op
                        | Cmd.Push (seg,i)   -> Stack_inst.push seg i
                        | Cmd.Pop (seg,i)    -> Stack_inst.pop seg i
                        | Cmd.PushStatic n   -> Stack_inst.push_static file n
                        | Cmd.PopStatic n    -> Stack_inst.pop_static file n
                        | Cmd.PushConst n    -> Stack_inst.push_const n
                        | Cmd.Label s        -> [L (Label (User (curr_func^"$"^s)))]
                        | Cmd.Goto s         -> Branching.goto (curr_func^"$"^s)
                        | Cmd.If s           -> Branching.ifgoto (curr_func^"$"^s)
                        | Cmd.Return         -> Function.return
                        | Cmd.Call (f,i)     -> Function.call f i counter
                        | Cmd.Function (s,i) -> Function.definition (Function s) i

(*Translating a function block into assembly instructions*)
        let t_function file n fblk = let f,blk = fblk in
                                             let helper (n, res) x = 
                                                let translated_line = tline (n+1) file f x in
                                                (n+1, translated_line::res) in
                                             let k, res = List.fold_left helper (n,[]) blk in
                                             k, List.flatten (List.rev res)

(*Gives list of asm instructions from vm program*)                                             
        let getasm prog file = let _ = Check.check_labels_prog prog in
                               let helper (curr_n,res) fblk = 
                                       let (next_n, new_blk) = t_function file curr_n fblk in
                                       (next_n, new_blk::res) in
                               let _, res = List.fold_left helper (0,[]) prog in
                               List.flatten (List.rev res)
        
(*Functions to get asm program type from list of asm instructions*)
        let get_prog_h (isprmbl,prmbl,bdy,cur_label,cur_blk,err) = function
                                                             | L l -> (if isprmbl then
                                                                                   (false,List.rev cur_blk,bdy,l,[],err)
                                                                           else 
                                                                                   (isprmbl,prmbl,(cur_label,List.rev cur_blk)::bdy,l,[],err))
                                                             | AC i -> match i with
                                                                     | Inst.C c -> (isprmbl,prmbl,bdy,cur_label,(Inst.C c)::cur_blk,err)
                                                                     | Inst.A a -> (isprmbl,prmbl,bdy,cur_label,(Inst.A a)::cur_blk,err)
                                                                     
        let get_prog lines = let init = (true,[],[],Label(Gen("Preamble",-1)),[],[]) in
                             Assembler.Parser.Parse.get_program_gen get_prog_h init lines
end
