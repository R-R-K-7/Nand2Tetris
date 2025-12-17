(*Ast of Virtual Machine Translator*)

module Segment = struct
        type t = ARG
                |LCL
                |THIS
                |THAT
                | SP
                | TEMP
end

module Commands = struct
        type arithmetic_and_logic = Add
                        | Sub
                        | Neg
                        | Eq
                        | Gt
                        | Lt
                        | And
                        | Or
                        | Not

        type t = Arithmetic of arithmetic_and_logic
                |Push of (Segment.t*int)
                |Pop of (Segment.t*int)
                |PushStatic of int
                |PopStatic of int
                |PushConst of int
                |Label of string
                |Goto of string
                |If of string
                |Return
                |Call of (string*int)
                |Function of (string*int)
end

module Function = struct
(*Groups commands into function name and commands till next function definition*)
        type t = string*(Commands.t list)

end

module Program = struct

        type t = Function.t list

end
