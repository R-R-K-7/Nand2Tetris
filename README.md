## Assembler

### How to Use

Navigate to ```HackAssembler/assembler``` and run

```dune exec assembler <path_to_input_file.asm> <path_to_output_file.hack>```

* The above command writes the binary strings corresponding to the instructions in input_file into output_file.
* If output_file doesn't exist, it will be created, if exists, will be over written.
* Requires input file to have '.asm' and output file to have '.hack' extensions.

### AST 
```HackAssembler/assembler/lib/ast.ml```

* The types Registers, Instructions, Block(of instructions), Labeled block, Program to capture various types in assembly language.
* Result type to accumulate errors that are not caught by the parser.
* Mnemonics module provides shortcuts for expressing certain C-instructions.
* Symbol_table for initializing all built-in variables including registers, KBD, SCREEN, and segments.
* PrettyPrint module for getting the entire program as a string, assumes no error present in its input. 

### Parser 
```HackAssembler/assembler/lib/parser.ml```

* Gets a string of input file's contents as input, gives output of type Assembler.Ast.Program.prog.
* Part of lines after '//' are considered as comments and removed. Empty lines are removed.
* Allows variable names and labels containing only alphabets, digits, '$', '.', '_', ':' and first character should not be a digit.
* Groups unremoved lines into Label, C_inst, A_inst and creates Assembler.Ast.Program.prog.
* Uses built-in Result type for collecting errors.
* All errors encountered while parsing are collected and raised as an exception (ParseError), including invalid instructions.

### Machine 
```HackAssembler/assembler/lib/machine.ml```

* Gets an Assembler.Ast.Program.prog and converts it into a binary string.
* Converts each instruction into a list of integers.
* These list of integers are concatenated at last and converted into binary string.

## Virtual Machine Translator

### How to Use

Navigate to ```HackAssembler/jackvm``` and run

```dune exec jackvm -- -[a] <path_to_input_file.vm or folder> ```

* The above command creates .asm or .hack files based on given instruction.
* If input is file, output file is created in the parent directory of the input file.
* If input is folder, output file is created inside the folder.
* Requires atleast one file with '.vm' extension in provided folder/file.
* -a flag is optional, if used, creates .asm file, no .hack file.
* If -a flag is not used, .hack file is created, no .asm file.
* Output file is created with base name of the input file/folder with '.hack' or '.asm' extension accordingly.

### AST 
```HackAssembler/jackvm/lib/ast.ml```

* The Arithmetic module captures various kinds of arithmetic operations.
* The Commands module captures all possible commands in Virtual Machine language.
* The Function module groups commands into a tuple of function name and the list of commands till next function definition.
* The Program module groups the entire program into a list of Function.t types.

### Parser
```HackAssembler/jackvm/lib/parser.ml```

* Gets a string of input file's contents as input, gives output of type Jackvm.Ast.Program.t.
* Parts of lines including and after '//' and empty lines are removed.
* Allows function names and labels containing only alphabets, digits, '.', '_', ':' and first character should not be a digit.
* Uses built-in Result type for collecting errors and raising them.
* Collects invalid commands, incorrect indices (e.g. push argument -1) and raises/ returns them appropriately.
* Does not allow any commands before the first function declaration, i.e. all commands should be enclosed within functions.

### Translate 
```HackAssembler/jackvm/lib/translate.ml```

* Translates Jackvm.Ast.Program.t into Assembler.Ast.Program.prog datatype with A instructions having type 'address'.
* Uses types address, label to capture different types of labels used in assembly instructions.
* Map_address for mapping address type into corresponding unique strings.
* Uses type cover_inst to cover A, C instructions and label declarations in assembly code.
* Arithmetic, Stack_inst, Branching, Function modules for converting different vm commands into assembly instructions.
* Init module initializes stack pointer and other segments and calls 'Sys.init'.
* Program module to convert list of assembly instructions into Assembler.Ast.Program.prog datatype.
* Check module for checking for undefined or duplicated (declared more than once) labels and functions.
* By default, always checks for 'Sys.init' function to be defined.
* Check module has functions that collect all errors as a string and returns/ raises them appropriately.
