(*top level program, parses the AST and prints an error message (generated
automatically) if the program is invalid or a success message if the input
is valid
Adapted from microc/class slides*)

let () =
        let usage_msg = "usage: ./toplevel.native [file.olh]" in
        let channel = ref stdin in
        Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;
        let lexbuf = Lexing.from_channel !channel in
        (*let _ = print_endline "passed lexer" in*)
        let ast = Parser.program Scanner.tokenize lexbuf in
        (*let _ = print_endline "passed parser" in*)
        let sast = Semant.check ast in
        (*let _ = print_endline "passed semant" in*)
        let m = Codegen.translate sast in
    	  Llvm_analysis.assert_valid_module m;
    	  print_string (Llvm.string_of_llmodule m)

        (*print_endline "valid program!"*)
