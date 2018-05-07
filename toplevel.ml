(*top level program, parses the AST and prints an error message (generated
automatically) if the program is invalid or a success message if the input
is valid
Adapted from microc/class slides
Contributors: Caroline Roig-Irwin clr2176*)

let () =
        (*link in the standard library*)
        let temp_file = Sys.argv.(1) ^ "i" in (*!olleh intermediate file*)
        let temp_out = open_out temp_file in
        let stdlib = open_in "standard-lib.olh" in (*add stdlib to file*)
          try while true; do
            let line = input_line stdlib in
            Printf.fprintf temp_out "%s\n" line;
          done;
        with End_of_file -> close_in stdlib;
        let olhfile = open_in Sys.argv.(1) in (*add user program to file*)
          try while true; do
            let line = input_line olhfile in
            Printf.fprintf temp_out "%s\n" line;
          done;
        with End_of_file -> close_in olhfile;
        close_out temp_out;

        let channel = ref stdin in
        channel := open_in temp_file;
        let lexbuf = Lexing.from_channel !channel in
        let ast = Parser.program Scanner.tokenize lexbuf in
        let sast = Semant.check ast in
        let m = Codegen.translate sast in
    	  Llvm_analysis.assert_valid_module m;
    	  print_string (Llvm.string_of_llmodule m)
