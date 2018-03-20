(*top level program, parses the AST and prints an error message (generated
automatically) if the program is invalid or a success message if the input
is valid
Adapted from microc/class slides*)

let () =
        let usage_msg = "usage: ./toplevel.native [file.olh]" in
        let channel = ref stdin in
        Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;
        let lexbuf = Lexing.from_channel !channel in
        let ast = Parser.program Scanner.tokenize lexbuf in
        let _(*sast*) = Semant.check ast in
        print_endline "valid program!"
