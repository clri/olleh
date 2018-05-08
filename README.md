The !OLLEH compiler

Amnah Ahmad: aza2111@barnard.edu
Mahika Bhalla: mmb2276@columbia.edu
Caroline Roig-Irwin: clr2176@columbia.edu

Coded in OCaml, this takes !OLLEH, a language made to code word-games and
compiles it into LLVM IR.

It needs the OCaml llvm library, which is most easily installed through opam.

Install LLVM and its development libraries, the m4 macro preprocessor,
and opam, then use opam to install llvm.

The version of the OCaml llvm library must match the version of the LLVM
system installed on your system.

The compiler compiles from hello.olh in the tests folder.  testall.sh runs the OLLEH executable on each testcase (.olh file) to produce a .ll file, invokes
"llc" (the LLVM compiler) and produces a .s (assembly) file, then
invokes "cc" (the stock C compiler) to assemble the .s file, link in
the hello and generate an executable. See testall.sh for details.

Included files:
tests/\*: tests with output they will be measured against
ast.ml: abstract syntax tree, output of the parser
codegen.ml: code generator, converts sast to LLVM code
example.txt: example !OLLEH dictionary used for testing. please do not modify.
Makefile: compiles the compiler
ostdlib.c: builtin functions linked with the !OLLEH compiler.
parser.mly: parser, converts tokens to ast
sast.ml: semantically checked abstract syntax tree, output of semantic checker
scanner.mll: scanner, tokenizes input file
semant.ml: semantic checker, turns ast to sast
standard-lib.olh: !OLLEH standard library that is linked to every file
testall.sh: shell script that runs the tests
olleh.ml: driver program for the compiler that compiles !OLLEH to LLVM.



HOW TO COMPILE AND RUN:
$ make
$ ./testall.sh

If the output of testall.sh does not report any errors, you can go ahead and
compile your own code (assuming your-code.olh is the program you've written).
$ ./olleh your-code.olh > your-code.ll
$ llc your-code.ll > your-code.s
$ cc -o your-code.exe your-code.s ostdlib.o
$./your-code.exe
