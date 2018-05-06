The Olleh compiler

Amnah Ahmad: aza2111@barnard.edu
Mahika Bhalla: mmb2276@columbia.edu
Caroline Roig-Irwin: clr2176@columbia.edu

Coded in OCaml, this takes a language made to code word-games and compiles it into LLVM IR.

It needs the OCaml llvm library, which is most easily installed through opam.

Install LLVM and its development libraries, the m4 macro preprocessor,
and opam, then use opam to install llvm.

The version of the OCaml llvm library must match the version of the LLVM
system installed on your system.

The compiler compiles from hello.olh in the tests folder.  testall.sh runs the olleh executable on each testcase (.olh file) to produce a .ll file, invokes
"llc" (the LLVM compiler) and produces a .s (assembly) file, then
invokes "cc" (the stock C compiler) to assemble the .s file, link in
the hello and generate an executable. See testall.sh for details.

Included files:
tests/\*: tests with output they will be measured against
ast.ml: abstract syntax tree, output of the parser
codegen.ml: code generator, converts sast to LLVM code
example.txt: example !Olleh dictionary used for testing. please do not modify.
Makefile: compiles the compiler
ostdlib.c: builtin functions linked with the !Olleh compiler.
parser.mly: parser, converts tokens to ast
sast.ml: semantically checked abstract syntax tree, output of semantic checker
scanner.mll: scanner, tokenizes input file
semant.ml: semantic checker, turns ast to sast
standard-lib.olh: !Olleh standard library that is linked to every file
testall.sh: shell script that runs the tests
toplevel.ml: driver program for the compiler that compiles !Olleh to LLVM.


* Test Suite--see \*.olh in tests folder (run ./testall.sh to see output).
fail-\*-neg.olh are negative tests; test-\*-pos.olh are positive tests.
@TODO list our tests if this is necessary
1. fail-list-neg.olh: Negative test. Semantic checker does not allow lists of mixed integers and characters.
2. fail-print-neg.olh: Negative test. Semantic checker does not allow concatenation of strings and booleans.
3. fail-subtract-neg.olh: Negative test. Semantic checker does not allow subtraction of two strings.
4. test-add-pos.olh: Positive test. Sums two integers and prints the results (tests addition and printing integers).
5. test-concat-pos.olh: Positive test. Concatenates two strings and prints the result (tests concatenation of strings). MicroC does not process or concatenate strings.
6. test-func-pos.olh: Positive test. Defines a function that prints a string and calls the function (tests function definition and call).
7. test-if-pos.olh: Positive test. Conditional that prints one string if the condition is true and another if false; condition is always true here (tests conditional evaluation and branching).
8. test-list-pos.olh: Positive test. Prints a list of four characters (tests list creation and printing lists). MicroC does not implement lists of chars.
9. test-print-pos.olh: Positive test. Concatenates a string to an int and prints it (tests string/int concatenation). MicroC does not allow binops on expressions of different types.
10. test-printbasic-pos.olh: Positive test. Prints two strings in sequence (tests string printing).


HOW TO COMPILE AND RUN:
$ make
$ @TODO: copy from report

You should get the hello, world printed on your console!
Soon, you should be able to play some word games too!
