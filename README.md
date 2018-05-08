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


* Test Suite--see \*.olh in tests folder (run ./testall.sh to see output).
fail-\*-neg.olh are negative tests; test-\*-pos.olh are positive tests.
@TODO list our tests/what they do if this is necessary
fail-add1-neg.olh
fail-add3-neg.olh
fail-ambigmap-neg.olh
fail-and-neg.olh
fail-chars-neg.olh
fail-cmp-neg.olh
fail-comment-neg.olh
fail-condscope-neg.olh
fail-div-neg.olh
fail-doublequote-neg.olh
fail-efor-neg.olh
fail-eif-neg.olh
fail-eq-neg.olh
fail-folo-neg.olh
fail-foreach-neg.olh
fail-fpla-neg.olh
fail-freshlis1-neg.olh
fail-freshlis2-neg.olh
fail-freshlis3-neg.olh
fail-freshpla1-neg.olh
fail-freshpla2-neg.olh
fail-funcfunc-neg.olh
fail-glist-neg.olh
fail-glofo-neg.olh
fail-glolo-neg.olh
fail-gmap-neg.olh
fail-intlist-neg.olh
fail-intlist2-neg.olh
fail-list-neg.olh
fail-listlistlist-neg.olh
fail-listype-neg.olh
fail-mapkey-neg.olh
fail-maptype-neg.olh
fail-mapval-neg.olh
fail-mixcmp-neg.olh
fail-mixmap-neg.olh
fail-mod-neg.olh
fail-mult-neg.olh
fail-nega-neg.olh
fail-noparens-neg.olh
fail-not-neg.olh
fail-not2-neg.olh
fail-null-neg.olh
fail-or-neg.olh
fail-paramnum-neg.olh
fail-paramtype-neg.olh
fail-prefix-neg.olh
fail-print-neg.olh
fail-printplayer-neg.olh
fail-prior-neg.olh
fail-quote-neg.olh
fail-ret-neg.olh
fail-scope-neg.olh
fail-semi-neg.olh
fail-slist-neg.olh
fail-smap-neg.olh
fail-subtract-neg.olh
fail-undef-neg.olh
fail-unop-neg.olh
fail-var1-neg.olh
fail-var2-neg.olh
fail-var3-neg.olh
fail-var4-neg.olh
fail-var5-neg.olh
fail-vartype-neg.olh
fail-voidas-neg.olh
fail-voidvar-neg.olh
test-add-pos.olh
test-assign-pos.olh
test-builtin-pos.olh
test-compare-pos.olh
test-concat-pos.olh
test-emptybody-pos.olh
test-exit-pos.olh
test-for-pos.olh
test-foreach-pos.olh
test-func-pos.olh
test-if-pos.olh
test-int-pos.olh
test-list-pos.olh
test-loop-pos.olh
test-map-pos.olh
test-null-pos.olh
test-player-pos.olh
test-print-pos.olh
test-printbasic-pos.olh
test-stdlib-pos.olh
test-str-pos.olh
test-while-pos.olh


HOW TO COMPILE AND RUN:
$ make
$ ./testall.sh

If the output of testall.sh does not report any errors, you can go ahead and
compile your own code (assuming your-code.olh is the program you've written).
$ ./olleh your-code.olh > your-code.ll
$ llc your-code.ll > your-code.s
$ cc -o your-code.exe your-code.s ostdlib.o
$./your-code.exe
