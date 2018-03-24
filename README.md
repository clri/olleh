The OLLEH compiler 

Amnah Ahmad: aza2111@barnard.edu
Mahika Bhalla: mmb2276@columbia.edu
Caroline Roig-Irwin: clr2176@columbia.edu

Coded in OCaml, this takes a language made to code word-games and compiles it into LLVM IR

It needs the OCaml llvm library, wihch is most easily installed through opam

The version of the OCaml llvm library should match the version of LLVM that is installed on yyour system. 

HOW TO COMPILE AND RUN:
run the command "make" on your command line 
The first time you may have to run: "chmod 777 testall.sh"
To run the test files: "./testall.sh" 
You will see the results printed and you can look at them in testall.log too. 


