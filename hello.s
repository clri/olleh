	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rax
Lcfi0:
	.cfi_def_cfa_offset 16
	leaq	L_fmt(%rip), %rdi
	leaq	L___unnamed_1(%rip), %rsi
	xorl	%eax, %eax
	callq	_printf
	popq	%rax
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"

L___unnamed_1:                          ## @0
	.asciz	"\"hello, world!\""


.subsections_via_symbols
