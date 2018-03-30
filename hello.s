	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:                                ## %entry
	pushq	%rbx
Lcfi0:
	.cfi_def_cfa_offset 16
Lcfi1:
	.cfi_offset %rbx, -16
	callq	_InitializeLocalGarbage
	leaq	L_fmt(%rip), %rbx
	leaq	L___unnamed_1(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	leaq	L___unnamed_2(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	callq	_CollectLocalGarbage
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"

L___unnamed_1:                          ## @0
	.asciz	"hello, world!"

L___unnamed_2:                          ## @1
	.asciz	"!dlrow ,olleh"


.subsections_via_symbols
