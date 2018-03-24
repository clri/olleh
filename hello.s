	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	__start                 ## -- Begin function _start
	.p2align	4, 0x90
__start:                                ## @_start
	.cfi_startproc
## BB#0:                                ## %entry
	subq	$88, %rsp
Lcfi0:
	.cfi_def_cfa_offset 96
	movl	$0, 80(%rsp)
	movl	$34, 72(%rsp)
	movl	$33, 64(%rsp)
	movl	$100, 56(%rsp)
	movl	$108, 48(%rsp)
	movl	$114, 40(%rsp)
	movl	$111, 32(%rsp)
	movl	$119, 24(%rsp)
	movl	$32, 16(%rsp)
	movl	$44, 8(%rsp)
	movl	$111, (%rsp)
	leaq	L_fmt(%rip), %rdi
	movl	$34, %esi
	movl	$104, %edx
	movl	$101, %ecx
	movl	$108, %r8d
	movl	$108, %r9d
	xorl	%eax, %eax
	callq	_printf
	addq	$88, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"


.subsections_via_symbols
