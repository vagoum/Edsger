	.text
	.file	"a.ll"
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp0:
	.cfi_def_cfa_offset 16
	movl	$.Ltmp, %edi
	callq	writeString
	xorl	%eax, %eax
	popq	%rcx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.type	.Ltmp,@object           # @tmp
	.section	.rodata.str1.1,"aMS",@progbits,1
.Ltmp:
	.asciz	"\"Hello World\""
	.size	.Ltmp, 14


	.section	".note.GNU-stack","",@progbits
