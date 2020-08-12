.text
#if (__APPLE__)
	.global _entry_point

_entry_point:
#else
	.global entry_point

entry_point:
#endif
	push %rbp	# save stack frame for C convention
	mov %rsp, %rbp

	# beginning generated code
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	addq %rbx, %rax
	# end generated code
	# %rax contains the result

	mov %rbp, %rsp	# reset frame
	pop %rbp
	ret



