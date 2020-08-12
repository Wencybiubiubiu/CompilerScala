.text
.global _putchar, _getchar, _entry_point

################# FUNCTIONS #####################
_f:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, %rdx
	movq %rsi, %rcx
	addq %rcx, %rdx
	movq %rdx,%rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret

_g:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, %rcx
	movq %rsi, %r8
	movq $2, %r8
	push %rdi
	push %rsi
	push %rdx
	push %rcx
	movq %r8,%rdi
	movq %r9,%rsi
	call _f
	pop %rcx
	pop %rdx
	pop %rsi
	pop %rdi
	movq %rax,%r8
	imul %r8, %rcx
	movq %rdx, %r8
	addq %r8, %rcx
	movq %rcx,%rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret

#################################################


###################### MAIN #####################
_entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	movq $1, %rdi
	movq $2, %rdi
	movq %rdi,%rdi
	movq %rsi,%rsi
	call _f
	movq %rax,%rdi
	movq $2, %rsi
	movq $5, %rsi
	movq $100, %rsi
	push %rdi
	movq %rsi,%rdi
	movq %rdx,%rsi
	movq %rcx,%rdx
	call _g
	pop %rdi
	movq %rax,%rsi
	movq %rsi, %rdx
	movq %rdx, %rsi
	movq %rsi, %rdi
	movq %rdi, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret
#################################################


#################### DATA #######################

.data
heap:	.quad 0
#################################################
