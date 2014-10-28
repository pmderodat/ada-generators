	.bss

	.align	32
	.globl	coroutines__switch_cptr
	.type	coroutines__switch_cptr, @object
	.size	coroutines__switch_cptr, 8
coroutines__switch_cptr:
	.zero	8

	.text
	.align	32
	.globl	coroutines__switch_helper
	.type	coroutines__switch_helper, @function
coroutines__switch_helper:
	# This is a pointer to the coroutine we are switching to.
	movq	%rdi, coroutines__switch_cptr(%rip)

	# Update the previous coroutine (which will be the coroutine we are
	# switching from).
	movq	coroutines__current(%rip), %rdi
	movq	%rdi, coroutines__previous(%rip)

	# Now, make rdi contain a pointer to current coroutine's register base.
	# First get the address of the coroutine data record in RDI.
	movq	0x08(%rdi), %rdi
	lea	0x10(%rdi), %rdi

	#
	# Save registers to the coroutine we are switching from.
	#

	# Some registers are caller-saved, so we do not need to save them.
	# Anyway...

	movq	%rax, 0x00(%rdi)
	movq	%rcx, 0x08(%rdi)
	movq	%rdx, 0x10(%rdi)
	movq	%rbx, 0x18(%rdi)
	movq	%rsp, 0x20(%rdi)
	movq	%rbp, 0x28(%rdi)
	movq	%rsi, 0x30(%rdi)

	# Remember, the content of %rdi was saved to coroutines__switch_cptr
	movq	coroutines__switch_cptr(%rip), %rax
	movq	%rax, 0x38(%rdi)

	movq	%r8,  0x40(%rdi)
	movq	%r9,  0x48(%rdi)
	movq	%r10, 0x50(%rdi)
	movq	%r11, 0x58(%rdi)
	movq	%r12, 0x60(%rdi)
	movq	%r13, 0x68(%rdi)
	movq	%r14, 0x70(%rdi)
	movq	%r15, 0x78(%rdi)

	# Get the coroutine we are switching to and update the current
	# coroutine.
	movq	coroutines__switch_cptr(%rip), %rdi
	movq	%rdi, coroutines__current(%rip)
	# ... and more specifically a pointer to its register base.
	movq	0x08(%rdi), %rdi
	lea	0x10(%rdi), %rdi

	#
	# Now, get registers from the coroutine we are switching to.
	#

	movq	0x00(%rdi), %rax
	movq	0x08(%rdi), %rcx
	movq	0x10(%rdi), %rdx
	movq	0x18(%rdi), %rbx
	movq	0x20(%rdi), %rsp
	movq	0x28(%rdi), %rbp
	movq	0x30(%rdi), %rsi
	# We are strongly using %rdi to restore register: we'll restore it
	# itself later...
	movq	0x40(%rdi), %r8
	movq	0x48(%rdi), %r9
	movq	0x50(%rdi), %r10
	movq	0x58(%rdi), %r11
	movq	0x60(%rdi), %r12
	movq	0x68(%rdi), %r13
	movq	0x70(%rdi), %r14
	movq	0x78(%rdi), %r15

	# ... like now!
	movq	0x38(%rdi), %rdi

	#
	# And now, return to the new coroutine!
	#
	ret

	.size	coroutines__switch_helper, .-coroutines__switch_helper
