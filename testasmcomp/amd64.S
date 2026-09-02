/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

        .globl  call_gen_code
        .align  16
call_gen_code:
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
    /* claude: 6 pushes (48 bytes) alone preserve whatever alignment
       call_gen_code was entered with (16-aligned, per the SysV ABI's
       "function entry" invariant) -- without this, the generated Caml
       code entered below via "call *%r10" starts 8 bytes off, same as
       asmrun/amd64.S's caml_start_program pads for the same reason.
       Harmless for the other testasmcomp programs (fib/tak/quicksort/
       etc never call a variadic C function from *inside* the generated
       code), but soli.cmm calls printf directly from Caml code and
       crashed inside glibc's `movaps` var-args prologue without this. */
	subq	$8, %rsp
	movq	%rdi, %r10
	movq	%rsi, %rax
	movq	%rdx, %rbx
	movq	%rcx, %rdi
	movq	%r8, %rsi
        call	*%r10
	addq	$8, %rsp
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %rbp
        popq    %rbx
        ret

        .globl  caml_c_call
        .align  16
caml_c_call:
        jmp     *%rax

	.section	.rodata.cst8,"aM",@progbits,8
        .globl  caml_negf_mask
        .align  16
caml_negf_mask:
	.quad	0x8000000000000000, 0
        .globl  caml_absf_mask
        .align  16
caml_absf_mask:
	.quad	0x7FFFFFFFFFFFFFFF, 0

        .comm   young_limit, 8
