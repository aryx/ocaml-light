/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, AMD64 processor */
/* Must be preprocessed by cpp */

#define FUNCTION_ALIGN 4

#define FUNCTION(name) \
        .globl name; \
        .type  name,@function; \
        .align FUNCTION_ALIGN; \
        name:

        .text

/* Allocation */

FUNCTION(caml_call_gc)
    /* Record lowest stack address and return address */
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
    /* Save young_ptr, caml_exception_pointer */
	movq	%r15, young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
    /* Build array of registers, save it into caml_gc_regs */
.L105:  
        pushq   %r13
        pushq   %r12
        pushq   %rbp
        pushq   %r11
        pushq   %r10
        pushq   %r9
        pushq   %r8
        pushq   %rcx
        pushq   %rdx
        pushq   %rsi
        pushq   %rdi
        pushq   %rbx
        pushq   %rax
        movq    %rsp, caml_gc_regs
    /* Save floating-point registers */
        subq    $(16*8), %rsp
        movlpd  %xmm0, 0*8(%rsp)
        movlpd  %xmm1, 1*8(%rsp)
        movlpd  %xmm2, 2*8(%rsp)
        movlpd  %xmm3, 3*8(%rsp)
        movlpd  %xmm4, 4*8(%rsp)
        movlpd  %xmm5, 5*8(%rsp)
        movlpd  %xmm6, 6*8(%rsp)
        movlpd  %xmm7, 7*8(%rsp)
        movlpd  %xmm8, 8*8(%rsp)
        movlpd  %xmm9, 9*8(%rsp)
        movlpd  %xmm10, 10*8(%rsp)
        movlpd  %xmm11, 11*8(%rsp)
        movlpd  %xmm12, 12*8(%rsp)
        movlpd  %xmm13, 13*8(%rsp)
        movlpd  %xmm14, 14*8(%rsp)
        movlpd  %xmm15, 15*8(%rsp)
    /* Call the garbage collector */
        call    garbage_collection
    /* Restore all regs used by the code generator */
        movlpd  0*8(%rsp), %xmm0
        movlpd  1*8(%rsp), %xmm1
        movlpd  2*8(%rsp), %xmm2
        movlpd  3*8(%rsp), %xmm3
        movlpd  4*8(%rsp), %xmm4
        movlpd  5*8(%rsp), %xmm5
        movlpd  6*8(%rsp), %xmm6
        movlpd  7*8(%rsp), %xmm7
        movlpd  8*8(%rsp), %xmm8
        movlpd  9*8(%rsp), %xmm9
        movlpd  10*8(%rsp), %xmm10
        movlpd  11*8(%rsp), %xmm11
        movlpd  12*8(%rsp), %xmm12
        movlpd  13*8(%rsp), %xmm13
        movlpd  14*8(%rsp), %xmm14
        movlpd  15*8(%rsp), %xmm15
        addq    $(16*8), %rsp
        popq    %rax
        popq    %rbx
        popq    %rdi
        popq    %rsi
        popq    %rdx
        popq    %rcx
        popq    %r8
        popq    %r9
        popq    %r10
        popq    %r11
        popq    %rbp
        popq    %r12
        popq    %r13
    /* Restore young_ptr, caml_exception_pointer */
	movq	young_ptr(%rip), %r15
	movq	caml_exception_pointer(%rip), %r14
    /* Return to caller */
        ret

/* claude: each slow path below must save %r15/%r14 into the young_ptr/
   caml_exception_pointer globals itself, exactly like caml_call_gc's own
   header does, before falling into the shared .L105 -- unlike e.g.
   asmrun/alpha.S, where caml_alloc1/2/3/caml_alloc branch straight into
   caml_call_gc's entry point (so its header always runs), amd64's
   .L105 is a continuation AFTER that header, reached directly here
   without ever running it. Without this, garbage_collection() (see
   asmrun/signals.c) decides whether to run minor_collection() at all by
   comparing the *global* young_ptr against young_start -- a stale
   (pre-this-allocation-burst) young_ptr can wrongly read as "plenty of
   room left", so the collection that .L105's caller is blocked on gets
   silently skipped, .L105's post-GC "movq young_ptr(%rip), %r15" then
   reloads that same stale value instead of the reset young_end, and the
   retried allocation proceeds against a %r15 that was never actually
   validated -- corrupting whatever memory it walks into. (Reproduced as
   a heap-corruption segfault, sometimes deep inside an unrelated later
   allocation or C call, once total allocation first crosses the minor
   heap size; see the amd64 port's commit message.) caml_exception_pointer
   is saved for the same reason garbage_collection() itself needs it:
   a pending signal handled here (execute_signal -> callback) unwinds
   through it if the callback raises. */
FUNCTION(caml_alloc1)
        subq    $16, %r15
        cmpq    young_limit(%rip), %r15
        jb      .L100
        ret
.L100:
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
	movq	%r15, young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
	subq	$8, %rsp
        call    .L105
	addq	$8, %rsp
        jmp     caml_alloc1

FUNCTION(caml_alloc2)
        subq    $24, %r15
        cmpq    young_limit(%rip), %r15
        jb      .L101
        ret
.L101:
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
	movq	%r15, young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
	subq	$8, %rsp
        call    .L105
	addq	$8, %rsp
        jmp     caml_alloc2

FUNCTION(caml_alloc3)
        subq    $32, %r15
        cmpq    young_limit(%rip), %r15
        jb      .L102
        ret
.L102:
        movq    0(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    8(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
	movq	%r15, young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
	subq	$8, %rsp
        call    .L105
	addq	$8, %rsp
        jmp     caml_alloc3

FUNCTION(caml_alloc)
        subq    %rax, %r15
        cmpq    young_limit(%rip), %r15
        jb      .L103
        ret
.L103:
        pushq   %rax                       /* save desired size */
        movq    8(%rsp), %rax
        movq    %rax, caml_last_return_address(%rip)
        leaq    16(%rsp), %rax
        movq    %rax, caml_bottom_of_stack(%rip)
	movq	%r15, young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
        call    .L105
        popq    %rax                      /* recover desired size */
        jmp     caml_alloc

/* Call a C function from Caml */

FUNCTION(caml_c_call)
    /* Record lowest stack address and return address */
        popq    %r12
        movq    %r12, caml_last_return_address(%rip)
        movq    %rsp, caml_bottom_of_stack(%rip)
    /* Make the exception handler and alloc ptr available to the C code */
	movq	%r15, young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
    /* Call the function (address in %rax) */
        call    *%rax
    /* Reload alloc ptr */
	movq	young_ptr(%rip), %r15
    /* Return to caller */
	pushq	%r12
	ret

/* Start the Caml program */

FUNCTION(caml_start_program)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial entry point is caml_program */
        leaq    caml_program(%rip), %r12
    /* Common code for caml_start_program and callback* */
.L106:
    /* Build a callback link */
	subq	$8, %rsp	/* stack 16-aligned */
        pushq   caml_gc_regs(%rip)
        pushq   caml_last_return_address(%rip)
        pushq   caml_bottom_of_stack(%rip)
    /* Setup alloc ptr and exception ptr */
	movq	young_ptr(%rip), %r15
	movq	caml_exception_pointer(%rip), %r14
    /* Build an exception handler */
        lea     .L108(%rip), %r13
        pushq   %r13
        pushq   %r14
        movq    %rsp, %r14
    /* Call the Caml code */
        call    *%r12
.L107:
    /* Pop the exception handler */
        popq    %r14
        popq    %r12    /* dummy register */
.L109:
    /* Update alloc ptr and exception ptr */
	movq	%r15, young_ptr(%rip)
	movq	%r14, caml_exception_pointer(%rip)
    /* Pop the callback link, restoring the global variables */
        popq    caml_bottom_of_stack(%rip)
        popq    caml_last_return_address(%rip)
        popq    caml_gc_regs(%rip)
	addq	$8, %rsp
    /* Restore callee-save registers. */
	addq	$8, %rsp
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %rbp
        popq    %rbx
    /* Return to caller. */
        ret
.L108:
    /* Exception handler. Unlike upstream OCaml's callback_exn (which
       tags the bucket and returns it, leaving the C caller to check the
       tag itself), ocaml-light's callback()/callback2()/callback3() are
       plain "value f(...)" C functions with no such tagging convention
       (see byterun/callback.h / asmrun/callback.c) -- so, like every
       other target here (e.g. asmrun/alpha.S's $109, asmrun/m68k.S's
       L108), unwind the callback link and re-raise through mlraise so
       the exception propagates to the next enclosing trap frame (or
       terminates the program), instead of returning here at all.
       raise_caml_exception already popped caml_exception_pointer back
       to the enclosing handler before jumping here, so only young_ptr
       (from the still-live %r15) needs updating below. */
        popq    caml_bottom_of_stack(%rip)
        popq    caml_last_return_address(%rip)
        popq    caml_gc_regs(%rip)
        addq    $8, %rsp
        movq    %r15, young_ptr(%rip)
        movq    %rax, %rdi
        call    mlraise         /* never returns */

/* Raise an exception from C */

FUNCTION(raise_caml_exception)
        movq    %rdi, %rax
        movq    caml_exception_pointer(%rip), %rsp
        popq    caml_exception_pointer(%rip)
        ret

/* Callback from C to Caml */

FUNCTION(callback)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial loading of arguments */
        movq    %rdi, %rbx      /* closure */
        movq    %rsi, %rax      /* argument */
        movq    0(%rbx), %r12   /* code pointer */
        jmp     .L106

FUNCTION(callback2)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial loading of arguments */
        /* closure stays in %rdi */
        movq    %rsi, %rax               /* first argument */
        movq    %rdx, %rbx               /* second argument */
        leaq    caml_apply2(%rip), %r12  /* code pointer */
        jmp     .L106

FUNCTION(callback3)
    /* Save callee-save registers */
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
	subq	$8, %rsp	/* stack 16-aligned */
    /* Initial loading of arguments */
        movq    %rsi, %rax               /* first argument */
        movq    %rdx, %rbx               /* second argument */
        movq    %rdi, %rsi               /* closure */
        movq    %rcx, %rdi               /* third argument */
        leaq    caml_apply3(%rip), %r12  /* code pointer */
        jmp     .L106

        .data
        .globl  system_frametable
        .type   system_frametable,@object
        .align  8
system_frametable:
        .quad   1           /* one descriptor */
        .quad   .L107       /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  8

	.section	.rodata.cst8,"aM",@progbits,8
        .globl  caml_negf_mask
        .type   caml_negf_mask,@object
        .align  16
caml_negf_mask:
	.quad	0x8000000000000000, 0
        .globl  caml_absf_mask
        .type   caml_absf_mask,@object
        .align  16
caml_absf_mask:
	.quad	0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF
