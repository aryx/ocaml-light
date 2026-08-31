/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Asm part of the runtime system, ARM processor */

trap_ptr	.req	r11
alloc_ptr	.req	r8
alloc_limit	.req	r9

//old: forbidden with arm-linux-gnueabihf-as
//sp		.req	r13
//lr		.req	r14
//pc		.req	r15

        .text

/* Allocation functions and GC interface */

        .global caml_call_gc
caml_call_gc:
    /* Record return address */
    /* We can use r10 as a temp reg since it's not live here */
        ldr     r10, .Lcaml_last_return_address
        str     lr, [r10, #0]
    /* Branch to shared GC code */
        bl      .Linvoke_gc
    /* Restart allocation sequence (4 instructions before) */
        sub     lr, lr, #16
        mov     pc, lr

        .global caml_alloc1
caml_alloc1:
	ldr	r10, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, #8
        cmp     alloc_ptr, r10
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r10, .Lcaml_last_return_address
        str     lr, [r10, #0]
    /* Invoke GC */
        bl      .Linvoke_gc
    /* Try again */
        b       caml_alloc1

        .global caml_alloc2
caml_alloc2:
	ldr	r10, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, #12
        cmp     alloc_ptr, r10
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r10, .Lcaml_last_return_address
        str     lr, [r10, #0]
    /* Invoke GC */
        bl      .Linvoke_gc
    /* Try again */
        b       caml_alloc2

        .global caml_alloc3
caml_alloc3:
	ldr	r10, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, #16
        cmp     alloc_ptr, r10
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r10, .Lcaml_last_return_address
        str     lr, [r10, #0]
    /* Invoke GC */
        bl      .Linvoke_gc
    /* Try again */
        b       caml_alloc3

        .global caml_alloc
caml_alloc:
	str	r12, [sp, #-4]!
	ldr	r12, [alloc_limit, #0]
        sub     alloc_ptr, alloc_ptr, r10
        cmp     alloc_ptr, r12
	ldr	r12, [sp], #4
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address and desired size */
        ldr     alloc_limit, .Lcaml_last_return_address
        str     lr, [alloc_limit, #0]
        str     r10, .Lcaml_requested_size
    /* Invoke GC */
        bl      .Linvoke_gc
    /* Try again */
	ldr	r10, .Lcaml_requested_size
        b       caml_alloc

/* Shared code to invoke the GC */
.Linvoke_gc:
    /* Record lowest stack address */
        ldr     r10, .Lcaml_bottom_of_stack
        str     sp, [r10, #0]
    /* Save integer registers and return address on stack */
        stmfd   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r10,r12,lr}
    /* Store pointer to saved integer registers in caml_gc_regs */
        ldr     r10, .Lcaml_gc_regs
        str     sp, [r10, #0]
    /* claude: pad by 4 bytes. AAPCS requires sp to be 8-byte aligned at
     * the "bl garbage_collection" call below (see the longer explanation
     * on the float-register save just past this). The 11 words just
     * pushed above (44 bytes) are not themselves a multiple of 8; this
     * pad (removed again, symmetrically, once the float registers are
     * restored below) corrects that without disturbing the caml_gc_regs
     * pointer just recorded, which must keep pointing at the integer
     * register block in the exact layout the GC's root-scanning code
     * expects -- that's why this pad sits here, after caml_gc_regs is
     * captured, rather than folded into the stmfd above. */
        sub     sp, sp, #4
    /* Save float registers */
	//old: was using stfd f0, ... but actually not supported by
        // arm-linux-gnueabihf-as so we used vstr.f64 and d0 as suggested
        // by chatGPT.
        //old: moreover the ! suffix "write-back-to-base-register" is not
        // supported so had to transform in sub sp thing below
        //old:
        // stfd    f0, [sp, #-8]!
        // stfd    f1, [sp, #-8]!
        // stfd    f2, [sp, #-8]!
        // stfd    f3, [sp, #-8]!
    /* claude: this used to save only d0-d3 here ("non-callee-save float
     * registers", per the old comment) and then, worse, restore into
     * d4-d7 below instead of d0-d3 -- a straight typo, popping the saved
     * bits into the wrong registers entirely. Both parts trace back to
     * the same stale assumption as the "f4-f7 preserved by C" comment in
     * asmcomp/arm/proc.ml (see the "claude:" comment there): under the
     * real AAPCS-VFP ABI this target uses, ALL of d0-d7 are caller-saved
     * (volatile), not just d0-d3, so garbage_collection() (a plain C
     * function, free to clobber any of them) can and does trash whichever
     * of d0-d7 happen to hold live Caml float values across this call.
     * asmcomp/arm/proc.ml's destroyed_at_oper for Ialloc claims none of
     * the 8 float registers are destroyed by an allocation (correct, if
     * this routine actually preserves them, which is exactly what it
     * must do here) -- so all 8 need saving, not 4, and the save/restore
     * pairing must actually match. vpush/vpop {d0-d7} do exactly that in
     * one instruction each (confirmed to assemble fine with this
     * target's -march/-mfpu flags), and additionally pad the total
     * pushed size to a multiple of 8 for free (8 doubles = 64 bytes) --
     * unlike the previous 4-double version, which combined with the
     * 11-word (44-byte, not a multiple of 8) integer-register save above
     * to leave sp misaligned at the "bl garbage_collection" call below:
     * yet another case of the AAPCS 8-byte stack alignment bug fixed
     * elsewhere in caml_start_program/emit.mlp's frame_size, except this
     * one corrupted floating-point results (not just, say, sprintf) in
     * proportion to how often the program actually triggered the GC --
     * which is exactly the symptom that led here: test/fft.ml's error
     * grew with problem size (more allocation => more GC => more chances
     * to hit this) instead of staying near the expected ~1e-15. */
        vpush   {d0-d7}
    /* Save current allocation pointer for debugging purposes */
        ldr     r10, .Lyoung_ptr
        str     alloc_ptr, [r10, #0]
    /* Save trap pointer in case an exception is raised during GC */
        ldr     r10, .Lcaml_exception_pointer
        str     trap_ptr, [r10, #0]
    /* Call the garbage collector */
        bl      garbage_collection
    /* Restore the float registers from the stack */
        vpop    {d0-d7}
    /* claude: undo the 4-byte alignment pad from before the float save */
        add     sp, sp, #4

        ldmfd   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r10,r12}
    /* Reload return address */
        ldr     r10, .Lcaml_last_return_address
        ldr     lr, [r10, #0]
    /* Say that we are back into Caml code */
        mov     alloc_ptr, #0
        str     alloc_ptr, [r10, #0]
    /* Reload new allocation pointer and allocation limit */
        ldr     r10, .Lyoung_ptr
        ldr     alloc_ptr, [r10, #0]
        ldr     alloc_limit, .Lyoung_limit
    /* Return to caller */
        ldmfd   sp!, {pc}

/* Call a C function from Caml */
/* Function to call is in r10 */

        .global caml_c_call
caml_c_call:
    /* Preserve return address in callee-save register r4 */
        mov     r4, lr
    /* Record lowest stack address and return address */
        ldr     r5, .Lcaml_last_return_address
        ldr     r6, .Lcaml_bottom_of_stack
        str     lr, [r5, #0]
        str     sp, [r6, #0]
    /* Make the exception handler and alloc ptr available to the C code */
        ldr     r6, .Lyoung_ptr
        ldr     r7, .Lcaml_exception_pointer
        str     alloc_ptr, [r6, #0]
        str     trap_ptr, [r7, #0]
    /* Call the function */
        mov     lr, pc
        mov     pc, r10
    /* Reload alloc ptr */
        ldr     alloc_ptr, [r6, #0]    /* r6 still points to young_ptr */
    /* Say that we are back into Caml code */
        mov     r6, #0
        str     r6, [r5, #0] /* r5 still points to caml_last_return_address */
    /* Return */
        mov     pc, r4

/* Start the Caml program */

        .global caml_start_program
caml_start_program:
        ldr     r10, .Lcaml_program

/* Code shared with callback* */
/* Address of Caml code to call is in r10 */
/* Arguments to the Caml code are in r0...r3 */

.Ljump_to_caml:
    /* Save return address and callee-save registers */
        stmfd   sp!, {r4,r5,r6,r7,r8,r9,r11,lr}
        //old: stfd ... f7 ...
        //stfd    f7, [sp, #-8]!
        //stfd    f6, [sp, #-8]!
        //stfd    f5, [sp, #-8]!
        //stfd    f4, [sp, #-8]!
	sub sp, sp, #8
	vstr.f64 d7, [sp]
	sub sp, sp, #8
	vstr.f64 d6, [sp]
	sub sp, sp, #8
	vstr.f64 d5, [sp]
	sub sp, sp, #8
	vstr.f64 d4, [sp]
    /* Setup a callback link on the stack */
    /* claude: this is 3 words of real data (bottom_of_stack,
     * last_return_address, gc_regs, at offsets 0/4/8 below) padded out to
     * 4*4=16 bytes instead of 4*3=12. AAPCS requires sp to be 8-byte
     * aligned at every call out to a C function; every other push in
     * this function (the 8 callee-save regs above, the four vstr.f64
     * pushes) is already a multiple of 8, so this odd 3-word (12 byte)
     * allocation was the one place quietly knocking sp 4 bytes off
     * doubleword alignment for the rest of a Caml program's execution
     * (asmcomp/arm/emit.mlp's frame_size has the matching fix, with a
     * longer explanation of the symptom this caused: sprintf("%.12g",
     * some_double) silently printing garbage as soon as it's reached
     * through any call chain rooted here, i.e. always). This must stay
     * in sync with the two matching "add sp, sp, #4*4" below (was #4*3)
     * that pop this same callback link. */
        sub     sp, sp, #4*4
        ldr     r4, .Lcaml_bottom_of_stack
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, .Lcaml_last_return_address
        ldr     r4, [r4, #0]
        str     r4, [sp, #4]
        ldr     r4, .Lcaml_gc_regs
        ldr     r4, [r4, #0]
        str     r4, [sp, #8]
    /* Setup a trap frame to catch exceptions escaping the Caml code */
        sub     sp, sp, #4*2
        ldr     r4, .Lcaml_exception_pointer
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, .LLtrap_handler
        str     r4, [sp, #4]
	mov	trap_ptr, sp
    /* Reload allocation pointers */
        ldr     r4, .Lyoung_ptr
        ldr     alloc_ptr, [r4, #0]
        ldr     alloc_limit, .Lyoung_limit
    /* We are back into Caml code */
        ldr     r4, .Lcaml_last_return_address
        mov     r5, #0
        str     r5, [r4, #0]
    /* Call the Caml code */
        mov     lr, pc
        mov     pc, r10
.Lcaml_retaddr:
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldr     r4, .Lcaml_exception_pointer
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        add     sp, sp, #2 * 4
    /* Pop the callback link, restoring the global variables */
        ldr     r4, .Lcaml_bottom_of_stack
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_last_return_address
        ldr     r5, [sp, #4]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_gc_regs
        ldr     r5, [sp, #8]
        str     r5, [r4, #0]
        add     sp, sp, #4*4    /* claude: was #4*3, see the matching sub above */
    /* Update allocation pointer */
        ldr     r4, .Lyoung_ptr
        str     alloc_ptr, [r4, #0]
    /* Reload callee-save registers and return */
        //old:
        //ldfd    f4, [sp], #8
        //ldfd    f5, [sp], #8
        //ldfd    f6, [sp], #8
        //ldfd    f7, [sp], #8
	vldr.f64 d4, [sp]
	vldr.f64 d5, [sp, #8]
	vldr.f64 d6, [sp, #16]
	vldr.f64 d7, [sp, #24]
	add sp, sp, #32  // adjust SP after all loads

        ldmfd   sp!, {r4,r5,r6,r7,r8,r9,r11,pc}

    /* The trap handler */
.Ltrap_handler:
    /* Restore the global variables used by caml_c_call */
        ldr     r4, .Lcaml_bottom_of_stack
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_last_return_address
        ldr     r5, [sp, #4]
        str     r5, [r4, #0]
        ldr     r4, .Lcaml_gc_regs
        ldr     r5, [sp, #8]
        str     r5, [r4, #0]
        add     sp, sp, #4*4    /* claude: was #4*3, see the matching sub above */
    /* Save young_ptr */
        ldr     r4, .Lyoung_ptr
        str     alloc_ptr, [r4, #0]
    /* Re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
        bl      mlraise         /* Keep retaddr in lr for debugging */
        /* never returns */

/* Raise an exception from C */

        .global raise_caml_exception
raise_caml_exception:
    /* Reload Caml allocation pointers */
        ldr     r1, .Lyoung_ptr
        ldr     alloc_ptr, [r1, #0]
        ldr     alloc_limit, .Lyoung_limit
    /* Say we're back into Caml */
        ldr     r1, .Lcaml_last_return_address
        mov     r2, #0
        str     r2, [r1, #0]
    /* Cut stack at current trap handler */
        ldr     r1, .Lcaml_exception_pointer
        ldr     sp, [r1, #0]
    /* Pop previous handler and addr of trap, and jump to it */
        ldmfd	sp!, {trap_ptr, pc}

/* Callback from C to Caml */

        .global callback
callback:
    /* Initial shuffling of arguments (r0 = closure, r1 = first arg) */
        mov     r10, r0
        mov     r0, r1            /* r0 = first arg */
        mov     r1, r10           /* r1 = closure environment */
        ldr     r10, [r10, #0]    /* code pointer */
        b       .Ljump_to_caml

        .global callback2
callback2:
    /* Initial shuffling of arguments (r0 = closure, r1 = arg1, r2 = arg2) */
        mov     r10, r0
        mov     r0, r1           /* r0 = first arg */
        mov     r1, r2           /* r1 = second arg */
        mov     r2, r10          /* r2 = closure environment */
        ldr     r10, .Lcaml_apply2
        b       .Ljump_to_caml

        .global callback3
callback3:
    /* Initial shuffling of arguments */
    /* (r0 = closure, r1 = arg1, r2 = arg2, r3 = arg3) */
        mov     r10, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r2          /* r1 = second arg */
        mov     r2, r3          /* r2 = third arg */
        mov     r3, r10         /* r3 = closure environment */
        ldr     r10, .Lcaml_apply3
        b       .Ljump_to_caml

    /* claude: no trampoline needed here. array_bound_error is a plain C
     * function (asmrun/array.c) that just calls fatal_error and never
     * returns -- it does no allocation, so unlike an "external" call it
     * doesn't need the caml_c_call protocol (recording
     * caml_bottom_of_stack, alloc_ptr, etc. for the GC's benefit).
     * emit.mlp's bounds-check code (Icheckbound) branches to it directly
     * ("blls array_bound_error" / "blcs array_bound_error"), exactly
     * like asmcomp/i386/emit.mlp's "call array_bound_error". A previous
     * version of this file defined a same-named trampoline right here
     * that loaded array_bound_error's address into r10 and jumped into
     * caml_c_call -- which, now that this symbol is a real global
     * (see asmrun/array.c), collides with it at link time
     * ("multiple definition of `array_bound_error'"). */
    /* old:
     *         .global caml_array_bound_error
     * array_bound_error:
     *     // Load address of array_bound_error in r10
     *         ldr     r10, .Larray_bound_error
     *     // Call that function
     *         b       caml_c_call
     */

/* Global references */

.Lcaml_last_return_address:     .word caml_last_return_address
.Lcaml_bottom_of_stack:         .word caml_bottom_of_stack
.Lcaml_gc_regs:                 .word caml_gc_regs
.Lyoung_ptr:                    .word young_ptr
.Lyoung_limit:                  .word young_limit
.Lcaml_exception_pointer:       .word caml_exception_pointer
.Lcaml_program:                 .word caml_program
.LLtrap_handler:                .word .Ltrap_handler
.Lcaml_apply2:                  .word caml_apply2
.Lcaml_apply3:                  .word caml_apply3
.Lcaml_requested_size:		.word 0

/* GC roots for callback */

        .data

        .global system_frametable
system_frametable:
        .word   1               /* one descriptor */
        .word   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  2
