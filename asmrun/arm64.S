/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2013 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Asm part of the runtime system, ARM processor, 64-bit mode */
/* Must be preprocessed by cpp */

/* Special registers */

#define TRAP_PTR x26
#define ALLOC_PTR x27
#define ALLOC_LIMIT x28
#define ARG x15
#define TMP x16
#define TMP2 x17

/* claude: dropped CFI_STARTPROC/ENDPROC/ADJUST (DWARF call-frame debug
   info) and the __PIC__ branch of ADDRGLOBAL/LOADGLOBAL/STOREGLOBAL
   (GOT-indirect symbol access) -- neither exists elsewhere in this fork
   (no other asmrun/*.S has CFI, and every -target-arch here always
   links -static, never PIC, see configure). Also dropped the PROFILE
   hook (gprofile support doesn't exist in this fork's Clflags either,
   see amd64/emit.mlp's own emit_profile removal). */

#define ADDRGLOBAL(reg,symb) \
	adrp	reg, symb; \
	add	reg, reg, #:lo12:symb

#define LOADGLOBAL(reg,symb) \
	adrp	TMP2, symb; \
	ldr	reg, [TMP2, #:lo12:symb]

#define STOREGLOBAL(reg,symb) \
	adrp	TMP2, symb; \
	str	reg, [TMP2, #:lo12:symb]

/* Allocation functions and GC interface */

        .align  2
        .globl  caml_call_gc
caml_call_gc:
    /* Record return address */
	STOREGLOBAL(x30, caml_last_return_address)
.Lcaml_call_gc:
    /* Record lowest stack address */
	mov	TMP, sp
	STOREGLOBAL(TMP, caml_bottom_of_stack)
    /* Set up stack space, saving return address and frame pointer */
    /* (2 regs RA/GP, 24 allocatable int regs, 24 caller-save float regs) * 8 */
        stp     x29, x30, [sp, -400]!
        add     x29, sp, #0
    /* Save allocatable integer registers on the stack, in the order
       given in proc.ml */
	stp	x0, x1, [sp, 16]
        stp     x2, x3, [sp, 32]
        stp     x4, x5, [sp, 48]
        stp     x6, x7, [sp, 64]
        stp     x8, x9, [sp, 80]
        stp     x10, x11, [sp, 96]
        stp     x12, x13, [sp, 112]
        stp     x14, x15, [sp, 128]
        stp     x19, x20, [sp, 144]
        stp     x21, x22, [sp, 160]
        stp     x23, x24, [sp, 176]
        str     x25, [sp, 192]
     /* Save caller-save floating-point registers on the stack
        (callee-saves are preserved by garbage_collection) */
        stp     d0, d1, [sp, 208]
        stp     d2, d3, [sp, 224]
        stp     d4, d5, [sp, 240]
        stp     d6, d7, [sp, 256]
        stp     d16, d17, [sp, 272]
        stp     d18, d19, [sp, 288]
        stp     d20, d21, [sp, 304]
        stp     d22, d23, [sp, 320]
        stp     d24, d25, [sp, 336]
        stp     d26, d27, [sp, 352]
        stp     d28, d29, [sp, 368]
        stp     d30, d31, [sp, 384]
    /* Store pointer to saved integer registers in caml_gc_regs */
	add	TMP, sp, #16
	STOREGLOBAL(TMP, caml_gc_regs)
    /* Save current allocation pointer for debugging purposes */
	STOREGLOBAL(ALLOC_PTR, young_ptr)
    /* Save trap pointer in case an exception is raised during GC */
	STOREGLOBAL(TRAP_PTR, caml_exception_pointer)
    /* Call the garbage collector */
        bl      garbage_collection
    /* Restore registers */
	ldp	x0, x1, [sp, 16]
        ldp     x2, x3, [sp, 32]
        ldp     x4, x5, [sp, 48]
        ldp     x6, x7, [sp, 64]
        ldp     x8, x9, [sp, 80]
        ldp     x10, x11, [sp, 96]
        ldp     x12, x13, [sp, 112]
        ldp     x14, x15, [sp, 128]
        ldp     x19, x20, [sp, 144]
        ldp     x21, x22, [sp, 160]
        ldp     x23, x24, [sp, 176]
        ldr     x25, [sp, 192]
        ldp     d0, d1, [sp, 208]
        ldp     d2, d3, [sp, 224]
        ldp     d4, d5, [sp, 240]
        ldp     d6, d7, [sp, 256]
        ldp     d16, d17, [sp, 272]
        ldp     d18, d19, [sp, 288]
        ldp     d20, d21, [sp, 304]
        ldp     d22, d23, [sp, 320]
        ldp     d24, d25, [sp, 336]
        ldp     d26, d27, [sp, 352]
        ldp     d28, d29, [sp, 368]
        ldp     d30, d31, [sp, 384]
    /* Reload new allocation pointer and allocation limit */
	LOADGLOBAL(ALLOC_PTR, young_ptr)
	LOADGLOBAL(ALLOC_LIMIT, young_limit)
    /* Free stack space and return to caller */
	ldp     x29, x30, [sp], 400
        ret
        .type   caml_call_gc, %function
        .size   caml_call_gc, .-caml_call_gc

        .align  2
        .globl  caml_alloc1
caml_alloc1:
1:      sub     ALLOC_PTR, ALLOC_PTR, #16
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL(x30, caml_last_return_address)
    /* Call GC */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        .type   caml_alloc1, %function
        .size   caml_alloc1, .-caml_alloc1

        .align  2
        .globl  caml_alloc2
caml_alloc2:
1:      sub     ALLOC_PTR, ALLOC_PTR, #24
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL(x30, caml_last_return_address)
    /* Call GC */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        .type   caml_alloc2, %function
        .size   caml_alloc2, .-caml_alloc2

        .align  2
        .globl  caml_alloc3
caml_alloc3:
1:      sub     ALLOC_PTR, ALLOC_PTR, #32
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL(x30, caml_last_return_address)
    /* Call GC */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        .type   caml_alloc3, %function
        .size   caml_alloc3, .-caml_alloc3

    /* claude: renamed from upstream's caml_allocN -- this fork's
       asmcomp/arm64/emit.mlp (Ialloc's non-fastcode/generic-size path,
       matching every other target here) calls this "caml_alloc", with
       the desired size loaded into ARG (x15) beforehand. */
        .align  2
        .globl  caml_alloc
caml_alloc:
1:      sub     ALLOC_PTR, ALLOC_PTR, ARG
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL(x30, caml_last_return_address)
    /* Call GC.  This preserves ARG */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        .type   caml_alloc, %function
        .size   caml_alloc, .-caml_alloc

/* Call a C function from OCaml */
/* Function to call is in ARG */

        .align  2
        .globl  caml_c_call
caml_c_call:
    /* Preserve return address in callee-save register x19 */
        mov     x19, x30
    /* Record lowest stack address and return address */
        STOREGLOBAL(x30, caml_last_return_address)
        add     TMP, sp, #0
        STOREGLOBAL(TMP, caml_bottom_of_stack)
    /* Make the exception handler alloc ptr available to the C code */
	STOREGLOBAL(ALLOC_PTR, young_ptr)
        STOREGLOBAL(TRAP_PTR, caml_exception_pointer)
    /* Call the function */
        blr     ARG
    /* Reload alloc ptr and alloc limit */
	LOADGLOBAL(ALLOC_PTR, young_ptr)
        LOADGLOBAL(ALLOC_LIMIT, young_limit)
    /* Return */
        ret     x19
        .type   caml_c_call, %function
        .size   caml_c_call, .-caml_c_call

/* Start the OCaml program */

        .align  2
        .globl  caml_start_program
caml_start_program:
	ADDRGLOBAL(ARG, caml_program)

/* Code shared with caml_callback* */
/* Address of OCaml code to call is in ARG */
/* Arguments to the OCaml code are in x0...x7 */

.Ljump_to_caml:
    /* Set up stack frame and save callee-save registers */
	stp     x29, x30, [sp, -160]!
	add     x29, sp, #0
        stp     x19, x20, [sp, 16]
        stp     x21, x22, [sp, 32]
        stp     x23, x24, [sp, 48]
        stp     x25, x26, [sp, 64]
        stp     x27, x28, [sp, 80]
	stp     d8, d9, [sp, 96]
        stp     d10, d11, [sp, 112]
        stp     d12, d13, [sp, 128]
        stp     d14, d15, [sp, 144]
    /* Setup a callback link on the stack */
	LOADGLOBAL(x8, caml_bottom_of_stack)
        LOADGLOBAL(x9, caml_last_return_address)
        LOADGLOBAL(x10, caml_gc_regs)
        stp     x8, x9, [sp, -32]!     /* 16-byte alignment */
        str     x10, [sp, 16]
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
	LOADGLOBAL(x8, caml_exception_pointer)
        adr     x9, .Ltrap_handler
        stp     x8, x9, [sp, -16]!
        add     TRAP_PTR, sp, #0
    /* Reload allocation pointers */
	LOADGLOBAL(ALLOC_PTR, young_ptr)
        LOADGLOBAL(ALLOC_LIMIT, young_limit)
    /* Call the OCaml code */
        blr     ARG
.Lcaml_retaddr:
    /* Pop the trap frame, restoring caml_exception_pointer */
	ldr     x8, [sp], 16
        STOREGLOBAL(x8, caml_exception_pointer)
    /* Pop the callback link, restoring the global variables */
.Lreturn_result:
	ldr     x10, [sp, 16]
        ldp     x8, x9, [sp], 32
	STOREGLOBAL(x8, caml_bottom_of_stack)
        STOREGLOBAL(x9, caml_last_return_address)
        STOREGLOBAL(x10, caml_gc_regs)
    /* Update allocation pointer */
	STOREGLOBAL(ALLOC_PTR, young_ptr)
    /* Reload callee-save registers and return address */
        ldp     x19, x20, [sp, 16]
        ldp     x21, x22, [sp, 32]
        ldp     x23, x24, [sp, 48]
        ldp     x25, x26, [sp, 64]
        ldp     x27, x28, [sp, 80]
	ldp     d8, d9, [sp, 96]
        ldp     d10, d11, [sp, 112]
        ldp     d12, d13, [sp, 128]
        ldp     d14, d15, [sp, 144]
	ldp     x29, x30, [sp], 160
    /* Return to C caller */
        ret
        .type   .Lcaml_retaddr, %function
        .size   .Lcaml_retaddr, .-.Lcaml_retaddr
        .type   caml_start_program, %function
        .size   caml_start_program, .-caml_start_program

/* The trap handler for exceptions escaping caml_start_program/
   caml_callback* while control is still inside compiler-generated OCaml
   code (reached via a plain "br" from Lraise's own inline trap-pointer
   unwind in asmcomp/arm64/emit.mlp, or transitively via
   raise_caml_exception below when the exception originates in C).
   claude: upstream tags the bucket (orr x0, x0, #2) and returns
   normally here, matching callback_exn's contract -- but this fork's
   callback()/callback2()/callback3() (asmrun/callback.c) are plain
   value-returning functions with no such tagging convention, so unwind
   the callback link (like .Lreturn_result above) and propagate through
   mlraise instead, exactly like every other target here (see e.g.
   asmrun/amd64.S's .L108 / asmrun/alpha.S's $109). */

        .align  2
.Ltrap_handler:
    /* Save exception pointer -- Lraise's inline unwind already
       restored TRAP_PTR itself to the outer handler, but never touches
       this global copy of it, which caml_c_call/callback needs. */
        STOREGLOBAL(TRAP_PTR, caml_exception_pointer)
    /* Pop the callback link, restoring the global variables */
	ldr     x10, [sp, 16]
        ldp     x8, x9, [sp], 32
	STOREGLOBAL(x8, caml_bottom_of_stack)
        STOREGLOBAL(x9, caml_last_return_address)
        STOREGLOBAL(x10, caml_gc_regs)
	STOREGLOBAL(ALLOC_PTR, young_ptr)
    /* Re-raise the exception through mlraise, so that local C roots are
       cleaned up correctly. Exception bucket is already in x0. */
        bl      mlraise
        .type   .Ltrap_handler, %function
        .size   .Ltrap_handler, .-.Ltrap_handler

/* Raise an exception from C.
   claude: renamed from upstream's caml_raise_exception -- this fork's
   fail.c calls this "raise_caml_exception" (no caml_ prefix), matching
   every other target here. Also dropped the caml_backtrace_active/
   caml_stash_backtrace calls: backtrace support in this fork
   (asmrun/backtrace.c) exists under the older, non-caml_-prefixed names
   (backtrace_active/stash_backtrace) but isn't wired into any target's
   native-code raise path here (see e.g. amd64/alpha's own
   raise_caml_exception, neither of which call it either) -- just unwind
   to the next handler. */

        .align  2
        .globl  raise_caml_exception
raise_caml_exception:
    /* x0 = exception bucket (already in place, our own argument).
       claude: the handler address must be kept in a register other than
       TMP2 across the STOREGLOBAL below -- STOREGLOBAL's own expansion
       uses TMP2 as scratch (adrp TMP2, symb; str reg, [TMP2, #:lo12:
       symb]), so loading it into TMP2 here and branching to TMP2 after
       the STOREGLOBAL branches into whatever adrp last computed instead
       (observed as an immediate segfault jumping into the middle of a
       compiler data table, on the very first exception raised). */
        LOADGLOBAL(TMP, caml_exception_pointer)
        mov     sp, TMP
        ldr     TMP, [sp]
        ldr     x8, [sp, 8]
        add     sp, sp, 16
        STOREGLOBAL(TMP, caml_exception_pointer)
        br      x8
        .type   raise_caml_exception, %function
        .size   raise_caml_exception, .-raise_caml_exception

/* Callback from C to OCaml */

        .align  2
        .globl  callback
callback:
    /* Initial shuffling of arguments (x0 = closure, x1 = first arg) */
	mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
        mov     x1, TMP         /* x1 = closure environment */
        ldr     ARG, [TMP]      /* code pointer */
        b       .Ljump_to_caml
        .type   callback, %function
        .size   callback, .-callback

        .align  2
        .globl  callback2
callback2:
    /* Initial shuffling of arguments (x0 = closure, x1 = arg1, x2 = arg2) */
	mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
	mov     x1, x2          /* x1 = second arg */
        mov     x2, TMP         /* x2 = closure environment */
	ADDRGLOBAL(ARG, caml_apply2)
        b       .Ljump_to_caml
        .type   callback2, %function
        .size   callback2, .-callback2

        .align  2
        .globl  callback3
callback3:
    /* Initial shuffling of arguments */
    /* (x0 = closure, x1 = arg1, x2 = arg2, x3 = arg3) */
        mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
        mov     x1, x2          /* x1 = second arg */
        mov     x2, x3          /* x2 = third arg */
        mov     x3, TMP         /* x3 = closure environment */
	ADDRGLOBAL(ARG, caml_apply3)
        b       .Ljump_to_caml
        .type   callback3, %function
        .size   callback3, .-callback3

/* claude: renamed from upstream's caml_ml_array_bound_error, matching
   this fork's asmcomp/arm64/emit.mlp, which (like every other target
   here) calls the C function directly by its own name (array_bound_error,
   asmrun/array.c) rather than through a caml_c_call trampoline --
   array_bound_error() is fatal (calls fatal_error(), never returns), so
   it needs none of the young_ptr/exception_pointer bookkeeping
   caml_c_call would set up. This whole trampoline is accordingly
   unneeded and dropped; see amd64/emit.mlp's identical fix. */

        .data
        .align  3
        .globl  system_frametable
system_frametable:
        .quad   1               /* one descriptor */
        .quad   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        .type   system_frametable, %object
        .size   system_frametable, .-system_frametable
