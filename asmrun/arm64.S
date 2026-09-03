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
   info) and the PROFILE hook (gprofile support doesn't exist in this
   fork's Clflags either, see amd64/emit.mlp's own emit_profile removal)
   -- neither exists elsewhere in this fork (no other asmrun/*.S has
   CFI). */

/* claude: Mach-O requires a leading underscore on every C-visible symbol
   (clang adds it automatically when compiling byterun/*.c and asmrun/*.c,
   and asmcomp/arm64/emit.mlp's own symbol_prefix does the same for
   OCaml-generated code -- see there) -- SYM() applies the same prefix
   here so hand-written references agree with both. -DSYS_$(SYSTEM) is
   already passed by asmrun/Makefile's FLAGS, so SYS_macosx is available
   for free. Applied at each *call site* below (matching upstream real
   OCaml's own G() macro for its arm64-on-macOS port), not baked into
   ADDRGLOBAL/LOADGLOBAL/STOREGLOBAL's own body -- those are now GNU-as
   ".macro" blocks (see next comment), and a cpp macro like SYM() cannot
   safely wrap a ".macro"-syntax "\symb" parameter reference (cpp runs
   before the assembler ever parses ".macro", so it sees the literal
   two-token text "\" "symb", not something SYM(...) can token-paste). */
#ifdef SYS_macosx
#define SYM(x) _##x
#else
#define SYM(x) x
#endif

/* claude: .type/.size are ELF-only directives -- Mach-O's assembler
   doesn't have them (nor a need for them: Mach-O tells functions from
   data via the section they're in, not a per-symbol directive), so these
   expand to nothing under SYS_macosx. Same idea as asmrun/i386.S's
   #if defined(SYS_linux_elf) / asmrun/sparc.S's
   #if defined(SYS_sunos) || defined(SYS_bsd) precedent, just as macros
   instead of wrapping every site in #ifndef. Safe as plain (single
   directive, no ";"-joined multi-statement body) cpp macros -- see the
   next comment for why ADDRGLOBAL/LOADGLOBAL/STOREGLOBAL can't be. */
#ifdef SYS_macosx
#define FUNCTION_TYPE(name)
#define FUNCTION_SIZE(name)
#else
#define FUNCTION_TYPE(name) .type SYM(name), %function
#define FUNCTION_SIZE(name) .size SYM(name), .-SYM(name)
#endif

/* claude: macros to load/store/take-the-address-of a global variable,
   ported from upstream real OCaml's actual arm64-on-macOS support
   (asmcomp/arm64/emit.mlp & asmrun/arm64.S, ~2014-2016, "iOS Porting
   Team"/Gerd Stolpmann commits ff43c71263/22fcdf723c/d189dcef62), for
   two reasons confirmed by testing on real Apple Silicon hardware:

   1. Mach-O's assembler has no ELF-style ":lo12:" operand-modifier
      syntax at all ("unknown AArch64 fixup kind") -- it needs the
      Apple-specific "symbol@PAGE"/"symbol@PAGEOFF" (direct) or
      "symbol@GOTPAGE"/"symbol@GOTPAGEOFF" (GOT-indirect) suffix syntax
      instead. Direct @PAGE/@PAGEOFF addressing *would* still be
      correct here (confirmed empirically, even across separately
      linked object files, since this fork never builds a dylib or
      loadable plugin) -- but matching upstream's own proven choice of
      always going through the GOT on macosx, rather than the untested
      direct-@PAGE alternative this fork could otherwise get away with.

   2. This originally used plain cpp "#define ... \" multi-statement
      macros (like every other asmrun/*.S here) -- but Apple's
      assembler treats ";" as a comment leader, not a statement
      separator (confirmed empirically: "mov x0, #1; mov x1, #2"
      assembles ONLY the first "mov"). cpp joins a "\"-continued
      #define's body onto one output line with ";" between statements,
      so on Mach-O every statement after the first in such a macro
      silently vanishes as a comment -- a real, still-reproducible
      instance of the exact bug upstream's real port had to work around.
      Using GNU-as ".macro"/".endm" instead sidesteps this entirely:
      each body line stays a distinct assembler line, no ";"-joining
      involved, and it works unchanged on both ELF and Mach-O. */
#if defined(SYS_macosx)
        .macro ADDRGLOBAL reg, symb
        adrp    TMP2, \symb@GOTPAGE
        ldr     \reg, [TMP2, \symb@GOTPAGEOFF]
        .endm

        .macro LOADGLOBAL reg, symb
        ADDRGLOBAL TMP2, \symb
        ldr     \reg, [TMP2]
        .endm

        .macro STOREGLOBAL reg, symb
        ADDRGLOBAL TMP2, \symb
        str     \reg, [TMP2]
        .endm
#else
        .macro ADDRGLOBAL reg, symb
        adrp    \reg, \symb
        add     \reg, \reg, #:lo12:\symb
        .endm

        .macro LOADGLOBAL reg, symb
        adrp    TMP2, \symb
        ldr     \reg, [TMP2, #:lo12:\symb]
        .endm

        .macro STOREGLOBAL reg, symb
        adrp    TMP2, \symb
        str     \reg, [TMP2, #:lo12:\symb]
        .endm
#endif

/* Allocation functions and GC interface */

        .align  2
        .globl  SYM(caml_call_gc)
SYM(caml_call_gc):
    /* Record return address */
	STOREGLOBAL x30, SYM(caml_last_return_address)
.Lcaml_call_gc:
    /* Record lowest stack address */
	mov	TMP, sp
	STOREGLOBAL TMP, SYM(caml_bottom_of_stack)
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
	STOREGLOBAL TMP, SYM(caml_gc_regs)
    /* Save current allocation pointer for debugging purposes */
	STOREGLOBAL ALLOC_PTR, SYM(young_ptr)
    /* Save trap pointer in case an exception is raised during GC */
	STOREGLOBAL TRAP_PTR, SYM(caml_exception_pointer)
    /* Call the garbage collector */
        bl      SYM(garbage_collection)
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
	LOADGLOBAL ALLOC_PTR, SYM(young_ptr)
	LOADGLOBAL ALLOC_LIMIT, SYM(young_limit)
    /* Free stack space and return to caller */
	ldp     x29, x30, [sp], 400
        ret
        FUNCTION_TYPE(caml_call_gc)
        FUNCTION_SIZE(caml_call_gc)

        .align  2
        .globl  SYM(caml_alloc1)
SYM(caml_alloc1):
1:      sub     ALLOC_PTR, ALLOC_PTR, #16
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL x30, SYM(caml_last_return_address)
    /* Call GC */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        FUNCTION_TYPE(caml_alloc1)
        FUNCTION_SIZE(caml_alloc1)

        .align  2
        .globl  SYM(caml_alloc2)
SYM(caml_alloc2):
1:      sub     ALLOC_PTR, ALLOC_PTR, #24
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL x30, SYM(caml_last_return_address)
    /* Call GC */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        FUNCTION_TYPE(caml_alloc2)
        FUNCTION_SIZE(caml_alloc2)

        .align  2
        .globl  SYM(caml_alloc3)
SYM(caml_alloc3):
1:      sub     ALLOC_PTR, ALLOC_PTR, #32
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL x30, SYM(caml_last_return_address)
    /* Call GC */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        FUNCTION_TYPE(caml_alloc3)
        FUNCTION_SIZE(caml_alloc3)

    /* claude: renamed from upstream's caml_allocN -- this fork's
       asmcomp/arm64/emit.mlp (Ialloc's non-fastcode/generic-size path,
       matching every other target here) calls this "caml_alloc", with
       the desired size loaded into ARG (x15) beforehand. */
        .align  2
        .globl  SYM(caml_alloc)
SYM(caml_alloc):
1:      sub     ALLOC_PTR, ALLOC_PTR, ARG
        cmp     ALLOC_PTR, ALLOC_LIMIT
        b.lo    2f
        ret
2:      stp     x29, x30, [sp, -16]!
        add     x29, sp, #0
    /* Record return address */
	STOREGLOBAL x30, SYM(caml_last_return_address)
    /* Call GC.  This preserves ARG */
        bl      .Lcaml_call_gc
    /* Restore return address */
	ldp     x29, x30, [sp], 16
    /* Try again */
        b       1b
        FUNCTION_TYPE(caml_alloc)
        FUNCTION_SIZE(caml_alloc)

/* Call a C function from OCaml */
/* Function to call is in ARG */

        .align  2
        .globl  SYM(caml_c_call)
SYM(caml_c_call):
    /* Preserve return address in callee-save register x19 */
        mov     x19, x30
    /* Record lowest stack address and return address */
        STOREGLOBAL x30, SYM(caml_last_return_address)
        add     TMP, sp, #0
        STOREGLOBAL TMP, SYM(caml_bottom_of_stack)
    /* Make the exception handler alloc ptr available to the C code */
	STOREGLOBAL ALLOC_PTR, SYM(young_ptr)
        STOREGLOBAL TRAP_PTR, SYM(caml_exception_pointer)
    /* Call the function */
        blr     ARG
    /* Reload alloc ptr and alloc limit */
	LOADGLOBAL ALLOC_PTR, SYM(young_ptr)
        LOADGLOBAL ALLOC_LIMIT, SYM(young_limit)
    /* Return */
        ret     x19
        FUNCTION_TYPE(caml_c_call)
        FUNCTION_SIZE(caml_c_call)

/* Start the OCaml program */

        .align  2
        .globl  SYM(caml_start_program)
SYM(caml_start_program):
	ADDRGLOBAL ARG, SYM(caml_program)

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
	LOADGLOBAL x8, SYM(caml_bottom_of_stack)
        LOADGLOBAL x9, SYM(caml_last_return_address)
        LOADGLOBAL x10, SYM(caml_gc_regs)
        stp     x8, x9, [sp, -32]!     /* 16-byte alignment */
        str     x10, [sp, 16]
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
	LOADGLOBAL x8, SYM(caml_exception_pointer)
        adr     x9, .Ltrap_handler
        stp     x8, x9, [sp, -16]!
        add     TRAP_PTR, sp, #0
    /* Reload allocation pointers */
	LOADGLOBAL ALLOC_PTR, SYM(young_ptr)
        LOADGLOBAL ALLOC_LIMIT, SYM(young_limit)
    /* Call the OCaml code */
        blr     ARG
.Lcaml_retaddr:
    /* Pop the trap frame, restoring caml_exception_pointer */
	ldr     x8, [sp], 16
        STOREGLOBAL x8, SYM(caml_exception_pointer)
    /* Pop the callback link, restoring the global variables */
.Lreturn_result:
	ldr     x10, [sp, 16]
        ldp     x8, x9, [sp], 32
	STOREGLOBAL x8, SYM(caml_bottom_of_stack)
        STOREGLOBAL x9, SYM(caml_last_return_address)
        STOREGLOBAL x10, SYM(caml_gc_regs)
    /* Update allocation pointer */
	STOREGLOBAL ALLOC_PTR, SYM(young_ptr)
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
        FUNCTION_TYPE(.Lcaml_retaddr)
        FUNCTION_SIZE(.Lcaml_retaddr)
        FUNCTION_TYPE(caml_start_program)
        FUNCTION_SIZE(caml_start_program)

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
        STOREGLOBAL TRAP_PTR, SYM(caml_exception_pointer)
    /* Pop the callback link, restoring the global variables */
	ldr     x10, [sp, 16]
        ldp     x8, x9, [sp], 32
	STOREGLOBAL x8, SYM(caml_bottom_of_stack)
        STOREGLOBAL x9, SYM(caml_last_return_address)
        STOREGLOBAL x10, SYM(caml_gc_regs)
	STOREGLOBAL ALLOC_PTR, SYM(young_ptr)
    /* Re-raise the exception through mlraise, so that local C roots are
       cleaned up correctly. Exception bucket is already in x0. */
        bl      SYM(mlraise)
        FUNCTION_TYPE(.Ltrap_handler)
        FUNCTION_SIZE(.Ltrap_handler)

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
        .globl  SYM(raise_caml_exception)
SYM(raise_caml_exception):
    /* x0 = exception bucket (already in place, our own argument).
       claude: the handler address must be kept in a register other than
       TMP2 across the STOREGLOBAL below -- STOREGLOBAL's own expansion
       uses TMP2 as scratch (on both the direct and GOT-indirect
       variants of ADDRGLOBAL above), so loading it into TMP2 here and
       branching to TMP2 after the STOREGLOBAL branches into whatever
       address ADDRGLOBAL last computed instead (observed as an
       immediate segfault jumping into the middle of a compiler data
       table, on the very first exception raised). */
        LOADGLOBAL TMP, SYM(caml_exception_pointer)
        mov     sp, TMP
        ldr     TMP, [sp]
        ldr     x8, [sp, 8]
        add     sp, sp, 16
        STOREGLOBAL TMP, SYM(caml_exception_pointer)
        br      x8
        FUNCTION_TYPE(raise_caml_exception)
        FUNCTION_SIZE(raise_caml_exception)

/* Callback from C to OCaml */

        .align  2
        .globl  SYM(callback)
SYM(callback):
    /* Initial shuffling of arguments (x0 = closure, x1 = first arg) */
	mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
        mov     x1, TMP         /* x1 = closure environment */
        ldr     ARG, [TMP]      /* code pointer */
        b       .Ljump_to_caml
        FUNCTION_TYPE(callback)
        FUNCTION_SIZE(callback)

        .align  2
        .globl  SYM(callback2)
SYM(callback2):
    /* Initial shuffling of arguments (x0 = closure, x1 = arg1, x2 = arg2) */
	mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
	mov     x1, x2          /* x1 = second arg */
        mov     x2, TMP         /* x2 = closure environment */
	ADDRGLOBAL ARG, SYM(caml_apply2)
        b       .Ljump_to_caml
        FUNCTION_TYPE(callback2)
        FUNCTION_SIZE(callback2)

        .align  2
        .globl  SYM(callback3)
SYM(callback3):
    /* Initial shuffling of arguments */
    /* (x0 = closure, x1 = arg1, x2 = arg2, x3 = arg3) */
        mov     TMP, x0
        mov     x0, x1          /* x0 = first arg */
        mov     x1, x2          /* x1 = second arg */
        mov     x2, x3          /* x2 = third arg */
        mov     x3, TMP         /* x3 = closure environment */
	ADDRGLOBAL ARG, SYM(caml_apply3)
        b       .Ljump_to_caml
        FUNCTION_TYPE(callback3)
        FUNCTION_SIZE(callback3)

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
        .globl  SYM(system_frametable)
SYM(system_frametable):
        .quad   1               /* one descriptor */
        .quad   .Lcaml_retaddr  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
#ifndef SYS_macosx
        .type   system_frametable, %object
        .size   system_frametable, .-system_frametable
#endif
