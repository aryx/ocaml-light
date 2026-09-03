/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2013 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* claude: Mach-O (macOS/Apple Silicon) prefixes identifiers with _,
   unlike Linux with ELF binaries -- same convention already used by
   this directory's own i386.S (see there), and by asmcomp/arm64/
   emit.mlp's symbol_prefix / asmrun/arm64.S's SYM() for the real
   runtime. -DSYS_$(SYSTEM) is passed by this Makefile's ASPPFLAGS. */
#ifdef SYS_macosx
#define G(x) _##x
#else
#define G(x) x
#endif

        .globl  G(call_gen_code)
        .align  2
G(call_gen_code):
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
    /* Shuffle arguments */
	mov     x8, x0
        mov     x0, x1
        mov     x1, x2
        mov     x2, x3
        mov     x3, x4
    /* Call generated asm */
        blr     x8
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
        ret

        .globl  G(caml_c_call)
        .align  2
G(caml_c_call):
        br	x15
