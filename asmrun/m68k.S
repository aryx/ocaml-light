|***********************************************************************
|*                                                                     *
|*                           Objective Caml                            *
|*                                                                     *
|*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
|*                                                                     *
|*  Copyright 1996 Institut National de Recherche en Informatique et   *
|*  Automatique.  Distributed only by permission.                      *
|*                                                                     *
|***********************************************************************

| Asm part of the runtime system, Motorola 68k processor

| claude: ocaml-light only wires up the m68k-linux-gnu- cross toolchain
| (see configure's -target-arch m68k), i.e. ELF gas -- unlike the a.out
| -style system(s) this file originally targeted, that means: no leading
| "_" on C symbol names (verified against what m68k-linux-gnu-gcc itself
| names a plain C function as -- see asmcomp/m68k/emit.mlp's emit_symbol
| for the same fix on the compiler side), and "%"-prefixed register
| names (GNU as's MIT-syntax m68k port rejects the bare "a7"/"d0"/"fp0"
| this file used, e.g. "syntax error -- statement `movel a7@,d0'
| ignored").

        .comm   caml_requested_size, 4

| Allocation

        .text
        .globl  caml_call_gc
        .globl  caml_alloc1
        .globl  caml_alloc2
        .globl  caml_alloc3
        .globl  caml_alloc

caml_call_gc:
    | Save desired size
        movel   %d5, caml_requested_size
    | Record lowest stack address and return address
        movel   %a7@, caml_last_return_address
        movel   %a7, %d5
        addql   #4, %d5
        movel   %d5, caml_bottom_of_stack
    | Record current allocation pointer (for debugging)
        movel   %d6, young_ptr
    | Save all regs used by the code generator
        movel   %d4, %a7@-
        movel   %d3, %a7@-
        movel   %d2, %a7@-
        movel   %d1, %a7@-
        movel   %d0, %a7@-
        movel   %a6, %a7@-
        movel   %a5, %a7@-
        movel   %a4, %a7@-
        movel   %a3, %a7@-
        movel   %a2, %a7@-
        movel   %a1, %a7@-
        movel   %a0, %a7@-
        movel   %a7, caml_gc_regs
        fmovem  %fp0-%fp7, %a7@-
    | Call the garbage collector
        jbsr    garbage_collection
    | Restore all regs used by the code generator
        fmovem  %a7@+, %fp0-%fp7
        movel   %a7@+, %a0
        movel   %a7@+, %a1
        movel   %a7@+, %a2
        movel   %a7@+, %a3
        movel   %a7@+, %a4
        movel   %a7@+, %a5
        movel   %a7@+, %a6
        movel   %a7@+, %d0
        movel   %a7@+, %d1
        movel   %a7@+, %d2
        movel   %a7@+, %d3
        movel   %a7@+, %d4
    | Reload allocation pointer and allocate block
        movel   young_ptr, %d6
        subl    caml_requested_size, %d6
    | Return to caller
        rts

caml_alloc1:
        subql   #8, %d6
        cmpl    young_limit, %d6
        bcs     L100
        rts
L100:   moveq   #8, %d5
        bra     caml_call_gc

caml_alloc2:
        subl    #12, %d6
        cmpl    young_limit, %d6
        bcs     L101
        rts
L101:   moveq   #12, %d5
        bra     caml_call_gc

caml_alloc3:
        subl    #16, %d6
        cmpl    young_limit, %d6
        bcs     L102
        rts
L102:   moveq   #16, %d5
        bra     caml_call_gc

caml_alloc:
        subl    %d5, %d6
        cmpl    young_limit, %d6
        bcs     caml_call_gc
        rts

| Call a C function from Caml

        .globl  caml_c_call

caml_c_call:
    | Record lowest stack address and return address
        movel   %a7@+, caml_last_return_address
        movel   %a7, caml_bottom_of_stack
    | Save allocation pointer and exception pointer
        movel   %d6, young_ptr
        movel   %d7, caml_exception_pointer
    | Call the function (address in a0)
        jbsr    %a0@
    | Reload allocation pointer
        movel   young_ptr, %d6
    | Return to caller
        movel   caml_last_return_address, %a1
        jmp     %a1@

| Start the Caml program

        .globl  caml_start_program

caml_start_program:
    | claude: the shared epilogue below (L107) ends with "unlk %fp",
    | which undoes a "link %fp" -- callback/callback2/callback3 (the
    | other entry points that fall through to L107) all do that at
    | their own entry, but this one originally didn't, so the epilogue's
    | unlk clobbered %sp with whatever garbage happened to be in %fp
    | (%a6) on entry, corrupting the stack for every native-compiled
    | program's very first call into the runtime (main() -> caml_main()
    | -> here). Observed as a segfault inside __libc_start_main_impl,
    | i.e. control landing on essentially-random code after this
    | function's "rts" popped a bogus return address off the wrecked
    | stack.
        link    %a6, #0
    | Save callee-save registers
        moveml  %a2-%a6/%d2-%d7, %a7@-
        fmovem  %fp2-%fp7, %a7@-
    | Initial code point is caml_program
        lea     caml_program, %a5

| Code shared between caml_start_program and callback*

L106:
    | Build a callback link
        movel   caml_gc_regs, %a7@-
        movel   caml_last_return_address, %a7@-
        movel   caml_bottom_of_stack, %a7@-
    | Build an exception handler
        pea     L108
        movel   caml_exception_pointer, %a7@-
        movel   %a7, %d7
    | Load allocation pointer
        movel   young_ptr, %d6
    | Call the Caml code
        jbsr    %a5@
L107:
    | Move result where C code expects it
        movel   %a0, %d0
    | Save allocation pointer
        movel   %d6, young_ptr
    | Pop the exception handler
        movel   %a7@+, caml_exception_pointer
        addql   #4, %a7
    | Pop the callback link, restoring the global variables
    | used by caml_c_call
        movel   %a7@+, caml_bottom_of_stack
        movel   %a7@+, caml_last_return_address
        movel   %a7@+, caml_gc_regs
    | Restore callee-save registers and return
        fmovem  %a7@+, %fp2-%fp7
        moveml  %a7@+, %a2-%a6/%d2-%d7
        unlk    %a6
        rts
L108:
    | Exception handler
    | Save allocation pointer and exception pointer
        movel   %d6, young_ptr
        movel   %d7, caml_exception_pointer
    | Pop the callback link, restoring the global variables
    | used by caml_c_call
        movel   %a7@+, caml_bottom_of_stack
        movel   %a7@+, caml_last_return_address
        movel   %a7@+, caml_gc_regs
    | Re-raise the exception through mlraise,
    | so that local C roots are cleaned up correctly.
        movel   %a0, %a7@-       | exn bucket is the argument
        jbsr    mlraise        | never returns

| Raise an exception from C

        .globl  raise_caml_exception
raise_caml_exception:
        movel   %a7@(4), %a0     | exception bucket
        movel   young_ptr, %d6
        movel   caml_exception_pointer, %a7
        movel   %a7@+, %d7
        rts

| Callback from C to Caml

        .globl  callback
callback:
        link    %a6, #0
    | Save callee-save registers
        moveml  %a2-%a6/%d2-%d7, %a7@-
        fmovem  %fp2-%fp7, %a7@-
    | Initial loading of arguments
        movel   %a6@(8), %a1     | closure
        movel   %a6@(12), %a0    | argument
        movel   %a1@(0), %a5     | code pointer
        bra     L106

        .globl  callback2
callback2:
        link    %a6, #0
    | Save callee-save registers
        moveml  %a2-%a6/%d2-%d7, %a7@-
        fmovem  %fp2-%fp7, %a7@-
    | Initial loading of arguments
        movel   %a6@(8), %a2     | closure
        movel   %a6@(12), %a0    | first argument
        movel   %a6@(16), %a1    | second argument
        lea     caml_apply2, %a5 | code pointer
        bra     L106

        .globl  callback3
callback3:
        link    %a6, #0
    | Save callee-save registers
        moveml  %a2-%a6/%d2-%d7, %a7@-
        fmovem  %fp2-%fp7, %a7@-
    | Initial loading of arguments
        movel   %a6@(8), %a3     | closure
        movel   %a6@(12), %a0    | first argument
        movel   %a6@(16), %a1    | second argument
        movel   %a6@(20), %a2    | third argument
        lea     caml_apply3, %a5 | code pointer
        bra     L106

        .data
        .globl  system_frametable
system_frametable:
        .long   1               | one descriptor
        .long   L107            | return address into callback
        .word   -1              | negative frame size => use callback link
        .word   0               | no roots here
