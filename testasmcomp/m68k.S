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

| claude: same fixes as asmrun/m68k.S -- no leading "_" on C symbol
| names (ELF, not this file's original a.out-style target), and
| "%"-prefixed register names (GNU as's m68k MIT syntax requires it).

| call_gen_code is used with the following types:
|       unit -> int
|       int -> int
|       int -> double
|       int * int * address -> void
|       int * int -> void
|       unit -> unit
| and we need a special case for int -> double
|
| claude: the comment above used to say "arg1 -> d0, arg2 -> d1,
| arg3 -> a0", and the code below matched that -- but that's not what
| asmcomp/m68k/proc.ml's loc_parameters actually does: Int and Addr
| arguments share one register sequence, a0/a1/a2/..., never the data
| registers (see calling_conventions in proc.ml, and the register-map
| comment at its top: "A0 - A6 address registers"). Passing arguments in
| d0/d1/a0 left the callee reading its real arguments from whatever
| garbage happened to be in a0/a1/a2, which segfaulted (or, for the
| int->double case below, silently computed nonsense) for every test
| program taking at least one argument. Likewise, the result of a call
| comes back in %a0 (loc_results, same register sequence) -- not %d0 --
| so it has to be moved there explicitly for the C caller, which (per
| the ordinary m68k C ABI) reads an int return value from %d0.

        .text
        .globl  call_gen_code
call_gen_code:
        link    %a6, #0
        | claude: this used to say plain "movem" here (no size suffix),
        | which GNU as defaults to word (16-bit) size -- corrupting the
        | upper 16 bits of every saved register on restore and, worse,
        | only reserving half as much stack space as the matching pop
        | expects. call_gen_code_float already used "moveml" (32-bit)
        | correctly; this one didn't, and every non-float test program
        | (fib, tak, quicksort, soli, checkbound -- none of which
        | exercise call_gen_code_float) segfaulted as a result.
        moveml  %d2-%d7/%a2-%a6, %a7@-
        fmovem  %fp2-%fp7, %a7@-
        movel   %a6@(8), %a3      | function to call
        movel   %a6@(12), %a0    | first OCaml argument
        movel   %a6@(16), %a1    | second OCaml argument
        movel   %a6@(20), %a2    | third OCaml argument
        jsr     %a3@
        movel   %a0, %d0         | OCaml result (%a0) -> C int result (%d0)
        fmovem  %a7@+, %fp2-%fp7
        moveml  %a7@+, %d2-%d7/%a2-%a6
        unlk    %a6
        rts

        .globl  call_gen_code_float
call_gen_code_float:
        link    %a6, #0
        moveml  %d2-%d7/%a2-%a6, %a7@-
        fmovem  %fp2-%fp7, %a7@-
        movel   %a6@(8), %a3     | function to call
        movel   %a6@(12), %a0    | (only) OCaml argument
        jsr     %a3@
                                 | OCaml float result already in %fp0,
                                 | which is also where the C ABI expects
                                 | this function's own double result
        fmovem  %a7@+, %fp2-%fp7
        moveml  %a7@+, %d2-%d7/%a2-%a6
        unlk    %a6
        rts

        .globl  caml_c_call
caml_c_call:
        jmp     %a0@
