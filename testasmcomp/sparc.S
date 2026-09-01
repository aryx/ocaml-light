/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* claude: SunOS/BSD prefix C identifiers with "_"; Solaris and Linux
   (our -target-arch sparc target, via SYS_linux_elf) do not -- same
   naming split as asmrun/sparc.S's own "#if defined(SYS_sunos) ||
   defined(SYS_bsd)" test. The original test here ("#ifndef SYS_solaris",
   prefixed by default, unprefixed only for Solaris) got this backwards
   for every system other than the two it explicitly named: it happened
   to work for the historic Sun/BSD/Solaris targets, but left our actual
   target, SYS_linux_elf, wrongly prefixed. */
#if defined(SYS_sunos) || defined(SYS_bsd)
#define Call_gen_code _call_gen_code
#define Caml_c_call _caml_c_call
#else
#define Call_gen_code call_gen_code
#define Caml_c_call caml_c_call
#endif

        .global Call_gen_code
Call_gen_code:
        save    %sp, -96, %sp
        mov     %i0, %l0
        mov     %i1, %i0
        mov     %i2, %i1
        mov     %i3, %i2
        mov     %i4, %i3
        mov     %i5, %i4
        call    %l0
        nop
        mov     %o0, %i0
        ret
        restore

        .global Caml_c_call
Caml_c_call:
        jmp     %g4
        nop
