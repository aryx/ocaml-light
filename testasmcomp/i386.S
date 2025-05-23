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

/* $Id$ */

/* Linux with ELF binaries does not prefix identifiers with _.
   Linux with a.out binaries, FreeBSD, and NextStep do. */

#ifdef SYS_linux_elf
#define G(x) x
#define FUNCTION_ALIGN 16
#else
#define G(x) _##x
#define FUNCTION_ALIGN 4
#endif

        .globl  G(call_gen_code)
        .align  FUNCTION_ALIGN
G(call_gen_code):
        pushl %ebp
        movl %esp,%ebp
        pushl %ebx
        pushl %esi
        pushl %edi
        movl 12(%ebp),%eax
        movl 16(%ebp),%ebx
        movl 20(%ebp),%ecx
        movl 24(%ebp),%edx
        call *8(%ebp)
        popl %edi
        popl %esi
        popl %ebx
        popl %ebp
        ret

        .globl  G(caml_c_call)
        .align  FUNCTION_ALIGN
G(caml_c_call):
        ffree   %st(0)
        ffree   %st(1)
        ffree   %st(2)
        ffree   %st(3)
        jmp     *%eax

        .comm   G(caml_exception_pointer), 4
        .comm   G(young_ptr), 4
        .comm   G(young_start), 4
