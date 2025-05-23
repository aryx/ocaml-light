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

/* $Id: i386.S,v 1.25 1997/11/27 16:27:52 xleroy Exp $ */

/* Asm part of the runtime system, Intel 386 processor */
/* Must be preprocessed by cpp */

/* Linux with ELF binaries do not prefix identifiers with _.
   Linux with a.out binaries, FreeBSD, and NextStep do. */

#if defined(SYS_linux_elf)
#define G(x) x
#define FUNCTION_ALIGN 4
#else
#define G(x) _##x
#define FUNCTION_ALIGN 2
#endif

/* Allocation */

        .text
        .globl  G(caml_call_gc)
        .globl  G(caml_alloc1)
        .globl  G(caml_alloc2)
        .globl  G(caml_alloc3)
        .globl  G(caml_alloc)

G(caml_call_gc):
    /* Record lowest stack address and return address */
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
    /* Build array of registers, save it into caml_gc_regs */
L105:   pushl   %ebp
        pushl   %edi
        pushl   %esi
        pushl   %edx
        pushl   %ecx
        pushl   %ebx
        pushl   %eax
        movl    %esp, G(caml_gc_regs)
    /* Call the garbage collector */
        call    G(garbage_collection)
    /* Restore all regs used by the code generator */
        popl    %eax
        popl    %ebx
        popl    %ecx
        popl    %edx
        popl    %esi
        popl    %edi
        popl    %ebp
    /* Return to caller */
        ret

        .align  FUNCTION_ALIGN
G(caml_alloc1):
        movl    G(young_ptr), %eax
        subl    $8, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_limit), %eax
        jb      L100
        ret
L100:   movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        call    L105
        jmp     G(caml_alloc1)

        .align  FUNCTION_ALIGN
G(caml_alloc2):
        movl    G(young_ptr), %eax
        subl    $12, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_limit), %eax
        jb      L101
        ret
L101:   movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        call    L105
        jmp     G(caml_alloc2)

        .align  FUNCTION_ALIGN
G(caml_alloc3):
        movl    G(young_ptr), %eax
        subl    $16, %eax
        movl    %eax, G(young_ptr)
        cmpl    G(young_limit), %eax
        jb      L102
        ret
L102:   movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        call    L105
        jmp     G(caml_alloc3)

        .align  FUNCTION_ALIGN
G(caml_alloc):
        subl    G(young_ptr), %eax      /* eax = size - young_ptr */
        negl    %eax                    /* eax = young_ptr - size */
        cmpl    G(young_limit), %eax
        jb      L103
        movl    %eax, G(young_ptr)
        ret
L103:   subl    G(young_ptr), %eax      /* eax = - size */
        negl    %eax                    /* eax = size */
        pushl   %eax                    /* save desired size */
        subl    %eax, G(young_ptr)      /* must update young_ptr */
        movl    4(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    8(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        call    L105
        popl    %eax                    /* recover desired size */
        jmp     G(caml_alloc)

/* Call a C function from Caml */

        .globl  G(caml_c_call)
        .align  FUNCTION_ALIGN
G(caml_c_call):
    /* Record lowest stack address and return address */
        movl    (%esp), %edx
        movl    %edx, G(caml_last_return_address)
        leal    4(%esp), %edx
        movl    %edx, G(caml_bottom_of_stack)
    /* Call the function (address in %eax) */
        jmp     *%eax

/* Start the Caml program */

        .globl  G(caml_start_program)
        .align  FUNCTION_ALIGN
G(caml_start_program):
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial entry point is caml_program */
        movl    $ G(caml_program), %esi
    /* Common code for caml_start_program and callback* */
L106:
    /* Build a callback link */
        pushl   G(caml_gc_regs)
        pushl   G(caml_last_return_address)
        pushl   G(caml_bottom_of_stack)
    /* Build an exception handler */
        pushl   $L108
        pushl   G(caml_exception_pointer)
        movl    %esp, G(caml_exception_pointer)
    /* Call the Caml code */
        call    *%esi
L107:
    /* Pop the exception handler */
        popl    G(caml_exception_pointer)
        popl    %esi    /* dummy register */
    /* Pop the callback link, restoring the global variables */
        popl    G(caml_bottom_of_stack)
        popl    G(caml_last_return_address)
        popl    G(caml_gc_regs)
    /* Restore callee-save registers. */
        popl    %ebp
        popl    %edi
        popl    %esi
        popl    %ebx
    /* Return to caller. */
        ret
L108:
    /* Exception handler*/
    /* Pop the callback link, restoring the global variables */
        popl    G(caml_bottom_of_stack)
        popl    G(caml_last_return_address)
        popl    G(caml_gc_regs)
    /* Re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
        pushl   %eax            /* exn bucket is the argument */
        call    G(mlraise)      /* never returns */

/* Raise an exception from C */

        .globl  G(raise_caml_exception)
        .align  FUNCTION_ALIGN
G(raise_caml_exception):
        movl    4(%esp), %eax
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer)
        ret

/* Callback from C to Caml */

        .globl  G(callback)
        .align  FUNCTION_ALIGN
G(callback):
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial loading of arguments */
        movl    20(%esp), %ebx   /* closure */
        movl    24(%esp), %eax   /* argument */
        movl    0(%ebx), %esi    /* code pointer */
        jmp     L106

        .globl  G(callback2)
        .align  FUNCTION_ALIGN
G(callback2):
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial loading of arguments */
        movl    20(%esp), %ecx   /* closure */
        movl    24(%esp), %eax   /* first argument */
        movl    28(%esp), %ebx   /* second argument */
        movl    $ G(caml_apply2), %esi   /* code pointer */
        jmp     L106

        .globl  G(callback3)
        .align  FUNCTION_ALIGN
G(callback3):
    /* Save callee-save registers */
        pushl   %ebx
        pushl   %esi
        pushl   %edi
        pushl   %ebp
    /* Initial loading of arguments */
        movl    20(%esp), %edx   /* closure */
        movl    24(%esp), %eax   /* first argument */
        movl    28(%esp), %ebx   /* second argument */
        movl    32(%esp), %ecx   /* third argument */
        movl    $ G(caml_apply3), %esi   /* code pointer */
        jmp     L106

        .data
        .globl  G(system_frametable)
G(system_frametable):
        .long   1               /* one descriptor */
        .long   L107            /* return address into callback */
#ifndef SYS_solaris
        .word   -1              /* negative frame size => use callback link */
        .word   0               /* no roots here */
#else
        .value  -1              /* negative frame size => use callback link */
        .value  0               /* no roots here */
#endif
