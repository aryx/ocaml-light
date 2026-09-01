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

        .globl  call_gen_code
        .ent    call_gen_code
	
call_gen_code:
        lda     $sp, -80($sp)
        stq     $26, 0($sp)
        stq     $9, 8($sp)
        stq     $10, 16($sp)
        stq     $11, 24($sp)
        stq     $12, 32($sp)
        stt     $f2, 40($sp)
        stt     $f3, 48($sp)
        stt     $f4, 56($sp)
        stt     $f5, 64($sp)
        mov     $16, $27
        mov     $17, $16
        mov     $18, $17
        mov     $19, $18
        mov     $20, $19
        jsr     ($27)
        ldq     $26, 0($sp)
        ldq     $9, 8($sp)
        ldq     $10, 16($sp)
        ldq     $11, 24($sp)
        ldq     $12, 32($sp)
        ldt     $f2, 40($sp)
        ldt     $f3, 48($sp)
        ldt     $f4, 56($sp)
        ldt     $f5, 64($sp)
	lda     $sp, 80($sp)
        ret     ($26)

        .end    call_gen_code

        .globl  caml_c_call
        .ent    caml_c_call
caml_c_call:
        lda     $sp, -16($sp)
        stq     $26, 0($sp)
        stq     $gp, 8($sp)
	mov     $25, $27
        jsr     ($25)
        ldq     $26, 0($sp)
        ldq     $gp, 8($sp)
        lda     $sp, 16($sp)
        ret     ($26)

        .end    caml_c_call

/* claude: asmcomp/alpha/emit.mlp's Icheckbound failure path branches to
   this trampoline (see asmrun/alpha.S, which defines the same thing for
   the real runtime) rather than array_bound_error (defined in main.c)
   directly, because $gp needs reinitializing before the call -- ported
   verbatim from asmrun/alpha.S since this standalone test harness links
   against this file instead. */

        .globl  call_array_bound_error
        .ent    call_array_bound_error
        .align  3
call_array_bound_error:
        br      $27, $200
$200:   ldgp    $gp, 0($27)
        jsr     array_bound_error       /* never returns */
        .end    call_array_bound_error

