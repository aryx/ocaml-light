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

/* Machine-dependent interface with the asm code */

#ifndef _stack_
#define _stack_

/* Macros to access the stack frame */

#ifdef TARGET_i386
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct caml_context *)(sp + 8))
#endif

#ifdef TARGET_arm
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct caml_context *)(sp + 8))
#endif

/* Structure of Caml callback contexts */

struct caml_context {
  char * bottom_of_stack;       /* beginning of Caml stack chunk */
  unsigned long last_retaddr;   /* last return address in Caml code */
  value * gc_regs;              /* pointer to register block */
};

/* Declaration of variables used in the asm code */
extern char * caml_bottom_of_stack;
extern unsigned long caml_last_return_address;
extern value * caml_gc_regs;
extern char * caml_exception_pointer;
extern value caml_globals[];
extern long * caml_frametable[];


#endif /* _stack_ */
