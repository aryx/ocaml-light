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

#ifdef TARGET_mips
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct caml_context *)(sp + 8))
#endif

#ifdef TARGET_m68k
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Callback_link(sp) ((struct caml_context *)(sp + 8))
#endif

#ifdef TARGET_sparc
#define Saved_return_address(sp) *((long *)(sp + 92))
#define Callback_link(sp) ((struct caml_context *)(sp + 104))
#endif

#ifdef TARGET_alpha
#define Saved_return_address(sp) *((long *)(sp - 8))
#define Already_scanned(sp, retaddr) (retaddr & 1L)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 8)) = retaddr | 1L)
#define Mask_already_scanned(retaddr) (retaddr & ~1L)
#define Callback_link(sp) ((struct caml_context *)(sp + 16))
#endif

/* claude: same "call pushes retaddr, frame_size includes it, 2 extra
   words of exception-link data sit between the caml_context struct and
   the retaddr" trampoline shape as i386/arm/mips/m68k -- just with
   8-byte words instead of 4, exactly like alpha above, so the same
   Saved_return_address(sp) = *(sp - wordsize) / Callback_link(sp) =
   sp + 2*wordsize relationship applies. See asmrun/amd64.S's .L106
   (bottom_of_stack/last_retaddr/gc_regs pushed, then the 2-word
   exception handler link, then the `call` that pushes the retaddr). */
#ifdef TARGET_amd64
#define Saved_return_address(sp) *((long *)(sp - 8))
#define Callback_link(sp) ((struct caml_context *)(sp + 16))
#endif

/* claude: unlike every arch above, AArch64's "bl"/"blr" puts the return
   address in a register (x30/LR), not on the stack -- but every
   asmcomp/arm64/emit.mlp function that makes calls reserves a slot for
   x30 at the *top* of its own frame (frame_size includes it) and saves
   it there in its prologue (see fundecl's "str x30, [sp, #(n-8)]"), so
   by the time roots.c's walk does "sp += d->frame_size" to reach the
   caller's original sp, that saved x30 sits at exactly sp-8, same
   relative position as every wordsize-based target above. Likewise,
   asmrun/arm64.S's .Ljump_to_caml sets TRAP_PTR to the exact sp at
   which "blr ARG" is executed, with the caml_context struct exactly 16
   bytes above it (the callback link, then the 2-word exception-handler
   link pushed after it) -- so the same 2*wordsize Callback_link
   relationship holds too. See asmrun/arm64.S's .Ljump_to_caml. */
#ifdef TARGET_arm64
#define Saved_return_address(sp) *((long *)(sp - 8))
#define Callback_link(sp) ((struct caml_context *)(sp + 16))
#endif

/* claude: upstream ocaml also has an "#ifdef SYS_aix / Trap_frame_size
   24 / #else / 8 / #endif" here -- dead code for ocaml-light's
   -target-arch power (elf/Linux only, whose asmcomp/power/emit.mlp
   trap_frame_size is always 8, since toc is always false there), so
   hardcoded; see the comment on Arch.toc. */
#ifdef TARGET_power
#define Saved_return_address(sp) *((long *)(sp - 4))
#define Already_scanned(sp, retaddr) (retaddr & 1)
#define Mark_scanned(sp, retaddr) (*((long *)(sp - 4)) = retaddr | 1)
#define Mask_already_scanned(retaddr) (retaddr & ~1)
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
