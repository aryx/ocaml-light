/*s: byterun/stacks.c */
/*s: copyright header C xavier */
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
/*e: copyright header C xavier */

/* To initialize and resize the stacks */

#include "config.h"

#ifndef OS_PLAN9
#include <string.h>
#endif

#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

/*s: global [[stack_low]] */
value * stack_low;
/*e: global [[stack_low]] */
/*s: global [[stack_high]] */
value * stack_high;
/*e: global [[stack_high]] */
/*s: global [[stack_threshold]] */
value * stack_threshold;
/*e: global [[stack_threshold]] */
/*s: global [[extern_sp]] */
value * extern_sp;
/*e: global [[extern_sp]] */
/*s: global [[trapsp]] */
value * trapsp;
/*e: global [[trapsp]] */
/*s: global [[trap_barrier]] */
value * trap_barrier;
/*e: global [[trap_barrier]] */
/*s: global [[global_data]] */
value global_data;
/*e: global [[global_data]] */

/*s: global [[max_stack_size]] */
unsigned long max_stack_size;            /* also used in gc_ctrl.c */
/*e: global [[max_stack_size]] */

/*s: function [[init_stack]] */
void init_stack (long unsigned int initial_max_size)
{
  stack_low = (value *) stat_alloc(Stack_size);
  stack_high = stack_low + Stack_size / sizeof (value);
  stack_threshold = stack_low + Stack_threshold / sizeof (value);
  extern_sp = stack_high;
  trapsp = stack_high;
  trap_barrier = stack_high + 1;
  max_stack_size = initial_max_size;
  gc_message ("Initial stack limit: %luk bytes\n",
          max_stack_size / 1024 * sizeof (value));
}
/*e: function [[init_stack]] */

/*s: function [[realloc_stack]] */
void realloc_stack(void)
{        
  asize_t size;
  value * new_low, * new_high, * new_sp;
  value * p;

  Assert(extern_sp >= stack_low);
  size = stack_high - stack_low;
  if (size >= max_stack_size) raise_stack_overflow();
  size *= 2;
  gc_message ("Growing stack to %luk bytes\n",
              (unsigned long) size * sizeof(value) / 1024);
  new_low = (value *) stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) stack_high - (char *) (ptr)))

  new_sp = (value *) shift(extern_sp);
  bcopy((char *) extern_sp,
        (char *) new_sp,
        (stack_high - extern_sp) * sizeof(value));
  stat_free(stack_low);
  trapsp = (value *) shift(trapsp);
  trap_barrier = (value *) shift(trap_barrier);
  for (p = trapsp; p < new_high; p = Trap_link(p))
    Trap_link(p) = (value *) shift(Trap_link(p));
  stack_low = new_low;
  stack_high = new_high;
  stack_threshold = stack_low + Stack_threshold / sizeof (value);
  extern_sp = new_sp;

#undef shift
}
/*e: function [[realloc_stack]] */

/*s: function [[change_max_stack_size]] */
void change_max_stack_size (long unsigned int new_max_size)
{
  asize_t size = stack_high - extern_sp + Stack_threshold / sizeof (value);

  if (new_max_size < size) new_max_size = size;
  if (new_max_size != max_stack_size){
    gc_message ("Changing stack limit to %luk bytes\n",
                new_max_size * sizeof (value) / 1024);
  }
  max_stack_size = new_max_size;
}
/*e: function [[change_max_stack_size]] */
/*e: byterun/stacks.c */
