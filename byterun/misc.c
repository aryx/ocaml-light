/*s: byterun/misc.c */
/*s: copyright header C xavier and damien */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C xavier and damien */

#include <stdio.h>
#include "config.h"
#include "misc.h"
#ifdef HAS_UI
#include "ui.h"
#endif

#ifdef DEBUG

/*s: function failed_assert */
void failed_assert (char * expr, char * file, int line)
{
  fprintf (stderr, "Assertion failed: %s; file %s; line %d\n",
           expr, file, line);
  exit (100);
}
/*e: function failed_assert */

/*s: global seed */
static unsigned long seed = 0x12345;
/*e: global seed */

/*s: function not_random */
unsigned long not_random (void)
{
  seed = seed * 65537 + 12345;
  return seed;
}
/*e: function not_random */

#endif

/*s: global verb_gc */
int verb_gc;
/*e: global verb_gc */

/*s: function gc_message */
void gc_message (char *msg, long unsigned int arg)
{
  if (verb_gc){
#ifdef HAS_UI
    ui_print_stderr(msg, (void *) arg);
#else
    fprintf (stderr, msg, arg);
    fflush (stderr);
#endif
  }
}
/*e: function gc_message */

/*s: function fatal_error */
void fatal_error (char *msg)
{
#ifdef HAS_UI
  ui_print_stderr("%s", msg);
  ui_exit (2);
#else
  fprintf (stderr, "%s", msg);
  exit(2);
#endif
}
/*e: function fatal_error */

/*s: function fatal_error_arg */
void fatal_error_arg (char *fmt, char *arg)
{
#ifdef HAS_UI
  ui_print_stderr(fmt, arg);
  ui_exit (2);
#else
  fprintf (stderr, fmt, arg);
  exit(2);
#endif
}
/*e: function fatal_error_arg */

#ifdef USING_MEMMOV

/*s: function memmov */
/* This should work on 64-bit machines as well as 32-bit machines.
   It assumes a long is the natural size for memory reads and writes.
*/
void memmov (char * dst, char * src, unsigned long length)
{
  unsigned long i;

  if ((unsigned long) dst <= (unsigned long) src){

      /* Copy in ascending order. */
    if (((unsigned long) src - (unsigned long) dst) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
           Copy byte by byte. */
      for (; length != 0; length--){
        *dst++ = *src++;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i != 0){
        i = sizeof (long) - i;              /* Number of bytes to copy. */
        if (i > length) i = length;         /* Never copy more than length.*/
        for (; i != 0; i--){
          *dst++ = *src++; --length;
        }
      }                    Assert ((unsigned long) dst % sizeof (long) == 0);
                           Assert ((unsigned long) src % sizeof (long) == 0);

      /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
        *(long *) dst = *(long *) src;
        dst += sizeof (long); src += sizeof (long);
      }

      /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
        *dst++ = *src++;
      }
    }
  }else{                                       /* Copy in descending order. */
    src += length; dst += length;
    if (((unsigned long) dst - (unsigned long) src) % sizeof (long) != 0){

        /* The pointers are not equal modulo sizeof (long).
           Copy byte by byte. */
      for (; length > 0; length--){
        *--dst = *--src;
      }
    }else{

        /* Copy the first few bytes. */
      i = (unsigned long) dst % sizeof (long);
      if (i > length) i = length;           /* Never copy more than length. */
      for (; i > 0; i--){
        *--dst = *--src; --length;
      }

        /* Then copy as many entire words as possible. */
      for (i = length / sizeof (long); i > 0; i--){
        dst -= sizeof (long); src -= sizeof (long);
        *(long *) dst = *(long *) src;
      }

        /* Then copy the last few bytes. */
      for (i = length % sizeof (long); i > 0; i--){
        *--dst = *--src;
      }
    }
  }
}
/*e: function memmov */

#endif /* USING_MEMMOV */

/*s: function aligned_malloc */
char *aligned_malloc (asize_t size, int modulo, void **block)
                  
                
                        /* output */
{
  char *raw_mem;
  unsigned long aligned_mem;
                                                 Assert (modulo < Page_size);
  raw_mem = (char *) malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  *block = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
  return (char *) (aligned_mem - modulo);
}
/*e: function aligned_malloc */
/*e: byterun/misc.c */
