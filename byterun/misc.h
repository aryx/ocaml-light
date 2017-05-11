/*s: byterun/misc.h */
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

/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_


#include "config.h"

/* Standard definitions */

#ifndef OS_PLAN9
#include <stddef.h>
#include <stdlib.h>
#else
#endif

/* Basic types and constants */

typedef size_t asize_t;

typedef char * addr;

#ifdef __GNUC__
/*s: constant Noreturn */
/* Works only in GCC 2.5 and later */
#define Noreturn __attribute ((noreturn))
/*e: constant Noreturn */
#else
/*s: constant Noreturn (byterun/misc.h) */
#define Noreturn
/*e: constant Noreturn (byterun/misc.h) */
#endif

/* Assertions */

#ifdef DEBUG
#define Assert(x) if (!(x)) failed_assert ( #x , __FILE__, __LINE__)
#else
/*s: function Assert */
#define Assert(x)
/*e: function Assert */
#endif

void failed_assert (char *, char *, int) Noreturn;
void fatal_error (char *) Noreturn;
void fatal_error_arg (char *, char *) Noreturn;

/* GC flags and messages */

extern int verb_gc;
void gc_message (char *, unsigned long);

/* Memory routines */

void memmov (char *, char *, unsigned long);
char *aligned_malloc (asize_t, int, void **);

#ifdef DEBUG
unsigned long not_random (void);
#endif


#endif /* _misc_ */
/*e: byterun/misc.h */
