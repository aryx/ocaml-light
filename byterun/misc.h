/*s: byterun/misc.h */
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

/* $Id: misc.h,v 1.7 1997/09/02 12:54:06 xleroy Exp $ */

/* Miscellaneous macros and variables. */

#ifndef _misc_
/*s: constant _misc_ */
#define _misc_
/*e: constant _misc_ */


#include "config.h"

/* Standard definitions */

#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#endif

/* Basic types and constants */

#ifdef __STDC__
typedef size_t asize_t;
#else
typedef int asize_t;
#endif

#ifndef NULL
/*s: constant NULL */
#define NULL 0
/*e: constant NULL */
#endif

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
#ifdef __STDC__
#define Assert(x) if (!(x)) failed_assert ( #x , __FILE__, __LINE__)
#else
#ifndef __LINE__
/*s: constant __LINE__ */
#define __LINE__ 0
/*e: constant __LINE__ */
#endif
#ifndef __FILE__
/*s: constant __FILE__ */
#define __FILE__ "(?)"
/*e: constant __FILE__ */
#endif
#define Assert(x) if (!(x)) failed_assert ("(?)" , __FILE__, __LINE__)
#endif
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
