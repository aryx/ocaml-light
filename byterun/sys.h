/*s: byterun/sys.h */
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

#ifndef _sys_
#define _sys_

#include "misc.h"

/*s: constant NO_ARG */
#define NO_ARG Val_int(0)
/*e: constant NO_ARG */
void sys_error (value);
void sys_init (char **);
value sys_exit (value);
char * searchpath (char * name);

#endif /* _sys_ */
/*e: byterun/sys.h */
