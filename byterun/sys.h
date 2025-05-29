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

/* $Id: sys.h,v 1.6 1997/09/02 12:54:10 xleroy Exp $ */

#ifndef _sys_
#define _sys_

#include "misc.h"

#define NO_ARG Val_int(0)
void sys_error (value);
void sys_init (char **);
value sys_exit (value);
char * searchpath (char * name);

#endif /* _sys_ */
