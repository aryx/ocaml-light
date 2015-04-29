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

/* $Id: execv.c,v 1.5 1997/09/02 12:54:33 xleroy Exp $ */

#include <mlvalues.h>
#include <memory.h>
#include "unixsupport.h"

extern char ** cstringvect(value arg);

value unix_execv(value path, value args)     /* ML */
{
  char ** argv;
  argv = cstringvect(args);
  (void) execv(String_val(path), argv);
  stat_free((char *) argv);
  uerror("execv", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

