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

/* $Id: execve.c,v 1.5 1997/09/02 12:54:33 xleroy Exp $ */

#include <mlvalues.h>
#include <memory.h>
#include "unixsupport.h"

extern char ** cstringvect(value arg);

value unix_execve(value path, value args, value env)     /* ML */
{
  char ** argv;
  char ** envp;
  argv = cstringvect(args);
  envp = cstringvect(env);
  (void) execve(String_val(path), argv, envp);
  stat_free((char *) argv);
  stat_free((char *) envp);
  uerror("execve", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

