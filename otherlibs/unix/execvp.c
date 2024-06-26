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

/* $Id: execvp.c,v 1.5 1997/09/02 12:54:34 xleroy Exp $ */

#include <mlvalues.h>
#include <memory.h>
#include "unixsupport.h"

extern char ** cstringvect(value arg);
extern char ** environ;

value unix_execvp(value path, value args)     /* ML */
{
  char ** argv;
  argv = cstringvect(args);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

value unix_execvpe(value path, value args, value env)     /* ML */
{
  char ** argv;
  char ** saved_environ;
  argv = cstringvect(args);
  saved_environ = environ;
  environ = cstringvect(env);
  (void) execvp(String_val(path), argv);
  stat_free((char *) argv);
  stat_free((char *) environ);
  environ = saved_environ;
  uerror("execvp", path);
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}

