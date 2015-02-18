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

/* $Id: envir.c,v 1.4 1997/09/02 12:54:33 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>

extern char ** environ;

value unix_environment(void)
{
  return copy_string_array(environ);
}
