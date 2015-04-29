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

/* $Id: readlink.c,v 1.5 1997/09/02 12:54:44 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>

#ifdef HAS_SYMLINK

#include <sys/stat.h> // for plan9
#include <sys/param.h>
#include "unixsupport.h"

#ifndef MAXPATHLEN // for plan9
#include <limits.h>
#define MAXPATHLEN _POSIX_PATH_MAX 
#endif


value unix_readlink(value path)        /* ML */
{
  char buffer[MAXPATHLEN];
  int len;
  len = readlink(String_val(path), buffer, sizeof(buffer) - 1);
  if (len == -1) uerror("readlink", path);
  buffer[len] = '\0';
  return copy_string(buffer);
}

#else

value unix_readlink(value path)
{ invalid_argument("readlink not implemented"); }

#endif
