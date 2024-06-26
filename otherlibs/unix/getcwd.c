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

/* $Id: getcwd.c,v 1.8 1997/09/02 12:54:35 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

#ifdef HAS_GETCWD

#include <sys/param.h>

#ifndef MAXPATHLEN // for plan9
#include <limits.h>
#define MAXPATHLEN _POSIX_PATH_MAX 
#endif

value unix_getcwd(value unit)     /* ML */
{
  char buff[MAXPATHLEN];
  if (getcwd(buff, sizeof(buff)) == 0) uerror("getcwd", Nothing);
  return copy_string(buff);
}

#else
#ifdef HAS_GETWD

#include <sys/param.h>

value unix_getcwd(value unit)
{
  char buff[MAXPATHLEN];
  if (getwd(buff) == 0) uerror("getcwd", copy_string(buff));
  return copy_string(buff);
}

#else

value unix_getcwd(value unit)
{ invalid_argument("getcwd not implemented"); }

#endif
#endif
