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

/* $Id: gethostname.c,v 1.5 1997/09/02 12:54:37 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>
#include <sys/param.h>
#include "unixsupport.h"

#ifdef HAS_GETHOSTNAME

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

value unix_gethostname(value unit)         /* ML */
{
  char name[MAXHOSTNAMELEN];
  gethostname(name, MAXHOSTNAMELEN);
  name[MAXHOSTNAMELEN-1] = 0;
  return copy_string(name);
}

#else
#ifdef HAS_UNAME

#include <sys/utsname.h>

value unix_gethostname(value unit)
{
  struct utsname un;
  uname(&un);
  return copy_string(un.nodename);
}

#else

value unix_gethostname(value unit)
{ invalid_argument("gethostname not implemented"); }

#endif
#endif
