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

/* $Id: opendir.c,v 1.5 1997/09/02 12:54:42 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

value unix_opendir(value path)         /* ML */
{
  DIR * d;
  d = opendir(String_val(path));
  if (d == (DIR *) NULL) uerror("opendir", path);
  return (value) d;
}
