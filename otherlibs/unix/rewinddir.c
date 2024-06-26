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

/* $Id: rewinddir.c,v 1.7 1997/09/02 12:54:44 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
#else
#include <sys/dir.h>
#endif

#ifdef HAS_REWINDDIR

value unix_rewinddir(value d)          /* ML */
{
  rewinddir((DIR *) d);
  return Val_unit;
}

#else

value unix_rewinddir(value d)
{ invalid_argument("rewinddir not implemented"); }

#endif
