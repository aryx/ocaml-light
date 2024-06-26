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

/* $Id: readdir.c,v 1.6 1997/09/02 12:54:44 xleroy Exp $ */

#include <mlvalues.h>
#include <fail.h>
#include <alloc.h>
#include "unixsupport.h"
#include <sys/types.h>
#ifdef HAS_DIRENT
#include <dirent.h>
typedef struct dirent directory_entry;
#else
#include <sys/dir.h>
typedef struct direct directory_entry;
#endif

value unix_readdir(value d)            /* ML */
{
  directory_entry * e;

  e = readdir((DIR *) d);
  if (e == (directory_entry *) NULL) raise_end_of_file();
  return copy_string(e->d_name);
}
