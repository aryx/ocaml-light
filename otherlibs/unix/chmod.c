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

/* $Id: chmod.c,v 1.6 1997/09/02 12:54:30 xleroy Exp $ */

#include <sys/types.h>
#include <sys/stat.h>
#include <mlvalues.h>
#include "unixsupport.h"

value unix_chmod(value path, value perm)     /* ML */
{
  int ret;
  ret = chmod(String_val(path), Int_val(perm));
  if (ret == -1) uerror("chmod", path);
  return Val_unit;
}
