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

/* $Id: mkdir.c,v 1.6 1997/09/02 12:54:41 xleroy Exp $ */

#include <sys/types.h>
#include <sys/stat.h>
#include <mlvalues.h>
#include "unixsupport.h"

value unix_mkdir(value path, value perm)     /* ML */
{
  if (mkdir(String_val(path), Int_val(perm)) == -1) uerror("mkdir", path);
  return Val_unit;
}
