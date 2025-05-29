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

/* $Id: chroot.c,v 1.5 1997/09/02 12:54:31 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_chroot(value path)           /* ML */
{
  int ret;
  ret = chroot(String_val(path));
  if (ret == -1) uerror("chroot", path);
  return Val_unit;
}
