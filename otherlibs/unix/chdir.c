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

/* $Id: chdir.c,v 1.5 1997/09/02 12:54:30 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_chdir(value path)           /* ML */
{
  int ret;
  ret = chdir(String_val(path));
  if (ret == -1) uerror("chdir", path);
  return Val_unit;
}
