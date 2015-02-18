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

/* $Id: unlink.c,v 1.5 1997/09/02 12:54:49 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_unlink(value path)          /* ML */
{
  if (unlink(String_val(path)) == -1) uerror("unlink", path);
  return Val_unit;
}
