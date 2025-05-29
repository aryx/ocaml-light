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

/* $Id: rmdir.c,v 1.6 1997/09/02 12:54:45 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_rmdir(value path)           /* ML */
{
  if (rmdir(String_val(path)) == -1) uerror("rmdir", path);
  return Val_unit;
}
