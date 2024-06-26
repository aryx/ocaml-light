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

/* $Id: setgid.c,v 1.5 1997/09/02 12:54:45 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_setgid(value gid)           /* ML */
{
  if (setgid(Int_val(gid)) == -1) uerror("setgid", Nothing);
  return Val_unit;
}
