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

/* $Id: setuid.c,v 1.5 1997/09/02 12:54:46 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_setuid(value uid)           /* ML */
{
  if (setuid(Int_val(uid)) == -1) uerror("setuid", Nothing);
  return Val_unit;
}
