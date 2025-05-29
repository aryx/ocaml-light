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

/* $Id: close.c,v 1.7 1997/09/02 12:54:31 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_close(value fd)             /* ML */
{
  if (close(Int_val(fd)) == -1) uerror("close", Nothing);
  return Val_unit;
}
