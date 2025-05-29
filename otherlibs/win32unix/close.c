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

/* $Id: close.c,v 1.1 1997/09/03 14:37:58 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_close(value fd)             /* ML */
{
  if (! CloseHandle(Handle_val(fd))) {
    _dosmaperr(GetLastError());
    uerror("close", Nothing);
  }
  return Val_unit;
}
