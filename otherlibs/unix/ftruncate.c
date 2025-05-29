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

/* $Id: ftruncate.c,v 1.5 1997/09/02 12:54:35 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_TRUNCATE

value unix_ftruncate(value fd, value len)    /* ML */
{
  if (ftruncate(Int_val(fd), Long_val(len)) == -1)
    uerror("ftruncate", Nothing);
  return Val_unit;
}

#else

value unix_ftruncate(value fd, value len)
{ invalid_argument("ftruncate not implemented"); }

#endif
