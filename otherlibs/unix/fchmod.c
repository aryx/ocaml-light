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

/* $Id: fchmod.c,v 1.6 1997/09/02 12:54:34 xleroy Exp $ */

#include <sys/types.h>
#include <sys/stat.h>
#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_FCHMOD

value unix_fchmod(value fd, value perm)      /* ML */
{
  if (fchmod(Int_val(fd), Int_val(perm)) == -1) uerror("fchmod", Nothing);
  return Val_unit;
}

#else

value unix_fchmod(value fd, value perm)
{ invalid_argument("fchmod not implemented"); }
  
#endif
