/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id: setsid.c,v 1.2 1997/09/02 12:54:45 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

value unix_setsid(value unit)              /* ML */
{
#ifdef HAS_SETSID
  return Val_int(setsid());
#else
  invalid_argument("setsid not implemented");
  return Val_unit;
#endif
}
