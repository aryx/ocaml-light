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

/* $Id: getpid.c,v 1.5 1997/09/02 12:54:37 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_getpid(void)              /* ML */
{
  return Val_int(getpid());
}
