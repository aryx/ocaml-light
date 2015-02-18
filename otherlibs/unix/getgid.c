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

/* $Id: getgid.c,v 1.5 1997/09/02 12:54:35 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_getgid(void)              /* ML */
{
  return Val_int(getgid());
}
