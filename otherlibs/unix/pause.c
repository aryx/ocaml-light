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

/* $Id: pause.c,v 1.5 1997/09/02 12:54:42 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_pause(void)               /* ML */
{
  pause();
  return Val_unit;
}
