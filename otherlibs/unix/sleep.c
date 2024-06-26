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

/* $Id: sleep.c,v 1.6 1997/09/02 12:54:46 xleroy Exp $ */

#include <mlvalues.h>
#include <signals.h>
#include "unixsupport.h"

value unix_sleep(value t)              /* ML */
{
  enter_blocking_section();
  sleep(Int_val(t));
  leave_blocking_section();
  return Val_unit;
}
