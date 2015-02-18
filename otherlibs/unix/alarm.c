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

/* $Id: alarm.c,v 1.4 1996/09/04 14:14:19 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_alarm(t)              /* ML */
     value t;
{
  return Val_int(alarm((unsigned int) Long_val(t)));
}
