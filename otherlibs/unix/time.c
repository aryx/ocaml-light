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

/* $Id: time.c,v 1.5 1997/09/02 12:54:48 xleroy Exp $ */

#include <time.h>
#include <mlvalues.h>
#include "unixsupport.h"

value unix_time(void)                /* ML */
{
  return Val_long(time((time_t *) NULL));
}
