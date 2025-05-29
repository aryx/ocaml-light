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

/* $Id: exit.c,v 1.5 1997/09/02 12:54:34 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_exit(value n)               /* ML */
{
  _exit(Int_val(n));
  return Val_unit;                  /* never reached, but suppress warnings */
                                /* from smart compilers */
}


