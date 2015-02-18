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

/* $Id: link.c,v 1.5 1997/09/02 12:54:40 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_link(value path1, value path2)    /* ML */
{
  if (link(String_val(path1), String_val(path2)) == -1) uerror("link", path2);
  return Val_unit;
}
