/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id: mkdir.c,v 1.2 1996/09/05 13:32:21 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

value unix_mkdir(path, perm)     /* ML */
     value path, perm;
{
  if (_mkdir(String_val(path)) == -1) uerror("mkdir", path);
  return Val_unit;
}
