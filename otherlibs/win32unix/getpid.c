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

/* $Id: getpid.c,v 1.1 1996/09/04 14:17:29 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

extern value val_process_id;

value unix_getpid(value unit)              /* ML */
{
  return val_process_id;
}
