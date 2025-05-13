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

/* $Id: errmsg.c,v 1.6 1997/09/02 12:54:33 xleroy Exp $ */

#include <errno.h>
#include <mlvalues.h>
#include <alloc.h>

extern int error_table[];

extern char * strerror(int);

value unix_error_message(value err)
{
  int errnum;
  errnum = error_table[Int_val(err)];
  return copy_string(strerror(errnum));
}
