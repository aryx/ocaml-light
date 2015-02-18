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

#ifdef HAS_STRERROR

extern char * strerror(int);

value unix_error_message(value err)
{
  int errnum;
  errnum = error_table[Int_val(err)];
  return copy_string(strerror(errnum));
}

#else

extern int sys_nerr;
extern char *sys_errlist[];

value unix_error_message(value err)
{
  int errnum;
  errnum = error_table[Int_val(err)];
  if (errnum < 0 || errnum >= sys_nerr) {
    return copy_string("Unknown error");
  } else {
    return copy_string(sys_errlist[errnum]);
  }
}

#endif
