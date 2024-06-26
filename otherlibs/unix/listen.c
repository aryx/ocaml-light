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

/* $Id: listen.c,v 1.6 1997/09/02 12:54:40 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/socket.h>

value unix_listen(value sock, value backlog)
{
  if (listen(Int_val(sock), Int_val(backlog)) == -1) uerror("listen", Nothing);
  return Val_unit;
}

#else

value unix_listen(value sock, value backlog)
{ invalid_argument("listen not implemented"); }

#endif
