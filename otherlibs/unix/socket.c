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

/* $Id: socket.c,v 1.6 1997/09/02 12:54:46 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/types.h>
#include <sys/socket.h>

int socket_domain_table[] = {
  PF_UNIX, PF_INET
};

int socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

value unix_socket(value domain, value type, value proto) /* ML */
{
  int retcode;
  retcode = socket(socket_domain_table[Int_val(domain)],
                   socket_type_table[Int_val(type)],
                   Int_val(proto));
  if (retcode == -1) uerror("socket", Nothing);
  return Val_int(retcode);

}

#else

value unix_socket(value domain, value type, value proto)
{ invalid_argument("socket not implemented"); }

#endif
