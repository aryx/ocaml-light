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

/* $Id: socketpair.c,v 1.7 1997/09/02 12:54:47 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/socket.h>

extern int socket_domain_table[], socket_type_table[];

value unix_socketpair(value domain, value type, value proto) /* ML */
{
  int sv[2];
  value res;
  if (socketpair(socket_domain_table[Int_val(domain)],
                 socket_type_table[Int_val(type)],
                 Int_val(proto), sv) == -1)
    uerror("socketpair", Nothing);
  res = alloc_small(2, 0);
  Field(res,0) = Val_int(sv[0]);
  Field(res,1) = Val_int(sv[1]);
  return res;
}

#else

value unix_socketpair(value domain, value type, value proto)
{ invalid_argument("socketpair not implemented"); }

#endif
