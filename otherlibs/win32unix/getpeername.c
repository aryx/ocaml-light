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

/* $Id: getpeername.c,v 1.3 1997/09/04 13:45:43 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"
#include "socketaddr.h"

value unix_getpeername(sock)          /* ML */
     value sock;
{
  int retcode;

  sock_addr_len = sizeof(sock_addr);
  retcode = getpeername((SOCKET) Handle_val(sock),
                        &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) unix_error(WSAGetLastError(), "getpeername", Nothing);
  return alloc_sockaddr();
}
