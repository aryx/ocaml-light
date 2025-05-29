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

/* $Id: getsockname.c,v 1.2 1997/09/03 14:37:59 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"
#include "socketaddr.h"

value unix_getsockname(sock)          /* ML */
     value sock;
{
  int retcode;

  sock_addr_len = sizeof(sock_addr);
  retcode = getsockname((SOCKET) Handle_val(sock),
                        &sock_addr.s_gen, &sock_addr_len);
  if (retcode == -1) uerror("getsockname", Nothing);
  return alloc_sockaddr();
}
