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

/* $Id: listen.c,v 1.4 1997/09/04 13:45:45 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <winsock.h>

value unix_listen(sock, backlog) /* ML */
     value sock, backlog;
{
  if (listen((SOCKET) Handle_val(sock), Int_val(backlog)) == -1)
    unix_error(WSAGetLastError(), "listen", Nothing);
  return Val_unit;
}
