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

/* $Id: getserv.c,v 1.7 1997/09/02 12:54:38 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <sys/types.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

static value alloc_service_entry(struct servent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit, proto = Val_unit;

  Begin_roots3 (name, aliases, proto);
    name = copy_string(entry->s_name);
    aliases = copy_string_array(entry->s_aliases);
    proto = copy_string(entry->s_proto);
    res = alloc_small(4, 0);
    Field(res,0) = name;
    Field(res,1) = aliases;
    Field(res,2) = Val_int(ntohs(entry->s_port));
    Field(res,3) = proto;
  End_roots();
  return res;
}

value unix_getservbyname(value name, value proto)  /* ML */
{
  struct servent * entry;
  entry = getservbyname(String_val(name), String_val(proto));
  if (entry == (struct servent *) NULL) raise_not_found();
  return alloc_service_entry(entry);
}

value unix_getservbyport(value port, value proto)  /* ML */
{
  struct servent * entry;
  entry = getservbyport(Int_val(port), String_val(proto));
  if (entry == (struct servent *) NULL) raise_not_found();
  return alloc_service_entry(entry);
}

#else

value unix_getservbyport(value port, value proto)
{ invalid_argument("getservbyport not implemented"); }
  
value unix_getservbyname(value name, value proto)
{ invalid_argument("getservbyname not implemented"); }

#endif
