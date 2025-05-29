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

/* $Id: socketaddr.h,v 1.2 1997/09/02 16:01:39 xleroy Exp $ */

#include <misc.h>
#include <sys/types.h>
#include <winsock.h>

union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_in s_inet;
};

extern union sock_addr_union sock_addr;
extern int sock_addr_len;

void get_sockaddr (value);
value alloc_sockaddr (void);
value alloc_inet_addr (unsigned int);

#define GET_INET_ADDR(v) (*((uint32 *) (v)))
