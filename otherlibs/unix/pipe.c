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

/* $Id: pipe.c,v 1.5 1997/09/02 12:54:43 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

value unix_pipe(void)                /* ML */
{
  int fd[2];
  value res;
  if (pipe(fd) == -1) uerror("pipe", Nothing);
  res = alloc_small(2, 0);
  Field(res, 0) = Val_int(fd[0]);
  Field(res, 1) = Val_int(fd[1]);
  return res;
}
