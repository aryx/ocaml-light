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

/* $Id: nice.c,v 1.6 1997/09/02 12:54:42 xleroy Exp $ */

#include <mlvalues.h>
#include "unixsupport.h"
#include <errno.h>

#ifdef HAS_GETPRIORITY

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

value unix_nice(value incr)
{
  int prio;
  errno = 0;
  prio = getpriority(PRIO_PROCESS, 0);
  if (prio == -1 && errno != 0)
    uerror("nice", Nothing);
  prio += Int_val(incr);
  if (setpriority(PRIO_PROCESS, 0, prio) == -1)
    uerror("nice", Nothing);
  return Val_int(prio);
}

#else

value unix_nice(value incr)
{
  int ret;
  errno = 0;
  ret = nice(Int_val(incr));
  if (ret == -1 && errno != 0) uerror("nice", Nothing);
  return Val_int(ret);
}

#endif
