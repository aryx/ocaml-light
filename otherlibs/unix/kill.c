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

/* $Id: kill.c,v 1.6 1997/09/02 12:54:40 xleroy Exp $ */

#include <mlvalues.h>
#include <fail.h>
#include "unixsupport.h"
#include <signal.h>

extern int posix_signals[];     /* defined in byterun/signals.c */

value unix_kill(value pid, value signal)     /* ML */
{
  int sig;
  sig = Int_val(signal);
  if (sig < 0) {
    sig = posix_signals[-sig-1];
    if (sig < 0) invalid_argument("Sys.signal: unavailable signal");
  }
  if (kill(Int_val(pid), sig) == -1)
    uerror("kill", Nothing);
  return Val_unit;
}
