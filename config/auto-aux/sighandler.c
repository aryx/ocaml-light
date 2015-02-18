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

/* $Id: sighandler.c,v 1.4 1997/09/02 12:54:19 xleroy Exp $ */

#include <signal.h>

int main(void)
{
  SIGRETURN (*old)();
  old = signal(SIGQUIT, SIG_DFL);
  return 0;
}
