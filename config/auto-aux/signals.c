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

/* $Id: signals.c,v 1.4 1997/09/02 12:54:19 xleroy Exp $ */

/* To determine the semantics of signal handlers
   (System V: signal is reset to default behavior on entrance to the handler
    BSD: signal handler remains active). */

#include <stdio.h>
#include <signal.h>

/* Find a signal that is ignored by default */

#ifdef SIGCHLD
#define IGNSIG SIGCHLD
#else
#ifdef SIGIO
#define IGNSIG SIGIO
#else
#ifdef SIGCLD
#define IGNSIG SIGCLD
#else
#ifdef SIGPWR
#define IGNSIG SIGPWR
#endif
#endif
#endif
#endif

#ifdef IGNSIG

int counter;

void sig_handler(int dummy)
{
  counter++;
}

int main(int argc, char **argv)
{
  signal(IGNSIG, sig_handler);
  counter = 0;
  kill(getpid(), IGNSIG);
  kill(getpid(), IGNSIG);
  return (counter == 2 ? 0 : 1);
}

#else

/* If no suitable signal was found, assume System V */

int main(int argc, char ** argv)
{
  return 1;
}

#endif
