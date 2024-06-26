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

/* $Id: dblalign.c,v 1.5 1997/09/02 12:54:17 xleroy Exp $ */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

double foo;

void access_double(double *p)
{
  foo = *p;
}

jmp_buf failure;

void sig_handler(int sig)
{
  longjmp(failure, 1);
}

int main(void)
{
  long n[10];
  int res;
  signal(SIGSEGV, sig_handler);
  signal(SIGBUS, sig_handler);
  if(setjmp(failure) == 0) {
    access_double((double *) n);
    access_double((double *) (n+1));
    res = 0;
  } else {
    res = 1;
  }
  signal(SIGSEGV, SIG_DFL);
  signal(SIGBUS, SIG_DFL);
  exit(res);
}

