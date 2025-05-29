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

/* $Id: headernt.c,v 1.4 1997/09/02 12:54:54 xleroy Exp $ */

#include <process.h>

char * runtime_name = "ocamlrun.exe";
char * errmsg = "Cannot find ocamlrun.exe\n";

int main(int argc, char ** argv)
{
  int retcode;
  retcode = spawnvp(P_WAIT, runtime_name, argv);
  /* We use P_WAIT instead of P_OVERLAY here because under NT,
     P_OVERLAY returns to the command interpreter, displaying the prompt
     before executing the command. */
  if (retcode == -1) {
    write(2, errmsg, strlen(errmsg));
    return 2;
  }
  return retcode;
}
