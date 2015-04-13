/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id: main.c,v 1.23 1997/11/17 15:11:56 doligez Exp $ */

/* Main entry point (can be overriden by a user-provided main()
   function that calls caml_main() later. */

#include "misc.h"
#include "mlvalues.h"
#include "sys.h"

extern void caml_main (char **);

int main(int argc, char **argv)
{
  caml_main(argv);
  sys_exit(Val_int(0));
  return 0; /* not reached */
}
