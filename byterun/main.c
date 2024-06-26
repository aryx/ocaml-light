/*s: byterun/main.c */
/*s: copyright header C xavier and damien */
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
/*e: copyright header C xavier and damien */

/* Main entry point (can be overriden by a user-provided main()
   function that calls caml_main() later. */

#include "misc.h"
#include "mlvalues.h"
#include "sys.h"

extern void caml_main (char **);

/*s: function [[main]] */
int main(int argc, char **argv)
{
  caml_main(argv);
  sys_exit(Val_int(0));
  return 0; /* not reached */
}
/*e: function [[main]] */
/*e: byterun/main.c */
