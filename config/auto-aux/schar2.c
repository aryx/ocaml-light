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

/* $Id: schar2.c,v 1.4 1997/09/02 12:54:19 xleroy Exp $ */

signed char foo[]="\377";
main(void)
{
  int i;
  i = foo[0];
  exit(i != -1);
}
