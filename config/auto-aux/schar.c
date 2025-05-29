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

/* $Id: schar.c,v 1.4 1997/09/02 12:54:18 xleroy Exp $ */

char foo[]="\377";
main(void)
{
  int i;
  i = foo[0];
  exit(i != -1);
}
