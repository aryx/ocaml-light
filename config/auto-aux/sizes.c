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

/* $Id: sizes.c,v 1.4 1997/09/02 12:54:20 xleroy Exp $ */

int main(int argc, char **argv)
{
  printf("%d %d %d\n", sizeof(int), sizeof(long), sizeof(long *));
  return 0;
}
