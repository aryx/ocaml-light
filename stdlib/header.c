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

/* $Id: header.c,v 1.9 1997/09/02 12:54:54 xleroy Exp $ */

char runtime_name [] = RUNTIME_NAME;
char errmsg [] = "Cannot exec ocamlrun.\n";

int main(int argc, char ** argv)
{
  execv(runtime_name, argv);
  write(2, errmsg, sizeof(errmsg)-1);
  return 2;
}
