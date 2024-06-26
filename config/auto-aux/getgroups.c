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

/* $Id: getgroups.c,v 1.4 1997/09/02 12:54:18 xleroy Exp $ */

#include <sys/types.h>
#include <sys/param.h>

#ifdef NGROUPS

int main(void)
{
  int gidset[NGROUPS];
  if (getgroups(NGROUPS, gidset) == -1) return 1;
  return 0;
}

#else

int main(void) { return 1; }

#endif
