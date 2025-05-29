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

/* $Id: cstringv.c,v 1.5 1997/09/02 12:54:32 xleroy Exp $ */

#include <mlvalues.h>
#include <memory.h>
#include "unixsupport.h"

char ** cstringvect(value arg)
{
  char ** res;
  mlsize_t size, i;

  size = Wosize_val(arg);
  res = (char **) stat_alloc((size + 1) * sizeof(char *));
  for (i = 0; i < size; i++) res[i] = String_val(Field(arg, i));
  res[size] = NULL;
  return res;
}

  
