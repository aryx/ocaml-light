/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <memory.h>
#include <mlvalues.h>
#include <str.h>
#include "unixsupport.h"

//#include <stdlib.h>
//#include <string.h>

// include <strings.h>
void bcopy(const void *src, void *dest, size_t n);


value unix_putenv(value name, value val) /* ML */
{
  mlsize_t namelen = string_length(name);
  mlsize_t vallen = string_length(val);
  char * s = (char *) stat_alloc(namelen + 1 + vallen + 1);

  bcopy(String_val(name), s, namelen);
  s[namelen] = '=';
  bcopy(String_val(val), s + namelen + 1, vallen);
  s[namelen + 1 + vallen] = 0;
  if (putenv(s) == -1) uerror("putenv", name);
  return Val_unit;
}
