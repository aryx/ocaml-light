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

/* $Id: getgr.c,v 1.7 1997/09/02 12:54:36 xleroy Exp $ */

#include <mlvalues.h>
#include <fail.h>
#include <alloc.h>
#include <memory.h>
#include "unixsupport.h"
#include <stdio.h>
#include <grp.h>

static value alloc_group_entry(struct group *entry)
{
  value res;
  value name = Val_unit, pass = Val_unit, mem = Val_unit;

  Begin_roots3 (name, pass, mem);
    name = copy_string(entry->gr_name);
    pass = copy_string(entry->gr_passwd);
    mem = copy_string_array(entry->gr_mem);
    res = alloc_small(4, 0);
    Field(res,0) = name;
    Field(res,1) = pass;
    Field(res,2) = Val_int(entry->gr_gid);
    Field(res,3) = mem;
  End_roots();
  return res;
}

value unix_getgrnam(value name)        /* ML */
{
  struct group * entry;
  entry = getgrnam(String_val(name));
  if (entry == NULL) raise_not_found();
  return alloc_group_entry(entry);
}

value unix_getgrgid(value gid)         /* ML */
{
  struct group * entry;
  entry = getgrgid(Int_val(gid));
  if (entry == NULL) raise_not_found();
  return alloc_group_entry(entry);
}
