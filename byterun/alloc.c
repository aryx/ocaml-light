/*s: byterun/alloc.c */
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

/* 1. Allocation functions doing the same work as the macros in the
      case where [Setup_for_gc] and [Restore_after_gc] are no-ops.
   2. Convenience functions related to allocation.
*/

#include "config.h"
#ifndef OS_PLAN9
#include <string.h>
#else
#endif

#include "alloc.h"
#include "major_gc.h"
#include "memory.h"
#include "mlvalues.h"
#include "stacks.h"

/*s: constant [[Setup_for_gc]] */
#define Setup_for_gc
/*e: constant [[Setup_for_gc]] */
/*s: constant [[Restore_after_gc]] */
#define Restore_after_gc
/*e: constant [[Restore_after_gc]] */

/*s: function [[alloc]] */
value alloc (mlsize_t wosize, tag_t tag)
{
  value result;
  mlsize_t i;

  Assert (wosize > 0);
  if (wosize <= Max_young_wosize){
    Alloc_small (result, wosize, tag);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = 0;
    }
  }else{
    result = alloc_shr (wosize, tag);
    if (tag < No_scan_tag) memset (Bp_val (result), 0, Bsize_wsize (wosize));
    result = check_urgent_gc (result);
  }
  return result;
}

value alloc_small (mlsize_t wosize, tag_t tag)
{
  value result;
  
  Assert (wosize > 0 && wosize <= Max_young_wosize);
  Alloc_small (result, wosize, tag);
  return result;
}
/*e: function [[alloc]] */

/*s: function [[alloc_tuple]] */
value alloc_tuple(mlsize_t n)
{
  return alloc(n, 0);
}
/*e: function [[alloc_tuple]] */

/*s: function [[alloc_string]] */
value alloc_string (mlsize_t len)
{
  value result;
  mlsize_t offset_index;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);

  if (wosize <= Max_young_wosize) {
    Alloc_small (result, wosize, String_tag);
  }else{
    result = alloc_shr (wosize, String_tag);
    result = check_urgent_gc (result);
  }
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  return result;
}
/*e: function [[alloc_string]] */

/*s: function [[alloc_final]] */
value alloc_final (mlsize_t len, final_fun fun, mlsize_t mem, mlsize_t max)
{
  value result = alloc_shr (len, Final_tag);

  Field (result, 0) = (value) fun;
  adjust_gc_speed (mem, max);
  result = check_urgent_gc (result);
  return result;
}
/*e: function [[alloc_final]] */

/*s: function [[copy_string]] */
value copy_string(char *s)
{
  int len;
  value res;

  len = strlen(s);
  res = alloc_string(len);
  bcopy(s, String_val(res), len);
  return res;
}
/*e: function [[copy_string]] */

/*s: function [[alloc_array]] */
value alloc_array(value (*funct)(char *), char ** arr)
{
  mlsize_t nbr, n;
  value v, result;

  nbr = 0;
  while (arr[nbr] != 0) nbr++;
  if (nbr == 0) {
    return Atom(0);
  } else {
    result = alloc (nbr, 0);
    Begin_root(result);
      for (n = 0; n < nbr; n++) {
    /* The two statements below must be separate because of evaluation
           order. */
    v = funct(arr[n]);
    modify(&Field(result, n), v);
      }
    End_roots();
    return result;
  }
}
/*e: function [[alloc_array]] */

/*s: function [[copy_string_array]] */
value copy_string_array(char **arr)
{
  return alloc_array(copy_string, arr);
}
/*e: function [[copy_string_array]] */

/*s: function [[convert_flag_list]] */
int convert_flag_list(value list, int *flags)
{
  int res;
  res = 0;
  while (list != Val_int(0)) {
    res |= flags[Int_val(Field(list, 0))];
    list = Field(list, 1);
  }
  return res;
}
/*e: function [[convert_flag_list]] */
/*e: byterun/alloc.c */
