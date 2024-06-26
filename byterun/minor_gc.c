/*s: byterun/minor_gc.c */
/*s: copyright header C damien */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C damien */

#include "config.h"

#ifndef OS_PLAN9
#include <string.h>
#endif

#include "fail.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"

/*s: global [[minor_heap_size]] */
asize_t minor_heap_size;
/*e: global [[minor_heap_size]] */
char *young_start = NULL;
char *young_end = NULL;

char *young_ptr = NULL;
char *young_limit = NULL;

static value **ref_table = NULL;
static value **ref_table_end;
static value **ref_table_threshold;

value **ref_table_ptr = NULL;
value **ref_table_limit;

static asize_t ref_table_size;
static asize_t ref_table_reserve;

/*s: global [[in_minor_collection]] */
int in_minor_collection = 0;
/*e: global [[in_minor_collection]] */

/*s: function [[set_minor_heap_size]] */
void set_minor_heap_size (asize_t size)
{
  char *new_heap;
  value **new_table;

  Assert (size >= Minor_heap_min);
  Assert (size <= Minor_heap_max);
  Assert (size % sizeof (value) == 0);
  if (young_ptr != young_end) minor_collection ();
                                           Assert (young_ptr == young_end);
  new_heap = (char *) stat_alloc (size);
  if (young_start != NULL){
    stat_free (young_start);
  }
  young_start = new_heap;
  young_end = new_heap + size;
  young_limit = young_start;
  young_ptr = young_end;
  minor_heap_size = size;

  ref_table_size = minor_heap_size / sizeof (value) / 8;
  ref_table_reserve = 256;
  new_table = (value **) stat_alloc ((ref_table_size + ref_table_reserve)
                                     * sizeof (value *));
  if (ref_table != NULL) stat_free (ref_table);
  ref_table = new_table;
  ref_table_ptr = ref_table;
  ref_table_threshold = ref_table + ref_table_size;
  ref_table_limit = ref_table_threshold;
  ref_table_end = ref_table + ref_table_size + ref_table_reserve;
}
/*e: function [[set_minor_heap_size]] */

/*s: function [[oldify]] */
void oldify (value v, value *p)
{
  value result, field0;
  header_t hd;
  mlsize_t sz, i;
  int tag;

 tail_call:
  if (Is_block (v) && Is_young (v)){
    Assert (Hp_val (v) >= young_ptr);
    hd = Hd_val (v);
    tag = Tag_hd (hd);
    if (Is_blue_hd (hd)){    /* Already forwarded ? */
      *p = Field (v, 0);     /* Then the forward pointer is the first field. */
    }else if (tag == Infix_tag) {
      mlsize_t offset = Infix_offset_hd (hd);
      oldify(v - offset, p);
      *p += offset;
    }else if (tag >= No_scan_tag){
      sz = Wosize_hd (hd);
      result = alloc_shr (sz, tag);
      for (i = 0; i < sz; i++) Field(result, i) = Field(v, i);
      Hd_val (v) = Bluehd_hd (hd);            /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      *p = result;
    }else{
      /* We can do recursive calls before all the fields are filled, because
         we will not be calling the major GC. */
      sz = Wosize_hd (hd);
      result = alloc_shr (sz, tag);
      *p = result;
      field0 = Field (v, 0);
      Hd_val (v) = Bluehd_hd (hd);            /* Put the forward flag. */
      Field (v, 0) = result;                  /* And the forward pointer. */
      if (sz == 1) {
        p = &Field (result, 0);
        v = field0;
        goto tail_call;
      } else {
        oldify (field0, &Field (result, 0));
        for (i = 1; i < sz - 1; i++){
          oldify (Field(v, i), &Field (result, i));
        }
        p = &Field (result, i);
        v = Field (v, i);
        goto tail_call;
      }
    }
  }else{
    *p = v;
  }
}
/*e: function [[oldify]] */

/*s: function [[minor_collection]] */
void minor_collection (void)
{
  value **r;
  long prev_alloc_words = allocated_words;

  in_minor_collection = 1;
  gc_message ("<", 0);
  oldify_local_roots();
  for (r = ref_table; r < ref_table_ptr; r++) oldify (**r, *r);
  stat_minor_words += Wsize_bsize (young_end - young_ptr);
  young_ptr = young_end;
  ref_table_ptr = ref_table;
  ref_table_limit = ref_table_threshold;
  gc_message (">", 0);
  in_minor_collection = 0;

  stat_promoted_words += allocated_words - prev_alloc_words;
  ++ stat_minor_collections;
  major_collection_slice ();
  force_major_slice = 0;
}
/*e: function [[minor_collection]] */

/*s: function [[check_urgent_gc]] */
value check_urgent_gc (value extra_root)
{
  if (force_major_slice) {
    Begin_root(extra_root);
      minor_collection();
    End_roots();
  }
  return extra_root;
}
/*e: function [[check_urgent_gc]] */

/*s: function [[realloc_ref_table]] */
void realloc_ref_table (void)
{                                 Assert (ref_table_ptr == ref_table_limit);
                                  Assert (ref_table_limit <= ref_table_end);
                            Assert (ref_table_limit >= ref_table_threshold);

  if (ref_table_limit == ref_table_threshold){
    gc_message ("ref_table threshold crossed\n", 0);
    ref_table_limit = ref_table_end;
    urge_major_slice ();
  }else{ /* This will almost never happen with the bytecode interpreter. */
    asize_t sz;
    asize_t cur_ptr = ref_table_ptr - ref_table;
                                                  Assert (force_major_slice);

    ref_table_size *= 2;
    sz = (ref_table_size + ref_table_reserve) * sizeof (value *);
    gc_message ("Growing ref_table to %ldk bytes\n", (long) sz / 1024);
    ref_table = (value **) realloc ((char *) ref_table, sz);
    if (ref_table == NULL) fatal_error ("Fatal error: ref_table overflow\n");
    ref_table_end = ref_table + ref_table_size + ref_table_reserve;
    ref_table_threshold = ref_table + ref_table_size;
    ref_table_ptr = ref_table + cur_ptr;
    ref_table_limit = ref_table_end;
  }
}
/*e: function [[realloc_ref_table]] */
/*e: byterun/minor_gc.c */
