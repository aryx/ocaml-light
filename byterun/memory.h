/*s: byterun/memory.h */
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

/* Allocation macros and functions */

#ifndef _memory_
#define _memory_


#include "config.h"
#include "gc.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"

value alloc_shr (mlsize_t, tag_t);
void adjust_gc_speed (mlsize_t, mlsize_t);
void modify (value *, value);
void initialize (value *, value);
value check_urgent_gc (value);
void * stat_alloc (asize_t);              /* Size in bytes. */
void stat_free (void *);
void * stat_resize (void *, asize_t);     /* Size in bytes. */

/* void shrink_heap (char *);        Only used in compact.c */

#ifdef NATIVE_CODE
/*s: constant Garbage_collection_function */
#define Garbage_collection_function garbage_collection
/*e: constant Garbage_collection_function */
#else
/*s: constant Garbage_collection_function (byterun/memory.h) */
#define Garbage_collection_function minor_collection
/*e: constant Garbage_collection_function (byterun/memory.h) */
#endif

/*s: function Alloc_small */
#define Alloc_small(result, wosize, tag) {            Assert (wosize >= 1); \
  young_ptr -= Bhsize_wosize (wosize);                                      \
  if (young_ptr < young_limit){                                             \
    Setup_for_gc;                                                           \
    Garbage_collection_function ();                                         \
    Restore_after_gc;                                                       \
    young_ptr -= Bhsize_wosize (wosize);                                    \
  }                                                                         \
  Hd_hp (young_ptr) = Make_header ((wosize), (tag), Black);                 \
  (result) = Val_hp (young_ptr);                                            \
}
/*e: function Alloc_small */

/* You must use [Modify] to change a field of an existing shared block,
   unless you are sure the value being overwritten is not a shared block and
   the value being written is not a young block. */
/*s: function Modify */
/* [Modify] never calls the GC. */

#define Modify(fp, val) {                                                   \
  value _old_ = *(fp);                                                      \
  *(fp) = (val);                                                            \
  if (Is_in_heap (fp)){                                                     \
    if (gc_phase == Phase_mark) darken (_old_, NULL);                       \
    if (Is_block (val) && Is_young (val)                                    \
        && ! (Is_block (_old_) && Is_young (_old_))){                       \
      *ref_table_ptr++ = (fp);                                              \
      if (ref_table_ptr >= ref_table_limit){                                \
        Assert (ref_table_ptr == ref_table_limit);                          \
        realloc_ref_table ();                                               \
      }                                                                     \
    }                                                                       \
  }                                                                         \
}                                                                           \
/*e: function Modify */

/*s: struct caml__roots_block */
/*
   [Begin_roots] and [End_roots] are used for C variables that are GC roots.
   It must contain all values in C local variables and function parameters
   at the time the minor GC is called.
   Usage:
   After initialising your local variables to legal Caml values, but before
   calling allocation functions, insert [Begin_roots_n(v1, ... vn)], where
   v1 ... vn are your variables of type [value] that you want to be updated
   across allocations.
   At the end, insert [End_roots()].

   Note that [Begin_roots] opens a new block, and [End_roots] closes it.
   Thus they must occur in matching pairs at the same brace nesting level.

   You can use [Val_unit] as a dummy initial value for your variables.
*/


struct caml__roots_block {
  struct caml__roots_block *next;
  long ntables;
  long nitems;
  value *tables [5];
};
/*e: struct caml__roots_block */

extern struct caml__roots_block *local_roots;  /* defined in roots.h */

/*s: constant Begin_root */
#define Begin_root Begin_roots1
/*e: constant Begin_root */

/*s: macro Begin_roots1 */
#define Begin_roots1(r0) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = &(r0);
/*e: macro Begin_roots1 */

#define Begin_roots2(r0, r1) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 2; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1);

#define Begin_roots3(r0, r1, r2) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 3; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2);

#define Begin_roots4(r0, r1, r2, r3) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 4; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3);

#define Begin_roots5(r0, r1, r2, r3, r4) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = 1; \
  caml__roots_block.ntables = 5; \
  caml__roots_block.tables[0] = &(r0); \
  caml__roots_block.tables[1] = &(r1); \
  caml__roots_block.tables[2] = &(r2); \
  caml__roots_block.tables[3] = &(r3); \
  caml__roots_block.tables[4] = &(r4);

#define Begin_roots_block(table, size) { \
  struct caml__roots_block caml__roots_block; \
  caml__roots_block.next = local_roots; \
  local_roots = &caml__roots_block; \
  caml__roots_block.nitems = (size); \
  caml__roots_block.ntables = 1; \
  caml__roots_block.tables[0] = (table);

/*s: macro End_roots */
#define End_roots() local_roots = caml__roots_block.next; }
/*e: macro End_roots */




/* [register_global_root] registers a global C variable as a memory root
   for the duration of the program, or until [remove_global_root] is
   called. */

void register_global_root (value *);

/* [remove_global_root] removes a memory root registered on a global C
   variable with [register_global_root]. */

void remove_global_root (value *);


#endif /* _memory_ */

/*e: byterun/memory.h */
