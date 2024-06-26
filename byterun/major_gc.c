/*s: byterun/major_gc.c */
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
// LONG_MAX
#include <limits.h>
#else
#endif

#include "compact.h"
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "weak.h"

/*s: global [[percent_free]] */
unsigned long percent_free;
/*e: global [[percent_free]] */
/*s: global [[major_heap_increment]] */
long major_heap_increment;
/*e: global [[major_heap_increment]] */
char *heap_start, *heap_end;
/*s: global [[page_table]] */
page_table_entry *page_table;
/*e: global [[page_table]] */
/*s: global [[page_table_size]] */
asize_t page_table_size;
/*e: global [[page_table_size]] */
/*s: global [[gc_sweep_hp]] */
char *gc_sweep_hp;
/*e: global [[gc_sweep_hp]] */
/*s: global [[gc_phase]] */
int gc_phase;
/*e: global [[gc_phase]] */
/*s: global [[gray_vals]] */
static value *gray_vals;
/*e: global [[gray_vals]] */
value *gray_vals_cur, *gray_vals_end;
/*s: global [[gray_vals_size]] */
static asize_t gray_vals_size;
/*e: global [[gray_vals_size]] */
/*s: global [[heap_is_pure]] */
static int heap_is_pure;   /* The heap is pure if the only gray objects
                              below [markhp] are also in [gray_vals]. */
/*e: global [[heap_is_pure]] */
/*s: global [[allocated_words]] */
unsigned long allocated_words;
/*e: global [[allocated_words]] */
/*s: global [[extra_heap_memory]] */
unsigned long extra_heap_memory;
/*e: global [[extra_heap_memory]] */
extern char *fl_merge;  /* Defined in freelist.c. */

static char *markhp, *chunk, *limit;

static void update_weak_pointers (void);

/*s: function [[realloc_gray_vals]] */
static void realloc_gray_vals (void)
{
  value *new;

  Assert (gray_vals_cur == gray_vals_end);
  if (gray_vals_size < stat_heap_size / 128){
    gc_message ("Growing gray_vals to %luk bytes\n",
                (long) gray_vals_size * sizeof (value) / 512);
    new = (value *) realloc ((char *) gray_vals,
                             2 * gray_vals_size * sizeof (value));
    if (new == NULL){
      gc_message ("No room for growing gray_vals\n", 0);
      gray_vals_cur = gray_vals;
      heap_is_pure = 0;
    }else{
      gray_vals = new;
      gray_vals_cur = gray_vals + gray_vals_size;
      gray_vals_size *= 2;
      gray_vals_end = gray_vals + gray_vals_size;
    }
  }else{
    gray_vals_cur = gray_vals + gray_vals_size / 2;
    heap_is_pure = 0;
  }
}
/*e: function [[realloc_gray_vals]] */

/*s: function [[darken]] */
void darken (value v, value *p)
             
                /* not used */
{
  if (Is_block (v) && Is_in_heap (v)) {
    if (Tag_val(v) == Infix_tag) v -= Infix_offset_val(v);
    if (Is_white_val (v)){
      Hd_val (v) = Grayhd_hd (Hd_val (v));
      *gray_vals_cur++ = v;
      if (gray_vals_cur >= gray_vals_end) realloc_gray_vals ();
    }
  }
}
/*e: function [[darken]] */

/*s: function [[start_cycle]] */
static void start_cycle (void)
{
  Assert (gc_phase == Phase_idle);
  Assert (gray_vals_cur == gray_vals);
  darken_all_roots();
  gc_phase = Phase_mark;
  markhp = NULL;
}
/*e: function [[start_cycle]] */

/*s: function [[mark_slice]] */
static void mark_slice (long int work)
{
  value *gray_vals_ptr;  /* Local copy of gray_vals_cur */
  value v, child;
  header_t hd;
  mlsize_t size, i;

  gray_vals_ptr = gray_vals_cur;
  while (work > 0){
    if (gray_vals_ptr > gray_vals){
      v = *--gray_vals_ptr;
      hd = Hd_val(v);
      Assert (Is_gray_hd (hd));
      Hd_val (v) = Blackhd_hd (hd);
      size = Wosize_hd(hd);
      if (Tag_hd (hd) < No_scan_tag){
        for (i = 0; i < size; i++){
          child = Field (v, i);
          if (Is_block (child) && Is_in_heap (child)) {
            hd = Hd_val(child);
            if (Tag_hd(hd) == Infix_tag) {
              child -= Infix_offset_val(child);
              hd = Hd_val(child);
            }
            if (Is_white_hd (hd)){
              Hd_val (child) = Grayhd_hd (hd);
              *gray_vals_ptr++ = child;
              if (gray_vals_ptr >= gray_vals_end) {
                gray_vals_cur = gray_vals_ptr;
                realloc_gray_vals ();
                gray_vals_ptr = gray_vals_cur;
              }
            }
          }
        }
      }
      work -= Whsize_wosize(size);
    }else if (markhp != NULL){
      if (markhp == limit){
        chunk = Chunk_next (chunk);
        if (chunk == NULL){
          markhp = NULL;
        }else{
          markhp = chunk;
          limit = chunk + Chunk_size (chunk);
        }
      }else{
        if (Is_gray_val (Val_hp (markhp))){
          Assert (gray_vals_ptr == gray_vals);
          *gray_vals_ptr++ = Val_hp (markhp);
        }
        markhp += Bhsize_hp (markhp);
      }
    }else if (!heap_is_pure){
      heap_is_pure = 1;
      chunk = heap_start;
      markhp = chunk;
      limit = chunk + Chunk_size (chunk);
    }else{
      /* Marking is done. */

      update_weak_pointers ();
      
      /* Initialise the sweep phase. */
      gray_vals_cur = gray_vals_ptr;
      gc_sweep_hp = heap_start;
      fl_init_merge ();
      gc_phase = Phase_sweep;
      chunk = heap_start;
      gc_sweep_hp = chunk;
      limit = chunk + Chunk_size (chunk);
      work = 0;
    }
  }
  gray_vals_cur = gray_vals_ptr;
}
/*e: function [[mark_slice]] */

/*s: function [[update_weak_pointers]] */
/* Walk through the linked list of weak arrays.
   Arrays that are white are removed from this list.
   For the other arrays, pointers to white objects are erased.
*/
static void update_weak_pointers (void)
{
  value *prev = &weak_list_head;
  value *cur = (value *) *prev;
  mlsize_t sz, i;

  while (cur != NULL){
    if (Color_val (cur) == White){
      *prev = Field (cur, 0);
      cur = (value *) *prev;
    }else{
      value curfield;

      sz = Wosize_val (cur);
      for (i = 1; i < sz; i++){
        curfield = Field (cur, i);
        if (curfield != 0 && Is_block (curfield) && Is_white_val (curfield)){
          Field (cur, i) = 0;
        }
      }
      prev = &Field (cur, 0);
      cur = (value *) *prev;
    }
  }
}
/*e: function [[update_weak_pointers]] */

/*s: function [[sweep_slice]] */
static void sweep_slice (long int work)
{
  char *hp;
  header_t hd;

  while (work > 0){
    if (gc_sweep_hp < limit){
      hp = gc_sweep_hp;
      hd = Hd_hp (hp);
      work -= Whsize_hd (hd);
      gc_sweep_hp += Bhsize_hd (hd);
      switch (Color_hd (hd)){
      case White:
        if (Tag_hd (hd) == Final_tag){
          Final_fun (Val_hp (hp)) (Val_hp (hp));
        }
        gc_sweep_hp = fl_merge_block (Bp_hp (hp));
        break;
      case Blue:
        /* Only the blocks of the free-list are blue.  See [freelist.c]. */
        fl_merge = Bp_hp (hp);
        break;
      default:          /* Gray or Black */
        Assert(Color_hd(hd) == Black);
        Hd_hp (hp) = Whitehd_hd (hd);
        break;
      }
      Assert (gc_sweep_hp <= limit);
    }else{
      chunk = Chunk_next (chunk);
      if (chunk == NULL){
        /* Sweeping is done. */
        ++ stat_major_collections;
        work = 0;
        gc_phase = Phase_idle;
      }else{
        gc_sweep_hp = chunk;
        limit = chunk + Chunk_size (chunk);
      }
    }
  }
}
/*e: function [[sweep_slice]] */

/*s: function [[major_collection_slice]] */
/* The main entry point for the GC.  Called at each minor GC. */
void major_collection_slice (void)
{
  /* 
     Free memory at the start of the GC cycle (garbage + free list) (assumed):
                 FM = stat_heap_size * percent_free / (100 + percent_free)
     Garbage at the start of the GC cycle:
                 G = FM * 2/3
     Proportion of free memory consumed since the previous slice:
                 PH = allocated_words / G
     Proportion of extra-heap memory consumed since the previous slice:
                 PE = extra_heap_memory / stat_heap_size
     Proportion of total work to do in this slice:
                 P  = PH + PE
     Amount of marking work for the GC cycle:
                 MW = stat_heap_size * 100 / (100 + percent_free)
     Amount of sweeping work for the GC cycle:
                 SW = stat_heap_size
     Amount of marking work for this slice:
                 MS = MW * P
                 MS = 3/2 * 100 * allocated_words / percent_free
                      + extra_heap_memory * 100 / (100 + percent_free)
     Amount of sweeping work for this slice:
                 SS = SW * P
                 SS = 3/2 * (100 + percent_free)/percent_free * allocated_words
                      + extra_heap_memory
     This slice will either mark 2*MS words or sweep 2*SS words.
  */

#define Margin 100  /* Make it a little faster to be on the safe side. */

  if (gc_phase == Phase_idle) start_cycle ();

  if (gc_phase == Phase_mark){
    mark_slice (300 * (allocated_words / percent_free + 1)
                + 200 * (extra_heap_memory / (100 + percent_free) + 1)
        + Margin);
    gc_message ("!", 0);
  }else{
    Assert (gc_phase == Phase_sweep);
    sweep_slice (3 * (100 + percent_free) * (allocated_words / percent_free + 1)
                 + 2 * extra_heap_memory
                 + Margin);
    gc_message ("$", 0);
  }

  if (gc_phase == Phase_idle) compact_heap_maybe ();

  stat_major_words += allocated_words;
  allocated_words = 0;
  extra_heap_memory = 0;
}
/*e: function [[major_collection_slice]] */

/* The minor heap must be empty when this function is called. */
/*s: function [[finish_major_cycle]] */
/* This does not call compact_heap_maybe because the estimations of
   free and live memory are only valid for a cycle done incrementally.
   Besides, this function is called by compact_heap_maybe.
*/
void finish_major_cycle (void)
{
  if (gc_phase == Phase_idle) start_cycle ();
  if (gc_phase == Phase_mark) mark_slice (LONG_MAX);
  Assert (gc_phase == Phase_sweep);
  sweep_slice (LONG_MAX);
  Assert (gc_phase == Phase_idle);
  stat_major_words += allocated_words;
  allocated_words = 0;
}
/*e: function [[finish_major_cycle]] */

/*s: function [[round_heap_chunk_size]] */
asize_t round_heap_chunk_size (asize_t request)
{                            Assert (major_heap_increment >= Heap_chunk_min);
  if (request < major_heap_increment){
                              Assert (major_heap_increment % Page_size == 0);
    return major_heap_increment;
  }else if (request <= Heap_chunk_max){
    return ((request + Page_size - 1) >> Page_log) << Page_log;
  }else{
    raise_out_of_memory ();
    /* not reached */ return 0;
  }
}
/*e: function [[round_heap_chunk_size]] */

/*s: function [[init_major_heap]] */
void init_major_heap (asize_t heap_size)
{
  asize_t i;
  void *block;

  stat_heap_size = round_heap_chunk_size (heap_size);
  Assert (stat_heap_size % Page_size == 0);
  heap_start = aligned_malloc (stat_heap_size + sizeof (heap_chunk_head),
                               sizeof (heap_chunk_head), &block);
  if (heap_start == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  heap_start += sizeof (heap_chunk_head);
  Assert ((unsigned long) heap_start % Page_size == 0);
  Chunk_size (heap_start) = stat_heap_size;
  Chunk_next (heap_start) = NULL;
  Chunk_block (heap_start) = block;
  heap_end = heap_start + stat_heap_size;
  Assert ((unsigned long) heap_end % Page_size == 0);
  page_table_size = 4 * stat_heap_size / Page_size;
  page_table = 
    (page_table_entry *) malloc (page_table_size * sizeof(page_table_entry));
  if (page_table == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  for (i = 0; i < page_table_size; i++){
    page_table [i] = Not_in_heap;
  }
  for (i = Page (heap_start); i < Page (heap_end); i++){
    page_table [i] = In_heap;
  }
  Hd_hp (heap_start) = Make_header (Wosize_bhsize (stat_heap_size), 0, Blue);
  fl_init_merge ();
  fl_merge_block (Bp_hp (heap_start));
  gc_phase = Phase_idle;
  gray_vals_size = 2048;
  gray_vals = (value *) malloc (gray_vals_size * sizeof (value));
  if (gray_vals == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  gray_vals_cur = gray_vals;
  gray_vals_end = gray_vals + gray_vals_size;
  heap_is_pure = 1;
  allocated_words = 0;
  extra_heap_memory = 0;
}
/*e: function [[init_major_heap]] */
/*e: byterun/major_gc.c */
