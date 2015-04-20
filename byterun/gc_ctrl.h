/*s: byterun/gc_ctrl.h */
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

/* $Id: gc_ctrl.h,v 1.6 1997/09/02 12:53:59 xleroy Exp $ */

#ifndef _gc_ctrl_
/*s: constant _gc_ctrl_ */
#define _gc_ctrl_
/*e: constant _gc_ctrl_ */

#include "misc.h"

extern long
     stat_minor_words,
     stat_promoted_words,
     stat_major_words,
     stat_minor_collections,
     stat_major_collections,
     stat_heap_size,
     stat_compactions;

void init_gc (unsigned long, unsigned long, unsigned long,
              unsigned long, unsigned long, unsigned long);


#endif /* _gc_ctrl_ */
/*e: byterun/gc_ctrl.h */
