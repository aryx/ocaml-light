/*s: byterun/compact.h */
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

/* $Id: compact.h,v 1.2 1997/09/02 12:53:56 xleroy Exp $ */

#ifndef _compact_
/*s: constant _compact_ */
#define _compact_
/*e: constant _compact_ */


#include "config.h"
#include "misc.h"

void compact_heap (void);
void compact_heap_maybe (void);


#endif /* _compact_ */
/*e: byterun/compact.h */
