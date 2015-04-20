/*s: byterun/signals.h */
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

/* $Id: signals.h,v 1.9 1997/09/02 12:54:08 xleroy Exp $ */

#ifndef _signals_
/*s: constant _signals_ */
#define _signals_
/*e: constant _signals_ */

#include "misc.h"
#include "mlvalues.h"

extern value signal_handlers;
extern volatile int pending_signal;
extern volatile int something_to_do;
extern volatile int force_major_slice;
extern volatile int async_signal_mode;

void enter_blocking_section (void);
void leave_blocking_section (void);
void urge_major_slice (void);

extern void (*enter_blocking_section_hook)();
extern void (*leave_blocking_section_hook)();

#endif /* _signals_ */

/*e: byterun/signals.h */
