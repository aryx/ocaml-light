/*s: byterun/instrtrace.h */
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

/* $Id: instrtrace.h,v 1.4 1997/09/02 12:54:00 xleroy Exp $ */

/* Trace the instructions executed */

#ifndef _instrtrace_
/*s: constant _instrtrace_ */
#define _instrtrace_
/*e: constant _instrtrace_ */


#include "mlvalues.h"
#include "misc.h"

extern int trace_flag;
extern long icount;
void stop_here (void);
void disasm_instr (code_t pc);


#endif
/*e: byterun/instrtrace.h */
