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

/* $Id: prims.h,v 1.3 1996/04/30 14:45:15 xleroy Exp $ */

/* Interface with C primitives. */

#ifndef _prims_
#define _prims_

typedef value (*c_primitive)();

extern c_primitive cprim[];
extern char * names_of_cprim[];

#endif /* _prims_ */
