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

/* $Id: cst2constr.h,v 1.4 1996/04/30 14:48:02 xleroy Exp $ */

#ifdef __STDC__
value cst_to_constr(int, int *, int, int);
#else
value cst_to_constr();
#endif
