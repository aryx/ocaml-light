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

/* $Id: reverse.h,v 1.5 1997/07/02 18:15:55 xleroy Exp $ */

/* Swap byte-order in 32-bit integers and in words */

#ifndef _reverse_
#define _reverse_

#define Reverse_int32(w) {                                                    \
  char * _p;                                                                  \
  int _a;                                                                     \
  _p = (char *) (w);                                                          \
  _a = _p[0];                                                                 \
  _p[0] = _p[3];                                                              \
  _p[3] = _a;                                                                 \
  _a = _p[1];                                                                 \
  _p[1] = _p[2];                                                              \
  _p[2] = _a;                                                                 \
}

#define Reverse_int64(d) {                                                    \
  char * _p;                                                                  \
  int _a;                                                                     \
  _p = (char *) (d);                                                          \
  _a = _p[0];                                                                 \
  _p[0] = _p[7];                                                              \
  _p[7] = _a;                                                                 \
  _a = _p[1];                                                                 \
  _p[1] = _p[6];                                                              \
  _p[6] = _a;                                                                 \
  _a = _p[2];                                                                 \
  _p[2] = _p[5];                                                              \
  _p[5] = _a;                                                                 \
  _a = _p[3];                                                                 \
  _p[3] = _p[4];                                                              \
  _p[4] = _a;                                                                 \
}

#define Reverse_double Reverse_int64


#endif /* _reverse_ */
