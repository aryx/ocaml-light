/*s: byterun/reverse.h */
/*s: copyright header C xavier */
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
/*e: copyright header C xavier */

/* claude: upstream (7175ab04, 2000-02-10) generalizes these to 2-arg
 * dst/src macros and adds a 16-bit variant; old 1-arg bodies kept below
 * under #if 0 rather than deleted, since the syncweb markers wrapping
 * them must not be renamed/moved (see docs/literate) */

/* Swap byte-order in 16, 32, and 64-bit integers or floats */

#ifndef _reverse_
#define _reverse_

/*s: function [[Reverse_int32]] */
#if 0
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
#endif
#define Reverse_32(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a, _b;                                                              \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _b = _p[1];                                                               \
  _q[0] = _p[3];                                                            \
  _q[1] = _p[2];                                                            \
  _q[3] = _a;                                                               \
  _q[2] = _b;                                                               \
}
/*e: function [[Reverse_int32]] */

/*s: function [[Reverse_int64]] */
#if 0
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
#endif
#define Reverse_64(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a, _b;                                                              \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _b = _p[1];                                                               \
  _q[0] = _p[7];                                                            \
  _q[1] = _p[6];                                                            \
  _q[7] = _a;                                                               \
  _q[6] = _b;                                                               \
  _a = _p[2];                                                               \
  _b = _p[3];                                                               \
  _q[2] = _p[5];                                                            \
  _q[3] = _p[4];                                                            \
  _q[5] = _a;                                                               \
  _q[4] = _b;                                                               \
}
/*e: function [[Reverse_int64]] */

/*s: constant [[Reverse_double]] */
#if 0
#define Reverse_double Reverse_int64
#endif
#define Reverse_16(dst,src) {                                               \
  char * _p, * _q;                                                          \
  char _a;                                                                  \
  _p = (char *) (src);                                                      \
  _q = (char *) (dst);                                                      \
  _a = _p[0];                                                               \
  _q[0] = _p[1];                                                            \
  _q[1] = _a;                                                               \
}
/*e: constant [[Reverse_double]] */


#endif /* _reverse_ */
/*e: byterun/reverse.h */
