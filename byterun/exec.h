/*s: byterun/exec.h */
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

/* $Id: exec.h,v 1.4 1997/06/13 15:49:32 xleroy Exp $ */

/* exec.h : format of executable bytecode files */

/*  offset 0 --->  initial junk
                   code block
                   names of primitives
                   data block
                   symbol table
                   debug infos
                   trailer
 end of file --->
*/

/*s: constant TRAILER_SIZE */
/* Structure of the trailer.
   Sizes are 32-bit unsigned integers, big endian */

#define TRAILER_SIZE (5*4+12)
/*e: constant TRAILER_SIZE */

/*s: struct exec_trailer */
struct exec_trailer {
  unsigned int code_size;      /* Size of the code block (in bytes) */
  unsigned int prim_size;      /* Size of the primitive table (in bytes) */
  unsigned int data_size;      /* Size of the global data table (bytes) */
  unsigned int symbol_size;    /* Size of the symbol table (bytes) */
  unsigned int debug_size;     /* Size of the debug infos (bytes) */
  char magic[12];              /* A magic string */
};
/*e: struct exec_trailer */

/*s: constant EXEC_MAGIC */
/* Magic number for this release */

#define EXEC_MAGIC "Caml1999X002"
/*e: constant EXEC_MAGIC */

/*e: byterun/exec.h */
