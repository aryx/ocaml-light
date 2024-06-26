/*s: byterun/fix_code.c */
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

/* Handling of blocks of bytecode (endianness switch, threading). */

#include "config.h"

#ifndef OS_PLAN9
#include <unistd.h>
#endif

#include "debugger.h"
#include "fix_code.h"
#include "instruct.h"
#include "md5.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "reverse.h"

/*s: global [[start_code]] */
code_t start_code;
/*e: global [[start_code]] */
/*s: global [[code_size]] */
asize_t code_size;
/*e: global [[code_size]] */
/*s: global [[saved_code]] */
unsigned char * saved_code;
/*e: global [[saved_code]] */
/*s: global [[code_md5]] */
unsigned char code_md5[16];
/*e: global [[code_md5]] */

/*s: function [[load_code]] */
/* Read the main bytecode block from a file */

void load_code(int fd, asize_t len)
{
  int i;
  struct MD5Context ctx;

  code_size = len;
  start_code = (code_t) stat_alloc(code_size);
  if (read(fd, (char *) start_code, code_size) != code_size)
    fatal_error("Fatal error: truncated bytecode file.\n");
  MD5Init(&ctx);
  MD5Update(&ctx, (unsigned char *) start_code, code_size);
  MD5Final(code_md5, &ctx);
#ifdef ARCH_BIG_ENDIAN
  fixup_endianness(start_code, code_size);
#endif
  if (debugger_in_use) {
    len /= sizeof(opcode_t);
    saved_code = (unsigned char *) stat_alloc(len);
    for (i = 0; i < len; i++) saved_code[i] = start_code[i];
  }
#ifdef THREADED_CODE
  /* Better to thread now than at the beginning of interprete(),
     since the debugger interface needs to perform SET_EVENT requests
     on the code. */
  thread_code(start_code, code_size);
#endif
}
/*e: function [[load_code]] */

/* This code is needed only if the processor is big endian */

#ifdef ARCH_BIG_ENDIAN

/*s: function [[fixup_endianness]] */
void fixup_endianness(code_t code, asize_t len)
{
  code_t p;
  len /= sizeof(opcode_t);
  for (p = code; p < code + len; p++) {
    Reverse_int32(p);
  }
}
/*e: function [[fixup_endianness]] */

#endif

/* This code is needed only if we're using threaded code */

#ifdef THREADED_CODE

/*s: global [[instr_table]] */
char ** instr_table;
/*e: global [[instr_table]] */
/*s: global [[instr_base]] */
char * instr_base;
/*e: global [[instr_base]] */

/*s: function [[thread_code]] */
void thread_code (code_t code, asize_t len)
{
  code_t p;
  int l [STOP + 1];
  int i;
  
  for (i = 0; i <= STOP; i++) {
    l [i] = 0;
  }
  /* Instructions with one operand */
  l[PUSHACC] = l[ACC] = l[POP] = l[ASSIGN] =
  l[PUSHENVACC] = l[ENVACC] = l[PUSH_RETADDR] = l[APPLY] =
  l[APPTERM1] = l[APPTERM2] = l[APPTERM3] = l[RETURN] =
  l[GRAB] = l[PUSHGETGLOBAL] = l[GETGLOBAL] = l[SETGLOBAL] =
  l[PUSHATOM] = l[ATOM] = l[MAKEBLOCK1] = l[MAKEBLOCK2] =
  l[MAKEBLOCK3] = l[GETFIELD] = l[SETFIELD] = l[DUMMY] =
  l[BRANCH] = l[BRANCHIF] = l[BRANCHIFNOT] = l[PUSHTRAP] =
  l[C_CALL1] = l[C_CALL2] = l[C_CALL3] = l[C_CALL4] = l[C_CALL5] =
  l[CONSTINT] = l[PUSHCONSTINT] = l[OFFSETINT] = l[OFFSETREF] = 1;

  /* Instructions with two operands */
  l[APPTERM] = l[CLOSURE] = l[CLOSUREREC] = l[PUSHGETGLOBALFIELD] =
  l[GETGLOBALFIELD] = l[MAKEBLOCK] = l[C_CALLN] = 2;

  len /= sizeof(opcode_t);
  for (p = code; p < code + len; /*nothing*/) {
    opcode_t instr = *p;
    if (instr < 0 || instr > STOP){
      fatal_error_arg ("Fatal error in fix_code: bad opcode (%lx)\n",
                       (char *)(long)instr);
    }
    *p++ = (opcode_t)(instr_table[instr] - instr_base);
    if (instr == SWITCH) {
      uint32 sizes = *p++;
      uint32 const_size = sizes & 0xFFFF;
      uint32 block_size = sizes >> 16;
      p += const_size + block_size;
    } else {
      p += l[instr];
    }
  }
  Assert(p == code + len);
}
/*e: function [[thread_code]] */

#endif /* THREADED_CODE */

/*s: function [[set_instruction]] */
void set_instruction(code_t pos, opcode_t instr)
{
#ifdef THREADED_CODE
  *pos = (opcode_t)(instr_table[instr] - instr_base);
#else
  *pos = instr;
#endif
}
/*e: function [[set_instruction]] */

int is_instruction(opcode_t instr1, opcode_t instr2)
{
#ifdef THREADED_CODE
  return instr1 == (opcode_t)(instr_table[instr2] - instr_base);
#else
  return instr1 == instr2;
#endif
}

/*e: byterun/fix_code.c */
