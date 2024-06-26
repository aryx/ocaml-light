/*s: byterun/instrtrace.c */
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

/* Trace the instructions executed */

#ifdef DEBUG

#include <stdio.h>

#include "instruct.h"
#include "misc.h"
#include "mlvalues.h"
#include "opnames.h"

extern code_t start_code;
extern char * names_of_cprim[];

/*s: global [[icount]] */
long icount = 0;
/*e: global [[icount]] */

/*s: function [[stop_here]] */
void stop_here () {}
/*e: function [[stop_here]] */

/*s: global [[trace_flag]] */
int trace_flag = 0;
/*e: global [[trace_flag]] */

/*s: function [[disasm_instr]] */
void disasm_instr(pc)
     code_t pc;
{
  int instr = *pc;
  printf("%6ld  %s", (long) (pc - start_code),
         instr < 0 || instr > STOP ? "???" : names_of_instructions[instr]);
  pc++;
  switch(instr) {
      /* Instructions with one integer operand */
    case PUSHACC: case ACC: case POP: case ASSIGN:
    case PUSHENVACC: case ENVACC: case PUSH_RETADDR: case APPLY:
    case APPTERM1: case APPTERM2: case APPTERM3: case RETURN:
    case GRAB: case PUSHGETGLOBAL: case GETGLOBAL: case SETGLOBAL:
    case PUSHATOM: case ATOM: case MAKEBLOCK1: case MAKEBLOCK2:
    case MAKEBLOCK3: case GETFIELD: case SETFIELD: case DUMMY:
    case BRANCH: case BRANCHIF: case BRANCHIFNOT: case PUSHTRAP:
    case CONSTINT: case PUSHCONSTINT: case OFFSETINT: case OFFSETREF:
      printf(" %d\n", pc[0]); break;
      /* Instructions with two operands */
    case APPTERM: case CLOSURE: case CLOSUREREC: case PUSHGETGLOBALFIELD:
    case GETGLOBALFIELD: case MAKEBLOCK:
      printf(" %d, %d\n", pc[0], pc[1]); break;
      /* Instructions with a C primitive as operand */
    case C_CALL1: case C_CALL2: case C_CALL3: case C_CALL4: case C_CALL5:
      printf(" %s\n", names_of_cprim[pc[0]]); break;
    case C_CALLN:
      printf(" %d, %s\n", pc[0], names_of_cprim[pc[1]]); break;
    default:
      printf("\n");
    }
}
/*e: function [[disasm_instr]] */

#endif
/*e: byterun/instrtrace.c */
