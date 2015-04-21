/*s: byterun/instruct.h */
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

/*s: enum instructions */
/* The instruction set. */

enum instructions {
  /*s: basic stack operations opcodes */
    ACC0, ACC1, ACC2, ACC3, ACC4, ACC5, ACC6, ACC7,
    ACC, PUSH,
    PUSHACC0, PUSHACC1, PUSHACC2, PUSHACC3,
    PUSHACC4, PUSHACC5, PUSHACC6, PUSHACC7,
    PUSHACC, 
    POP, ASSIGN,
  /*e: basic stack operations opcodes */
  /*s: env access opcodes */
    ENVACC1, ENVACC2, ENVACC3, ENVACC4, ENVACC,
    PUSHENVACC1, PUSHENVACC2, PUSHENVACC3, PUSHENVACC4, PUSHENVACC,
  /*e: env access opcodes */
  /*s: function application opcodes */
    PUSH_RETADDR, APPLY, APPLY1, APPLY2, APPLY3,
    APPTERM, APPTERM1, APPTERM2, APPTERM3, 
    RETURN, 
  /*e: function application opcodes */
  /*s: misc opcodes */
    RESTART, GRAB,
    CLOSURE, CLOSUREREC,
  /*e: misc opcodes */
  /*s: global data access opcodes */
    GETGLOBAL, PUSHGETGLOBAL, GETGLOBALFIELD, PUSHGETGLOBALFIELD, SETGLOBAL,
  /*e: global data access opcodes */
  /*s: blocks allocation opcodes */
    ATOM0, ATOM, PUSHATOM0, PUSHATOM,
    MAKEBLOCK, MAKEBLOCK1, MAKEBLOCK2, MAKEBLOCK3,
  /*e: blocks allocation opcodes */
  /*s: blocks access opcodes */
    GETFIELD0, GETFIELD1, GETFIELD2, GETFIELD3, GETFIELD,
    SETFIELD0, SETFIELD1, SETFIELD2, SETFIELD3, SETFIELD,
  /*e: blocks access opcodes */
  /*s: recursive definition opcodes */
    DUMMY, UPDATE,
  /*e: recursive definition opcodes */
  /*s: array opcodes */
    VECTLENGTH, GETVECTITEM, SETVECTITEM,
  /*e: array opcodes */
  /*s: string opcodes */
    GETSTRINGCHAR, SETSTRINGCHAR, 
  /*e: string opcodes */
  /*s: branching opcodes */
    BRANCH, BRANCHIF, BRANCHIFNOT, SWITCH, BOOLNOT,
  /*e: branching opcodes */
  /*s: exception opcodes */
    PUSHTRAP, POPTRAP, RAISE,
  /*e: exception opcodes */
  /*s: signal opcodes */
    CHECK_SIGNALS,
  /*e: signal opcodes */
  /*s: foreign C calls opcodes */
    C_CALL1, C_CALL2, C_CALL3, C_CALL4, C_CALL5, C_CALLN,
  /*e: foreign C calls opcodes */
  /*s: arithmetics opcodes */
    CONST0, CONST1, CONST2, CONST3, CONSTINT,
    PUSHCONST0, PUSHCONST1, PUSHCONST2, PUSHCONST3, PUSHCONSTINT,
    NEGINT, ADDINT, SUBINT, MULINT, DIVINT, MODINT,
    ANDINT, ORINT, XORINT, LSLINT, LSRINT, ASRINT,
    EQ, NEQ, LTINT, LEINT, GTINT, GEINT,
    OFFSETINT, OFFSETREF,
  /*e: arithmetics opcodes */
  /*s: oo opcodes */
    GETMETHOD,
  /*e: oo opcodes */
  /*s: debugger opcodes */
    STOP, EVENT, BREAK
  /*e: debugger opcodes */
};
/*e: enum instructions */
/*e: byterun/instruct.h */
