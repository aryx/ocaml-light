/*s: byterun/parsing.c */
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

/* The PDA automaton for parsers generated by camlyacc */

#include <stdio.h>
#include "config.h"
#include "mlvalues.h"
#include "memory.h"
#include "alloc.h"

/*s: constant ERRCODE */
#define ERRCODE 256
/*e: constant ERRCODE */

/*s: struct parser_tables */
struct parser_tables {    /* Mirrors parse_tables in ../stdlib/parsing.mli */
  value actions;
  value transl_const;
  value transl_block;
  char * lhs;
  char * len;
  char * defred;
  char * dgoto;
  char * sindex;
  char * rindex;
  char * gindex;
  value tablesize;
  char * table;
  char * check;
  value error_function;
};
/*e: struct parser_tables */

/*s: struct parser_env */
struct parser_env {       /* Mirrors parser_env in ../stdlib/parsing.ml */
  value s_stack;
  value v_stack;
  value symb_start_stack;
  value symb_end_stack;
  value stacksize;
  value stackbase;
  value curr_char;
  value lval;
  value symb_start;
  value symb_end;
  value asp;
  value rule_len;
  value rule_number;
  value sp;
  value state;
  value errflag;
};
/*e: struct parser_env */

#ifdef ARCH_BIG_ENDIAN
/*s: function Short (byterun/parsing.c) */
#define Short(tbl,n) \
  (*((unsigned char *)((tbl) + (n) * sizeof(short))) + \
          (*((schar *)((tbl) + (n) * sizeof(short) + 1)) << 8))
/*e: function Short (byterun/parsing.c) */
#else
/*s: function Short (byterun/parsing.c)2 */
#define Short(tbl,n) (((short *)(tbl))[n])
/*e: function Short (byterun/parsing.c)2 */
#endif

#ifdef DEBUG
/*s: global parser_trace */
int parser_trace = 0;
/*e: global parser_trace */
/*s: function Trace */
#define Trace(act) if(parser_trace) act
/*e: function Trace */
#else
/*s: function Trace (byterun/parsing.c) */
#define Trace(act)
/*e: function Trace (byterun/parsing.c) */
#endif

/* Input codes */
/*s: constant START */
/* Mirrors parser_input in ../stdlib/parsing.ml */
#define START 0
/*e: constant START */
/*s: constant TOKEN_READ */
#define TOKEN_READ 1
/*e: constant TOKEN_READ */
/*s: constant STACKS_GROWN_1 */
#define STACKS_GROWN_1 2
/*e: constant STACKS_GROWN_1 */
/*s: constant STACKS_GROWN_2 */
#define STACKS_GROWN_2 3
/*e: constant STACKS_GROWN_2 */
/*s: constant SEMANTIC_ACTION_COMPUTED */
#define SEMANTIC_ACTION_COMPUTED 4
/*e: constant SEMANTIC_ACTION_COMPUTED */
/*s: constant ERROR_DETECTED */
#define ERROR_DETECTED 5
/*e: constant ERROR_DETECTED */

/* Output codes */
/*s: constant READ_TOKEN */
/* Mirrors parser_output in ../stdlib/parsing.ml */
#define READ_TOKEN Val_int(0) 
/*e: constant READ_TOKEN */
/*s: constant RAISE_PARSE_ERROR */
#define RAISE_PARSE_ERROR Val_int(1)
/*e: constant RAISE_PARSE_ERROR */
/*s: constant GROW_STACKS_1 */
#define GROW_STACKS_1 Val_int(2)
/*e: constant GROW_STACKS_1 */
/*s: constant GROW_STACKS_2 */
#define GROW_STACKS_2 Val_int(3)
/*e: constant GROW_STACKS_2 */
/*s: constant COMPUTE_SEMANTIC_ACTION */
#define COMPUTE_SEMANTIC_ACTION Val_int(4)
/*e: constant COMPUTE_SEMANTIC_ACTION */
/*s: constant CALL_ERROR_FUNCTION */
#define CALL_ERROR_FUNCTION Val_int(5)
/*e: constant CALL_ERROR_FUNCTION */

/*s: constant SAVE */
/* To preserve local variables when communicating with the ML code */

#define SAVE \
  env->sp = Val_int(sp), \
  env->state = Val_int(state), \
  env->errflag = Val_int(errflag)
/*e: constant SAVE */

/*s: constant RESTORE */
#define RESTORE \
  sp = Int_val(env->sp), \
  state = Int_val(env->state), \
  errflag = Int_val(env->errflag)
/*e: constant RESTORE */

/*s: function parse_engine */
/* The pushdown automata */

value parse_engine(struct parser_tables *tables, struct parser_env *env, value cmd, value arg) /* ML */
{
  int state;
  mlsize_t sp, asp;
  int errflag;
  int n, n1, n2, m, state1;

  switch(Int_val(cmd)) {

  case START:
    state = 0;
    sp = Int_val(env->sp);
    errflag = 0;

  loop:
    Trace(printf("Loop %d\n", state));
    n = Short(tables->defred, state);
    if (n != 0) goto reduce;
    if (Int_val(env->curr_char) >= 0) goto testshift;
    SAVE;
    return READ_TOKEN;
                                /* The ML code calls the lexer and updates */
                                /* symb_start and symb_end */
  case TOKEN_READ:
    RESTORE;
    if (Is_block(arg)) {
      env->curr_char = Field(tables->transl_block, Tag_val(arg));
      modify(&env->lval, Field(arg, 0));
    } else {
      env->curr_char = Field(tables->transl_const, Int_val(arg));
      modify(&env->lval, Val_long(0));
    }
    Trace(printf("Token %d (0x%lx)\n", Int_val(env->curr_char), env->lval));
    
  testshift:
    n1 = Short(tables->sindex, state);
    n2 = n1 + Int_val(env->curr_char);
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == Int_val(env->curr_char)) goto shift;
    n1 = Short(tables->rindex, state);
    n2 = n1 + Int_val(env->curr_char);
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == Int_val(env->curr_char)) {
      n = Short(tables->table, n2);
      goto reduce;
    }
    if (errflag > 0) goto recover;
    SAVE;
    return CALL_ERROR_FUNCTION;
                                /* The ML code calls the error function */
  case ERROR_DETECTED:
    RESTORE;
  recover:
    if (errflag < 3) {
      errflag = 3;
      while (1) {
        state1 = Int_val(Field(env->s_stack, sp));
        n1 = Short(tables->sindex, state1);
        n2 = n1 + ERRCODE;
        if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
            Short(tables->check, n2) == ERRCODE) {
          Trace(printf("Recovering in state %d\n", state1));
          goto shift_recover;
        } else {
          Trace(printf("Discarding state %d\n", state1));
          if (sp <= Int_val(env->stackbase)) {
            Trace(printf("Fallen off bottom\n"));
            return RAISE_PARSE_ERROR; /* The ML code raises Parse_error */
          }
          sp--;
        }
      }
    } else {
      if (Int_val(env->curr_char) == 0)
        return RAISE_PARSE_ERROR; /* The ML code raises Parse_error */
      Trace(printf("Discarding token %d (0x%lx)\n",
                   Int_val(env->curr_char), env->lval));
      env->curr_char = Val_int(-1);
      goto loop;
    }
    
  shift:
    env->curr_char = Val_int(-1);
    if (errflag > 0) errflag--;
  shift_recover:
    state = Short(tables->table, n2);
    Trace(printf("Shift %d\n", state));
    sp++;
    if (sp < Long_val(env->stacksize)) goto push;
    SAVE;
    return GROW_STACKS_1;
                                 /* The ML code resizes the stacks */
  case STACKS_GROWN_1:
    RESTORE;
  push:
    Field(env->s_stack, sp) = Val_int(state);
    modify(&Field(env->v_stack, sp), env->lval);
    Field(env->symb_start_stack, sp) = env->symb_start;
    Field(env->symb_end_stack, sp) = env->symb_end;
    goto loop;

  reduce:
    Trace(printf("Reduce %d\n", n));
    m = Short(tables->len, n);
    env->asp = Val_int(sp);
    env->rule_number = Val_int(n);
    env->rule_len = Val_int(m);
    sp = sp - m + 1;
    m = Short(tables->lhs, n);
    state1 = Int_val(Field(env->s_stack, sp - 1));
    n1 = Short(tables->gindex, m);
    n2 = n1 + state1;
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == state1) {
      state = Short(tables->table, n2);
    } else {
      state = Short(tables->dgoto, m);
    }
    if (sp < Long_val(env->stacksize)) goto semantic_action;
    SAVE;
    return GROW_STACKS_2;
                                /* The ML code resizes the stacks */
  case STACKS_GROWN_2:
    RESTORE;
  semantic_action:
    SAVE;
    return COMPUTE_SEMANTIC_ACTION;
                                /* The ML code calls the semantic action */
  case SEMANTIC_ACTION_COMPUTED:
    RESTORE;
    Field(env->s_stack, sp) = Val_int(state);
    modify(&Field(env->v_stack, sp), arg);
    asp = Int_val(env->asp);
    Field(env->symb_end_stack, sp) = Field(env->symb_end_stack, asp);
    if (sp > asp) {
      /* This is an epsilon production. Take symb_start equal to symb_end. */
      Field(env->symb_start_stack, sp) = Field(env->symb_end_stack, asp);
    }
    goto loop;

  default:                      /* Should not happen */
    Assert(0);
    return RAISE_PARSE_ERROR;   /* Keeps gcc -Wall happy */
  }
  
}
/*e: function parse_engine */
/*e: byterun/parsing.c */
