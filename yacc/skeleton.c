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

/* Based on public-domain code from Berkeley Yacc */

/* $Id: skeleton.c,v 1.8 1997/09/02 12:55:01 xleroy Exp $ */

#include "defs.h"

char *header[] =
{
  "open Parsing",
  0
};

char *define_tables[] =
{
  "let yytables =",
  "  { Parsing.actions=yyact;",
  "    Parsing.transl_const=yytransl_const;",
  "    Parsing.transl_block=yytransl_block;",
  "    Parsing.lhs=yylhs;",
  "    Parsing.len=yylen;",
  "    Parsing.defred=yydefred;",
  "    Parsing.dgoto=yydgoto;",
  "    Parsing.sindex=yysindex;",
  "    Parsing.rindex=yyrindex;",
  "    Parsing.gindex=yygindex;",
  "    Parsing.tablesize=yytablesize;",
  "    Parsing.table=yytable;",
  "    Parsing.check=yycheck;",
  "    Parsing.error_function=parse_error }",
  0
};

void write_section(char **section)
{
    register int i;
    register FILE *fp;

    fp = code_file;
    for (i = 0; section[i]; ++i)
    {
	++outline;
	fprintf(fp, "%s\n", section[i]);
    }
}
