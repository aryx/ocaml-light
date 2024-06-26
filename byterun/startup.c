/*s: byterun/startup.c */
/*s: copyright header C xavier and damien */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C xavier and damien */

/* Start-up code */

#include "config.h"

#ifndef OS_PLAN9
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#else
#endif

#include "alloc.h"
#include "backtrace.h"
#include "debugger.h"
#include "exec.h"
#include "fail.h"
#include "fix_code.h"
#include "gc_ctrl.h"
#include "interp.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "stacks.h"
#include "sys.h"
#include "startup.h"


/*s: global [[atom_table]] */
header_t atom_table[256];
/*e: global [[atom_table]] */

/*s: function [[init_atoms]] */
/* Initialize the atom table */

static void init_atoms(void)
{
  int i;
  for(i = 0; i < 256; i++) atom_table[i] = Make_header(0, i, White);
}
/*e: function [[init_atoms]] */

/*s: function [[read_size]] */
/* Read the trailer of a bytecode file */

//static void fixup_endianness_trailer(uint32 * p)
//{
//#ifndef ARCH_BIG_ENDIAN
//  Reverse_32(p, p);
//#endif
//}


static unsigned long read_size(char * ptr)
{
  unsigned char * p = (unsigned char *) ptr;
  return ((unsigned long) p[0] << 24) + ((unsigned long) p[1] << 16) +
         ((unsigned long) p[2] << 8) + p[3];
}
/*e: function [[read_size]] */

/*s: constant [[FILE_NOT_FOUND]] */
/*e: constant [[FILE_NOT_FOUND]] */
/*s: constant [[TRUNCATED_FILE]] */
/*e: constant [[TRUNCATED_FILE]] */
/*s: constant [[BAD_MAGIC_NUM]] */
/*e: constant [[BAD_MAGIC_NUM]] */

/*s: function [[read_trailer]] */
static int read_trailer(int fd, struct exec_trailer *trail)
{
  char buffer[TRAILER_SIZE];

  lseek(fd, (long) -TRAILER_SIZE, SEEK_END);
  if (read(fd, buffer, TRAILER_SIZE) < TRAILER_SIZE) 
    return BAD_BYTECODE;
  //fixup_endianness_trailer(&trail->num_sections);
  trail->code_size = read_size(buffer);
  trail->prim_size = read_size(buffer + 4);
  trail->data_size = read_size(buffer + 8);
  trail->symbol_size = read_size(buffer + 12);
  trail->debug_size = read_size(buffer + 16);
  if (strncmp(buffer + 20, EXEC_MAGIC, 12) == 0)
    return 0;
  else
    return BAD_BYTECODE; // was BAD_MAGIC_NUM
}
/*e: function [[read_trailer]] */

/*s: function [[attempt_open]] */
int attempt_open(char **name, struct exec_trailer *trail, int do_open_script)
{
  char * truename;
  int fd;
  int err;
  char buf [2];

  truename = searchpath(*name);
  if (truename == 0) 
    truename = *name; 
  else 
    *name = truename;
  fd = open(truename, O_RDONLY);
  if (fd == -1) return FILE_NOT_FOUND;
  if (!do_open_script){
    err = read (fd, buf, 2);
    if ((err < 2) || (buf [0] == '#' && buf [1] == '!')) { 
      close(fd); 
      return BAD_BYTECODE; 
    }
  }
  err = read_trailer(fd, trail);
  if (err != 0) { 
    close(fd); 
    return err; 
  }
  return fd;
}
/*e: function [[attempt_open]] */



/*s: function [[check_primitives]] */
/* Check the primitives used by the bytecode file against the table of
   primitives linked in this interpreter */

static void check_primitives(int fd, int prim_size)
{
  char * prims = stat_alloc(prim_size);
  char * p;
  int idx;

  if (read(fd, prims, prim_size) != prim_size)
    fatal_error("Fatal error: cannot read primitive table\n");
  /* prims contains 0-terminated strings, concatenated. */
  for (p = prims, idx = 0;
       p < prims + prim_size;
       p = p + strlen(p) + 1, idx++) {
    if (names_of_cprim[idx] == NULL ||
        strcmp(p, names_of_cprim[idx]) != 0)
      fatal_error_arg("Fatal error: this bytecode file cannot run on this bytecode interpreter\nMismatch on primitive `%s'\n", p);
  }
  stat_free(prims);
}
/*e: function [[check_primitives]] */

/* Invocation of camlrun: 4 cases.

   1.  runtime + bytecode
       user types:  camlrun [options] bytecode args...
       arguments:  camlrun [options] bytecode args...

   2.  bytecode script
       user types:  bytecode args...
   2a  (kernel 1) arguments:  camlrun ./bytecode args...
   2b  (kernel 2) arguments:  bytecode bytecode args...

   3.  concatenated runtime and bytecode
       user types:  composite args...
       arguments:  composite args...

Algorithm:
  1-  If argument 0 is a valid byte-code file that does not start with #!,
      then we are in case 3 and we pass the same command line to the
      Caml Light program.
  2-  In all other cases, we parse the command line as:
        (whatever) [options] bytecode args...
      and we strip "(whatever) [options]" from the command line.

*/

/*s: global [[verbose_init]] */
/* Configuration parameters and flags */

static unsigned long verbose_init = 0;
/*e: global [[verbose_init]] */
/*s: global [[percent_free_init]] */
static unsigned long percent_free_init = Percent_free_def;
/*e: global [[percent_free_init]] */
/*s: global [[max_percent_free_init]] */
static unsigned long max_percent_free_init = Max_percent_free_def;
/*e: global [[max_percent_free_init]] */
/*s: global [[minor_heap_init]] */
static unsigned long minor_heap_init = Minor_heap_def;
/*e: global [[minor_heap_init]] */
/*s: global [[heap_chunk_init]] */
static unsigned long heap_chunk_init = Heap_chunk_def;
/*e: global [[heap_chunk_init]] */
/*s: global [[heap_size_init]] */
static unsigned long heap_size_init = Init_heap_def;
/*e: global [[heap_size_init]] */
/*s: global [[max_stack_init]] */
static unsigned long max_stack_init = Max_stack_def;
/*e: global [[max_stack_init]] */
extern int trace_flag;

/*s: function [[parse_command_line]] */
/* Parse options on the command line */

static int parse_command_line(char **argv)
{
  int i;

  for(i = 1; argv[i] != NULL && argv[i][0] == '-'; i++) {
    switch(argv[i][1]) {
    case 'b':
      init_backtrace();
      break;
    /*s: [[parse_command_line()]] cases */
    case 'v':
      verbose_init = 1;
      break;
    /*x: [[parse_command_line()]] cases */
    #ifdef DEBUG
        case 't':
          trace_flag = 1;
          break;
    #endif
    /*e: [[parse_command_line()]] cases */
    default:
      fatal_error_arg("Unknown option %s.\n", argv[i]);
    }
  }
  return i;
}
/*e: function [[parse_command_line]] */

/* Parse the CAMLRUNPARAM variable */
/*s: function [[scanmult]] */
/* The option letter for each runtime option is the first letter of the
   last word of the ML name of the option (see [stdlib/gc.mli]).
   Except for l (maximum stack size) and h (initial heap size).
*/

static void scanmult (char *opt, long unsigned int *var)
{
  char mult = ' ';
  sscanf (opt, "=%lu%c", var, &mult);
  if (mult == 'k') *var = *var * 1024;
  if (mult == 'M') *var = *var * (1024 * 1024);
  if (mult == 'G') *var = *var * (1024 * 1024 * 1024);
}
/*e: function [[scanmult]] */

/*s: function [[parse_camlrunparam]] */
static void parse_camlrunparam(void)
{
  char *opt = getenv ("CAMLRUNPARAM");
  if (opt != NULL){
    while (*opt != '\0'){
      switch (*opt++){
      case 's': scanmult (opt, &minor_heap_init); break;
      case 'i': scanmult (opt, &heap_chunk_init); break;
      case 'h': scanmult (opt, &heap_size_init); break;
      case 'l': scanmult (opt, &max_stack_init); break;
      case 'o': scanmult (opt, &percent_free_init); break;
      case 'O': scanmult (opt, &max_percent_free_init); break;
      /*s: [[parse_camlrunparam()]] switch opt cases */
      case 'b': init_backtrace(); break;
      /*x: [[parse_camlrunparam()]] switch opt cases */
      case 'v': scanmult (opt, &verbose_init); break;
      /*e: [[parse_camlrunparam()]] switch opt cases */
      }
    }
  }
}
/*e: function [[parse_camlrunparam]] */

extern void init_ieee_floats (void);

/*s: function [[caml_main]] */
/* Main entry point when loading code from a file */

void caml_main(char **argv)
{
  int fd;
  struct exec_trailer trail;
  int pos;
  struct longjmp_buffer raise_buf;
  struct channel * chan;

  /* Machine-dependent initialization of the floating-point hardware
     so that it behaves as much as possible as specified in IEEE */
  init_ieee_floats();

  /* Set up a catch-all exception handler */
  if (sigsetjmp(raise_buf.buf, 1) == 0) {
    external_raise = &raise_buf;
    /* Determine options and position of bytecode file */
#ifdef DEBUG
    verbose_init = 1;
#endif
    parse_camlrunparam();
    pos = 0;
    fd = attempt_open(&argv[0], &trail, 0);
    if (fd < 0) {

      pos = parse_command_line(argv);
      if (argv[pos] == 0)
        fatal_error("No bytecode file specified.\n");

      fd = attempt_open(&argv[pos], &trail, 1);
      switch(fd) {
      case FILE_NOT_FOUND:
        fatal_error_arg("Fatal error: cannot find file %s\n", argv[pos]);
        break;
      case BAD_BYTECODE:
        fatal_error_arg(
          "Fatal error: the file %s is not a bytecode executable file\n",
          argv[pos]);
        break;
      }
    }
    /* Initialize the abstract machine */
    init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
             percent_free_init, max_percent_free_init, verbose_init);
    init_stack (max_stack_init);
    init_atoms();

    /* Initialize the interpreter */
    interprete(NULL, 0);

    /* Initialize the debugger, if needed */
    debugger_init();

    /* Load the code */
    lseek(fd, - (long) (TRAILER_SIZE + trail.code_size + trail.prim_size
                        + trail.data_size + trail.symbol_size
                        + trail.debug_size), SEEK_END);
    load_code(fd, trail.code_size);

    /* Check the primitives */
    check_primitives(fd, trail.prim_size);

    /* Load the globals */
    chan = open_descriptor(fd);
    global_data = input_val(chan);
    close_channel(chan); // close also fd
    /* Ensure that the globals are in the major heap. */
    oldify(global_data, &global_data);

    /* Record the command-line arguments */
    sys_init(argv + pos);

    /* Execute the program */
    debugger(PROGRAM_START);

    interprete(start_code, trail.code_size);

  } else {
    if (debugger_in_use) {
      extern_sp = &exn_bucket; /* The debugger needs the exception value. */
      debugger(UNCAUGHT_EXC);
    }
    fatal_uncaught_exception(exn_bucket);
  }
}
/*e: function [[caml_main]] */

/*s: function [[caml_startup_code]] */
/* Main entry point when code is linked in as initialized data */

void caml_startup_code(code_t code, asize_t code_size, char *data, char **argv)
{
  struct longjmp_buffer raise_buf;

  init_ieee_floats();
  /*s: [[caml_startup_code()]] ifdef [[DEBUG]], set [[verbose_init]] */
  #ifdef DEBUG
    verbose_init = 1;
  #endif
  /*e: [[caml_startup_code()]] ifdef [[DEBUG]], set [[verbose_init]] */

  parse_camlrunparam();

  /* Set up a catch-all exception handler */
  if (sigsetjmp(raise_buf.buf, 1) == 0) {
    external_raise = &raise_buf;

    /* Initialize the abstract machine */
    init_gc (minor_heap_init, heap_size_init, heap_chunk_init,
             percent_free_init, max_percent_free_init, verbose_init);
    init_stack (max_stack_init);
    init_atoms();

    /* Initialize the interpreter */
    interprete(NULL, 0);

    /* Load the code */
    start_code = code;
#ifdef THREADED_CODE
    thread_code(start_code, code_size);
#endif

    /* Load the globals */
    global_data = input_val_from_string((value)data, 0);
    /* Ensure that the globals are in the major heap. */
    oldify(global_data, &global_data);

    /* Run the code */
    sys_init(argv);
    interprete(start_code, code_size);

  } else {
    fatal_uncaught_exception(exn_bucket);
  }
}
/*e: function [[caml_startup_code]] */
  
/*e: byterun/startup.c */
