/*s: byterun/alloc.h */
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

#ifndef _alloc_
#define _alloc_


#include "misc.h"
#include "mlvalues.h"

value alloc (mlsize_t, tag_t);
value alloc_small (mlsize_t, tag_t);
value alloc_tuple (mlsize_t);
value alloc_string (mlsize_t);
value copy_string (char *);
value copy_string_array (char **);
value copy_double (double);
value alloc_array (value (*funct) (char *), char ** array);

value alloc_custom(struct custom_operations * ops,
                   unsigned long size, /*size in bytes*/
                   mlsize_t mem, /*resources consumed*/
                   mlsize_t max  /*max resources*/);

typedef void (*final_fun)(value);
value alloc_final (mlsize_t /*size in words*/,
                   final_fun, /*finalization function*/
                   mlsize_t, /*resources consumed*/
                   mlsize_t  /*max resources*/);

int convert_flag_list (value, int *);

#endif /* _alloc_ */
/*e: byterun/alloc.h */
