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

/* $Id: image.h,v 1.4 1997/09/02 12:54:23 xleroy Exp $ */

struct grimage {
  int width, height;            /* Dimensions of the image */
  Pixmap data;                  /* Pixels */
  Pixmap mask;                  /* Mask for transparent points, or None */
};

#define Width_im(i) (((struct grimage *)Data_custom_val(i))->width)
#define Height_im(i) (((struct grimage *)Data_custom_val(i))->height)
#define Data_im(i) (((struct grimage *)Data_custom_val(i))->data)
#define Mask_im(i) (((struct grimage *)Data_custom_val(i))->mask)

#define Transparent (-1)

value gr_new_image(int w, int h);
